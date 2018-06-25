(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

include Logging.Make(struct let name = "client.denunciation" end)

open Proto_alpha
open Alpha_context

open Client_baking_blocks

module HLevel = Hashtbl.Make(struct
    include Raw_level
    let hash lvl = Int32.to_int (to_int32 lvl)
  end)

module Delegate_Map = Map.Make(Signature.Public_key_hash)

type state = {
  (* Endorsements seen so far *)
  endorsements_table : Kind.endorsement operation Delegate_Map.t HLevel.t ;
  (* Blocks received so far *)
  blocks_table : Block_hash.t Delegate_Map.t HLevel.t ;
  (* Maximum delta of level to register *)
  preserved_levels : Raw_level.t ;
  (* Highest level seen in a block *)
  mutable highest_level_encountered : Raw_level.t ;
}

let create_state ~preserved_levels =
  Alpha_environment.wrap_error @@ Raw_level.of_int32 (Int32.of_int preserved_levels)
  |> function
  | Error errs ->
      lwt_log_error "Bad preserved_levels conversion : %a" pp_print_error errs >>=
      exit 2
  | Ok raw_level_preserved_levels ->
      Lwt.return { endorsements_table = HLevel.create preserved_levels ;
                   blocks_table = HLevel.create preserved_levels ;
                   preserved_levels = raw_level_preserved_levels ;
                   highest_level_encountered = Raw_level.root (* 0l *) }

(* get the delegate that had the right to bake for a specific level/slot *)
let fetch_baker (cctxt : #Proto_alpha.full) ~chain ~block =
  Alpha_block_services.metadata cctxt ~chain ~block () >>=? fun
    { protocol_data = { Alpha_context.Block_header.baker  } } ->
  return baker

let get_block_offset level =
  Alpha_environment.wrap_error @@
  Raw_level.of_int32 6l |> function
  | Ok min_level ->
      begin if Raw_level.(level <= min_level) then
          Lwt.return (`Head 0)
        else
          Lwt.return (`Head 5) end
  | Error errs ->
      lwt_log_error "Invalid level conversion : %a" pp_print_error errs >>= fun () ->
      Lwt.return (`Head 0)

let process_endorsements (cctxt : #Proto_alpha.full) state ~chain
    (endorsements : Alpha_block_services.operation list) level =
  iter_s (fun { Alpha_block_services.shell ; receipt ; hash ; protocol_data ; _ } ->
      match protocol_data, receipt with
      | (Operation_data ({ contents = Single (Endorsement _) ; _ } as protocol_data)),
        Apply_operation_result.(
          Operation_metadata { contents = Single_result (Endorsement_result { delegate ; _ }) }) ->
          let new_endorsement : Kind.endorsement Alpha_context.operation = { shell ; protocol_data } in
          let map = match HLevel.find_opt state.endorsements_table level with
            | None -> Delegate_Map.empty
            | Some x -> x in
          (* If a previous endorsement made by this pkh is found for
             the same level we inject a double_endorsement *)
          begin match Delegate_Map.find_opt delegate map with
            | None -> return @@ HLevel.add state.endorsements_table level
                  (Delegate_Map.add delegate new_endorsement map)
            | Some existing_endorsement ->
                get_block_offset level >>= fun block ->
                (* TODO : verify that the chains are coherent *)
                Alpha_block_services.hash cctxt ~chain:`Main ~block () >>=? fun block_hash ->
                Alpha_services.Forge.double_endorsement_evidence
                  cctxt (`Main, block) ~branch:block_hash
                  ~op1:existing_endorsement
                  ~op2:new_endorsement () >>=? fun bytes ->
                let bytes = Signature.concat bytes Signature.zero in
                lwt_log_notice "Double endorsement detected" >>= fun () ->
                (* A denunciation may have already occured *)
                Shell_services.Injection.operation cctxt ~chain bytes >>=? fun op_hash ->
                lwt_log_notice "Double endorsement evidence injected %a"
                  Operation_hash.pp op_hash >>= fun () ->
                return @@ HLevel.replace state.endorsements_table level
                  (Delegate_Map.add delegate new_endorsement map)
          end
      | _ ->
          lwt_log_error "Inconsistent endorsement found %a"
            Operation_hash.pp hash >>= fun () ->
          return ()
    ) endorsements >>=? fun () ->
  return ()

let process_block (cctxt : #Proto_alpha.full) state ~chain (header : Alpha_block_services.block_info) =
  let { Alpha_block_services.hash ; metadata = { protocol_data = { baker ; level = { level } } } } = header in
  let map = match HLevel.find_opt state.blocks_table level with
    | None -> Delegate_Map.empty
    | Some x -> x in
  begin match Delegate_Map.find_opt baker map with
    | None -> return @@ HLevel.add state.blocks_table level
          (Delegate_Map.add baker hash map)
    | Some existing_hash when Block_hash.(=) existing_hash hash ->
        (* This case should never happen *)
        lwt_debug "Double baking detected but block hashes are equivalent. Skipping..." >>= fun () ->
        return @@ HLevel.replace state.blocks_table level
          (Delegate_Map.add baker hash map)
    | Some existing_hash ->
        (* If a previous endorsement made by this pkh is found for
           the same level we inject a double_endorsement *)
        (* TODO : verify that the chains are coherent *)
        Alpha_block_services.header cctxt ~chain ~block:(`Hash (existing_hash, 0)) () >>=?
        fun ( { shell ; protocol_data } : Alpha_block_services.block_header) ->
        let bh1 = { Alpha_context.Block_header.shell = shell ; protocol_data = protocol_data } in
        Alpha_block_services.header cctxt ~chain ~block:(`Hash (hash, 0)) () >>=?
        fun ( { shell ; protocol_data } : Alpha_block_services.block_header) ->
        let bh2 = { Alpha_context.Block_header.shell = shell ; protocol_data = protocol_data } in
        get_block_offset level >>= fun block ->
        Alpha_block_services.hash cctxt ~chain:`Main ~block () >>=? fun block_hash ->
        Alpha_services.Forge.double_baking_evidence cctxt (`Main, block) ~branch:block_hash
          ~bh1 ~bh2 () >>=? fun bytes ->
        let bytes = Signature.concat bytes Signature.zero in
        lwt_log_notice "Double baking detected" >>= fun () ->
        (* A denunciation may have already occured *)
        Shell_services.Injection.operation cctxt ~chain bytes >>=? fun op_hash ->
        lwt_log_notice "Double baking evidence injected %a"
          Operation_hash.pp op_hash >>= fun () ->
        return @@ HLevel.replace state.blocks_table level
          (Delegate_Map.add baker hash map)
  end

(* Remove levels that are lower than the [highest_level_encountered] minus [preserved_levels] *)
let cleanup_old_operations state =
  let diff = Raw_level.diff state.highest_level_encountered state.preserved_levels in
  Alpha_environment.wrap_error @@ begin if Int32.compare diff Int32.zero < 0 then
      Alpha_environment.Error_monad.ok Raw_level.root
    else
      Raw_level.of_int32 diff
  end |> function
  | Error errs ->
      lwt_log_error "Bad conversion : %a" pp_print_error errs >>=
      Lwt.return
  | Ok threshold ->
      let filter hmap =
        HLevel.filter_map_inplace (fun level x ->
            if Raw_level.(level < threshold) then
              None
            else
              Some x
          ) hmap in
      filter state.endorsements_table ; filter state.blocks_table ;
      Lwt.return ()

let endorsements_index = 0

(* Each new block is processed :
   - Checking that every endorser operated only once at this level
   - Checking that every baker injected only once at this level
*)
let process_new_block (cctxt : #Proto_alpha.full) state { hash ; chain_id ; level ; protocol ; next_protocol } =
  if Protocol_hash.(protocol <> next_protocol) then
    lwt_log_error "Protocol changing detected. Skipping the block." >>= fun () ->
    return ()
  else 
    lwt_debug "Block level : %a" Raw_level.pp level >>= fun () ->
    let chain = `Hash chain_id in
    let block = `Hash (hash, 0) in
    state.highest_level_encountered <- Raw_level.max level state.highest_level_encountered ;
    (* Processing blocks *)
    begin
      Alpha_block_services.info cctxt ~chain ~block () >>= function
      | Ok block_info ->
          process_block cctxt state ~chain block_info
      | Error errs ->
          lwt_log_error "Error while fetching operations in block %a@\n%a"
            Block_hash.pp_short hash
            pp_print_error errs >>= fun () ->
          return ()
    end >>=? fun () ->
    (* Processing endorsements *)
    begin Alpha_block_services.Operations.operations cctxt ~chain ~block () >>= function
      | Ok operations ->
          if List.length operations > endorsements_index then
            let endorsements = List.nth operations endorsements_index in
            process_endorsements cctxt state ~chain endorsements level
          else return ()
      | Error errs ->
          lwt_log_error "Error while fetching operations in block %a@\n%a"
            Block_hash.pp_short hash
            pp_print_error errs >>= fun () ->
          return ()
    end >>=? fun () ->
    cleanup_old_operations state >>= fun () ->
    return ()

let create (cctxt : #Proto_alpha.full) ~preserved_levels valid_blocks_stream =

  let process_block cctxt state bi =
    process_new_block cctxt state bi >>= function
    | Ok () ->
        lwt_log_notice
          "Block %a registered"
          Block_hash.pp_short bi.Client_baking_blocks.hash
        >>= return
    | Error errs ->
        lwt_log_error "Error while processing block %a@\n%a"
          Block_hash.pp_short bi.hash
          pp_print_error errs
        >>= return
  in

  let state_maker _ _ =
    create_state ~preserved_levels >>= return
  in

  Client_baking_scheduling.main
    ~name:"accuser"
    ~cctxt
    ~stream:valid_blocks_stream
    ~state_maker
    ~pre_loop:(fun _ _ _ -> return ())
    ~compute_timeout:(fun _ -> Lwt_utils.never_ending ())
    ~timeout_k:(fun _ _ () -> return ())
    ~event_k:process_block
