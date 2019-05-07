(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

include Internal_event.Legacy_logging.Make_semantic(struct
    let name = Proto_alpha.Name.name ^ ".client.denunciation"
  end)

open Proto_alpha
open Alpha_context

open Client_baking_blocks
open Logging

module HLevel = Hashtbl.Make(struct
    type t = Chain_id.t * Raw_level.t
    let equal (c, l) (c', l') =
      Chain_id.equal c c' && Raw_level.equal l l'
    let hash (c, lvl) = Hashtbl.hash (c, lvl)
  end)

module Delegate_Map = Map.Make(Signature.Public_key_hash)

type state = {
  (* Endorsements seen so far *)
  endorsements_table : Kind.endorsement operation Delegate_Map.t HLevel.t ;
  (* Blocks received so far *)
  blocks_table : Block_hash.t Delegate_Map.t HLevel.t ;
  (* Maximum delta of level to register *)
  preserved_levels : int ;
  (* Highest level seen in a block *)
  mutable highest_level_encountered : Raw_level.t ;
}

let create_state ~preserved_levels =
  Lwt.return { endorsements_table = HLevel.create preserved_levels ;
               blocks_table = HLevel.create preserved_levels ;
               preserved_levels ;
               highest_level_encountered = Raw_level.root (* 0l *) }

(* We choose a previous offset (5 blocks from head) to ensure that the
   injected operation is branched from a valid predecessor. *)
let get_block_offset level =
  match Alpha_environment.wrap_error (Raw_level.of_int32 5l) with
  | Ok min_level ->
      Lwt.return
        (if Raw_level.(level < min_level) then
           `Head 0
         else
           `Head 5)
  | Error errs ->
      lwt_log_error Tag.DSL.(fun f ->
          f "Invalid level conversion : %a"
          -% t event "invalid_level_conversion"
          -% a errs_tag errs) >>= fun () ->
      Lwt.return (`Head 0)

let process_endorsements (cctxt : #Proto_alpha.full) state
    (endorsements : Alpha_block_services.operation list) level =
  iter_s (fun { Alpha_block_services.shell ; chain_id ; receipt ; hash ; protocol_data ; _ } ->
      let chain = `Hash chain_id in
      match protocol_data, receipt with
      | (Operation_data ({ contents = Single (Endorsement _) ; _ } as protocol_data)),
        Apply_results.(
          Operation_metadata { contents = Single_result (Endorsement_result { delegate ; _ }) }) ->
          let new_endorsement : Kind.endorsement Alpha_context.operation = { shell ; protocol_data } in
          let map = match HLevel.find_opt state.endorsements_table (chain_id, level) with
            | None -> Delegate_Map.empty
            | Some x -> x in
          (* If a previous endorsement made by this pkh is found for
             the same level we inject a double_endorsement *)
          begin match Delegate_Map.find_opt delegate map with
            | None -> return @@ HLevel.add state.endorsements_table (chain_id, level)
                  (Delegate_Map.add delegate new_endorsement map)
            | Some existing_endorsement when
                Block_hash.(existing_endorsement.shell.branch <> new_endorsement.shell.branch) ->
                get_block_offset level >>= fun block ->
                Alpha_block_services.hash cctxt ~chain ~block () >>=? fun block_hash ->
                Alpha_services.Forge.double_endorsement_evidence
                  cctxt (`Hash chain_id, block) ~branch:block_hash
                  ~op1:existing_endorsement
                  ~op2:new_endorsement () >>=? fun bytes ->
                let bytes = Signature.concat bytes Signature.zero in
                lwt_log_notice Tag.DSL.(fun f ->
                    f "Double endorsement detected"
                    -% t event "double_endorsement_detected"
                    -% t conflicting_endorsements_tag (existing_endorsement, new_endorsement)) >>= fun () ->
                (* A denunciation may have already occured *)
                Shell_services.Injection.operation cctxt ~chain bytes >>=? fun op_hash ->
                lwt_log_notice Tag.DSL.(fun f ->
                    f "Double endorsement evidence injected %a"
                    -% t event "double_endorsement_denounced"
                    -% t signed_operation_tag bytes
                    -% a Operation_hash.Logging.tag op_hash) >>= fun () ->
                return @@ HLevel.replace state.endorsements_table (chain_id, level)
                  (Delegate_Map.add delegate new_endorsement map)
            | Some _ ->
                (* This endorsement is already present in another
                   block but endorse the same predecessor *)
                return_unit
          end
      | _ ->
          lwt_log_error Tag.DSL.(fun f ->
              f "Inconsistent endorsement found %a"
              -% t event "inconsistent_endorsement"
              -% a Operation_hash.Logging.tag hash) >>= fun () ->
          return_unit
    ) endorsements >>=? fun () ->
  return_unit

let process_block (cctxt : #Proto_alpha.full) state (header : Alpha_block_services.block_info) =
  let { Alpha_block_services.chain_id ; hash ;
        metadata = { protocol_data = { baker ; level = { level ; _ } ; _ } ; _ } ; _ } = header in
  let chain = `Hash chain_id in
  let map = match HLevel.find_opt state.blocks_table (chain_id, level) with
    | None -> Delegate_Map.empty
    | Some x -> x in
  begin match Delegate_Map.find_opt baker map with
    | None -> return @@ HLevel.add state.blocks_table (chain_id, level)
          (Delegate_Map.add baker hash map)
    | Some existing_hash when Block_hash.(=) existing_hash hash ->
        (* This case should never happen *)
        lwt_debug Tag.DSL.(fun f -> f "Double baking detected but block hashes are equivalent. Skipping..." -% t event "double_baking_but_not") >>= fun () ->
        return @@ HLevel.replace state.blocks_table (chain_id, level)
          (Delegate_Map.add baker hash map)
    | Some existing_hash ->
        (* If a previous endorsement made by this pkh is found for
           the same level we inject a double_endorsement *)
        Alpha_block_services.header cctxt ~chain ~block:(`Hash (existing_hash, 0)) () >>=?
        fun ( { shell ; protocol_data ; _ } : Alpha_block_services.block_header) ->
        let bh1 = { Alpha_context.Block_header.shell = shell ; protocol_data = protocol_data } in
        Alpha_block_services.header cctxt ~chain ~block:(`Hash (hash, 0)) () >>=?
        fun ( { shell ; protocol_data ; _ } : Alpha_block_services.block_header) ->
        let bh2 = { Alpha_context.Block_header.shell = shell ; protocol_data = protocol_data } in
        (* If the blocks are on different chains then skip it *)
        get_block_offset level >>= fun block ->
        Alpha_block_services.hash cctxt ~chain ~block () >>=? fun block_hash ->
        Alpha_services.Forge.double_baking_evidence
          cctxt
          (chain, block)
          ~branch:block_hash
          ~bh1 ~bh2
          () >>=? fun bytes ->
        let bytes = Signature.concat bytes Signature.zero in
        lwt_log_notice Tag.DSL.(fun f ->
            f "Double baking detected"
            -% t event "double_baking_detected") >>= fun () ->
        (* A denunciation may have already occured *)
        Shell_services.Injection.operation cctxt ~chain bytes >>=? fun op_hash ->
        lwt_log_notice Tag.DSL.(fun f ->
            f "Double baking evidence injected %a"
            -% t event "double_baking_denounced"
            -% t signed_operation_tag bytes
            -% a Operation_hash.Logging.tag op_hash) >>= fun () ->
        return @@ HLevel.replace state.blocks_table (chain_id, level)
          (Delegate_Map.add baker hash map)
  end

(* Remove levels that are lower than the [highest_level_encountered] minus [preserved_levels] *)
let cleanup_old_operations state =
  let highest_level_encountered =
    Int32.to_int (Raw_level.to_int32 state.highest_level_encountered)
  in
  let diff = highest_level_encountered  - state.preserved_levels in
  let threshold = begin if diff < 0 then
      Raw_level.root
    else
      Raw_level.of_int32 (Int32.of_int diff) |> function
      | Ok threshold -> threshold
      | Error _ -> Raw_level.root
  end in
  let filter hmap =
    HLevel.filter_map_inplace (fun (_, level) x ->
        if Raw_level.(level < threshold) then
          None
        else
          Some x
      ) hmap in
  filter state.endorsements_table ; filter state.blocks_table ;
  ()

let endorsements_index = 0

(* Each new block is processed :
   - Checking that every endorser operated only once at this level
   - Checking that every baker injected only once at this level
*)
let process_new_block (cctxt : #Proto_alpha.full) state { hash ; chain_id ; level ; protocol ; next_protocol ; _ } =
  if Protocol_hash.(protocol <> next_protocol) then
    lwt_log_error Tag.DSL.(fun f ->
        f "Protocol changing detected. Skipping the block."
        -% t event "protocol_change_detected"
        (* TODO which protocols -- in tag *)
      ) >>= fun () ->
    return_unit
  else
    lwt_debug Tag.DSL.(fun f ->
        f "Block level : %a"
        -% t event "accuser_saw_block"
        -% a level_tag level
        -% t Block_hash.Logging.tag hash) >>= fun () ->
    let chain = `Hash chain_id in
    let block = `Hash (hash, 0) in
    state.highest_level_encountered <- Raw_level.max level state.highest_level_encountered ;
    (* Processing blocks *)
    begin
      Alpha_block_services.info cctxt ~chain ~block () >>= function
      | Ok block_info ->
          process_block cctxt state block_info
      | Error errs ->
          lwt_log_error Tag.DSL.(fun f ->
              f "Error while fetching operations in block %a@\n%a"
              -% t event "fetch_operations_error"
              -% a Block_hash.Logging.tag hash
              -% a errs_tag errs) >>= fun () ->
          return_unit
    end >>=? fun () ->
    (* Processing endorsements *)
    begin Alpha_block_services.Operations.operations cctxt ~chain ~block () >>= function
      | Ok operations ->
          if List.length operations > endorsements_index then
            let endorsements = List.nth operations endorsements_index in
            process_endorsements cctxt state endorsements level
          else return_unit
      | Error errs ->
          lwt_log_error Tag.DSL.(fun f ->
              f "Error while fetching operations in block %a@\n%a"
              -% t event "fetch_operations_error"
              -% a Block_hash.Logging.tag hash
              -% a errs_tag errs) >>= fun () ->
          return_unit
    end >>=? fun () ->
    cleanup_old_operations state ;
    return_unit

let create (cctxt : #Proto_alpha.full) ~preserved_levels valid_blocks_stream =

  let process_block cctxt state bi =
    process_new_block cctxt state bi >>= function
    | Ok () ->
        lwt_log_notice Tag.DSL.(fun f ->
            f "Block %a registered"
            -% t event "accuser_processed_block"
            -% a Block_hash.Logging.tag bi.Client_baking_blocks.hash)
        >>= return
    | Error errs ->
        lwt_log_error Tag.DSL.(fun f ->
            f "Error while processing block %a@\n%a"
            -% t event "accuser_block_error"
            -% a Block_hash.Logging.tag bi.hash
            -% a errs_tag errs)
        >>= return
  in

  let state_maker _ =
    create_state ~preserved_levels >>= return
  in

  Client_baking_scheduling.main
    ~name:"accuser"
    ~cctxt
    ~stream:valid_blocks_stream
    ~state_maker
    ~pre_loop:(fun _ _ _ -> return_unit)
    ~compute_timeout:(fun _ -> Lwt_utils.never_ending ())
    ~timeout_k:(fun _ _ () -> return_unit)
    ~event_k:process_block
