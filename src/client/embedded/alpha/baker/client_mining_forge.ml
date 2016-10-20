(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)


open Client_commands
open Logging.Client.Mining
module Ed25519 = Environment.Ed25519

let generate_proof_of_work_nonce () =
  Sodium.Random.Bigbytes.generate Constants.proof_of_work_nonce_size

let generate_seed_nonce () =
  match Nonce.of_bytes @@
    Sodium.Random.Bigbytes.generate Constants.nonce_length with
  | Error _ -> assert false
  | Ok nonce -> nonce

let rec compute_stamp
    cctxt block delegate_sk shell mining_slot seed_nonce_hash =
  Client_proto_rpcs.Constants.stamp_threshold
    cctxt block >>=? fun stamp_threshold ->
  let rec loop () =
    let proof_of_work_nonce = generate_proof_of_work_nonce () in
    let unsigned_header =
      Tezos_context.Block.forge_header
        shell { mining_slot ; seed_nonce_hash ; proof_of_work_nonce } in
    let signed_header =
      Ed25519.Signature.append delegate_sk unsigned_header in
    let block_hash = Block_hash.hash_bytes [signed_header] in
    if Mining.check_hash block_hash stamp_threshold then
      proof_of_work_nonce
    else
      loop () in
  return (loop ())

let inject_block cctxt block
    ?force
    ~priority ~timestamp ~fitness ~seed_nonce
    ~src_sk operation_list =
  let block = match block with `Prevalidation -> `Head 0 | block -> block in
  Client_node_rpcs.Blocks.info cctxt block >>=? fun bi ->
  let seed_nonce_hash = Nonce.hash seed_nonce in
  Client_proto_rpcs.Context.next_level cctxt block >>=? fun level ->
  let operations =
    Operation_list_list_hash.compute
      (List.map Operation_list_hash.compute operation_list) in
  let shell =
    { Store.Block_header.net_id = bi.net ; predecessor = bi.hash ;
      timestamp ; fitness ; operations } in
  let slot =
    { Block.level = level.level ; priority = Int32.of_int priority } in
  compute_stamp cctxt block
    src_sk shell slot seed_nonce_hash >>=? fun proof_of_work_nonce ->
  Client_proto_rpcs.Helpers.Forge.block cctxt
    block
    ~net:bi.net
    ~predecessor:bi.hash
    ~timestamp
    ~fitness
    ~operations
    ~level:level.level
    ~priority:priority
    ~seed_nonce_hash
    ~proof_of_work_nonce
    () >>=? fun unsigned_header ->
  let signed_header = Ed25519.Signature.append src_sk unsigned_header in
  Client_node_rpcs.inject_block cctxt
    ?force signed_header operation_list >>=? fun block_hash ->
  return block_hash

let forge_block cctxt block
    ?force
    ?operations ?(best_effort = operations = None) ?(sort = best_effort)
    ?timestamp
    ~priority
    ~seed_nonce ~src_sk () =
  let block =
    match block with
    | `Prevalidation -> `Head 0
    | `Test_prevalidation -> `Test_head 0
    | block -> block in
  Client_proto_rpcs.Context.level cctxt block >>=? fun level ->
  let level = Raw_level.succ level.level in
  begin
    match operations with
    | None ->
        Client_node_rpcs.Blocks.pending_operations
          cctxt block >>=? fun (ops, pendings) ->
        return (Operation_hash.Set.elements @@
                Operation_hash.Set.union
                  (Prevalidation.preapply_result_operations ops)
                  pendings)
    | Some operations -> return operations
  end >>=? fun operations ->
  begin
    match priority with
    | `Set prio -> begin
        Client_proto_rpcs.Helpers.minimal_time
          cctxt block ~prio () >>=? fun time ->
        return (prio, time)
      end
    | `Auto (src_pkh, max_priority) ->
        Client_proto_rpcs.Helpers.Rights.mining_rights_for_delegate cctxt
          ?max_priority
          ~first_level:level
          ~last_level:level
          block src_pkh () >>=? fun possibilities ->
        try
          let _, prio, time =
            List.find (fun (l,_,_) -> l = level) possibilities in
          return (prio, time)
        with Not_found ->
          Error_monad.failwith "No slot found at level %a" Raw_level.pp level
  end >>=? fun (priority, minimal_timestamp) ->
  lwt_log_info "Mining block at level %a prio %d"
    Raw_level.pp level priority >>= fun () ->
  begin
    match timestamp, minimal_timestamp with
    | None, timestamp -> return timestamp
    | Some timestamp, minimal_timestamp ->
        if timestamp < minimal_timestamp then
          Error_monad.failwith
            "Proposed timestamp %a is earlier than minimal timestamp %a"
            Time.pp_hum timestamp
            Time.pp_hum minimal_timestamp
        else
          return timestamp
  end >>=? fun timestamp ->
  let request = List.length operations in
  Client_node_rpcs.Blocks.preapply
    cctxt block ~timestamp ~sort operations >>=?
  fun { operations ; fitness ; timestamp } ->
  let valid = List.length operations.applied in
  lwt_log_info "Found %d valid operations (%d refused) for timestamp %a"
    valid (request - valid)
    Time.pp_hum timestamp >>= fun () ->
  lwt_log_info "Computed fitness %a" Fitness.pp fitness >>= fun () ->
  if best_effort
     || ( Operation_hash.Map.is_empty operations.refused
          && Operation_hash.Map.is_empty operations.branch_refused
          && Operation_hash.Map.is_empty operations.branch_delayed ) then
    inject_block cctxt ?force ~src_sk
      ~priority ~timestamp ~fitness ~seed_nonce block
      [operations.applied]
  else
    failwith "Cannot (fully) validate the given operations."


(** Worker *)

module State : sig

  val get_block:
    Client_commands.context ->
    Raw_level.t -> Block_hash.t list tzresult Lwt.t

  val record_block:
    Client_commands.context ->
    Raw_level.t -> Block_hash.t -> Nonce.t -> unit tzresult Lwt.t

end = struct

  module LevelMap = Map.Make(Raw_level)

  type t = Block_hash.t list LevelMap.t
  let encoding : t Data_encoding.t =
    let open Data_encoding in
    conv
      (fun x -> LevelMap.bindings x)
      (fun l ->
         List.fold_left
           (fun x (y, z) -> LevelMap.add y z x)
           LevelMap.empty l)
      (list (obj2
               (req "level" Raw_level.encoding)
               (req "blocks" (list Block_hash.encoding))))

  let filename cctxt =
    Client_commands.(Filename.concat cctxt.config.base_dir "blocks")

  let load cctxt =
    let filename = filename cctxt in
    if not (Sys.file_exists filename) then return LevelMap.empty else
      Data_encoding_ezjsonm.read_file filename >>= function
      | Error _ ->
          failwith "couldn't to read the block file"
      | Ok json ->
          match Data_encoding.Json.destruct encoding json with
          | exception _ -> (* TODO print_error *)
              failwith "didn't understand the block file"
          | map ->
              return map

  let save cctxt map =
    Lwt.catch
      (fun () ->
         let dirname = Client_commands.(cctxt.config.base_dir) in
         (if not (Sys.file_exists dirname) then Lwt_utils.create_dir dirname
          else Lwt.return ()) >>= fun () ->
         let filename = filename cctxt in
         let json = Data_encoding.Json.construct encoding map in
         Data_encoding_ezjsonm.write_file filename json >>= function
         | Error _ -> failwith "Json.write_file"
         | Ok () -> return ())
      (fun exn ->
         Error_monad.failwith
           "could not write the block file: %s."
           (Printexc.to_string exn))

  let lock = Lwt_mutex.create ()

  let get_block cctxt level =
    Lwt_mutex.with_lock lock
      (fun () ->
         load cctxt >>=? fun map ->
         try
           let blocks = LevelMap.find level map in
           return blocks
         with Not_found -> return [])

  let record_block cctxt level hash nonce =
    Lwt_mutex.with_lock lock
      (fun () ->
         load cctxt >>=? fun map ->
         let previous =
           try LevelMap.find level map
           with Not_found -> [] in
         save cctxt
           (LevelMap.add level (hash :: previous) map)) >>=? fun () ->
    Client_proto_nonces.add cctxt hash nonce

end

let get_mining_slot cctxt
    ?max_priority (bi: Client_mining_blocks.block_info) delegates =
  let block = `Hash bi.hash in
  let level = Raw_level.succ bi.level.level in
  Lwt_list.filter_map_p
    (fun delegate ->
       Client_proto_rpcs.Helpers.Rights.mining_rights_for_delegate cctxt
         ?max_priority
         ~first_level:level
         ~last_level:level
         block delegate () >>= function
       | Error errs ->
           log_error "Error while fetching mining possibilities:\n%a"
             pp_print_error errs ;
           Lwt.return_none
       | Ok slots ->
           let convert = fun (_lvl, slot, timestamp) ->
             (timestamp, (bi, slot, delegate)) in
           Lwt.return (Some (List.map convert slots)))
    delegates >>= fun slots ->
  let sorted_slots =
    List.sort
      (fun (t1,_) (t2,_) -> Time.compare t1 t2)
      (List.flatten slots) in
  match sorted_slots with
  | [] -> Lwt.return None
  | slot :: _ -> Lwt.return (Some slot)

let rec insert_mining_slot slot = function
  | [] -> [slot]
  | ((timestamp,_) :: _) as slots when Time.(fst slot < timestamp) ->
      slot :: slots
  | slot' :: slots -> slot' :: insert_mining_slot slot slots

type state = {
  genesis: Block_hash.t ;
  delegates: public_key_hash list ;
  mutable best: Client_mining_blocks.block_info ;
  mutable future_slots:
    (Time.t * (Client_mining_blocks.block_info * int * public_key_hash)) list ;
}

let create_state genesis delegates best =
  { genesis ;
    delegates ;
    best ;
    future_slots = [] ;
  }

let drop_old_slots ~before state =
  state.future_slots <-
    List.filter
      (fun (t, _slot) -> Time.compare before t <= 0)
      state.future_slots

let compute_timeout { future_slots } =
  match future_slots with
  | [] ->
      Lwt_utils.never_ending
  | (timestamp, _) :: _ ->
      let now = Time.now () in
      let delay = Time.diff timestamp now in
      if delay <= 0L then
        if delay <= -1800L then
          Lwt_unix.sleep 10.
        else
          Lwt.return_unit
      else
        Lwt_unix.sleep (Int64.to_float delay)

let get_unrevealed_nonces cctxt ?(force = false) block =
  Client_proto_rpcs.Context.next_level cctxt.rpc_config block >>=? fun level ->
  let cur_cycle = level.cycle in
  match Cycle.pred cur_cycle with
  | None -> return []
  | Some cycle ->
      Client_mining_blocks.blocks_from_cycle
        cctxt.rpc_config block cycle >>=? fun blocks ->
      map_filter_s (fun hash ->
          Client_proto_nonces.find cctxt hash >>= function
          | None -> return None
          | Some nonce ->
              Client_proto_rpcs.Context.level
                cctxt.rpc_config (`Hash hash) >>=? fun level ->
              if force then
                return (Some (hash, (level.level, nonce)))
              else
                Client_proto_rpcs.Context.Nonce.get
                  cctxt.rpc_config block level.level >>=? function
                | Missing nonce_hash
                  when Nonce.check_hash nonce nonce_hash ->
                    cctxt.warning "Found nonce for %a (level: %a)@."
                      Block_hash.pp_short hash
                      Level.pp level >>= fun () ->
                    return (Some (hash, (level.level, nonce)))
                | Missing _nonce_hash ->
                    cctxt.error "Incoherent nonce for level %a"
                      Raw_level.pp level.level >>= fun () ->
                    return None
                | Forgotten -> return None
                | Revealed _ -> return None)
        blocks

let safe_get_unrevealed_nonces cctxt block =
  get_unrevealed_nonces cctxt block >>= function
  | Ok r -> Lwt.return r
  | Error err ->
      lwt_warn "Cannot read nonces: %a@." pp_print_error err >>= fun () ->
      Lwt.return []


let get_delegates cctxt state =
  match state.delegates with
  | [] ->
      Client_keys.get_keys cctxt >>=? fun keys ->
      return (List.map (fun (_,pkh,_,_) -> pkh) keys)
  | _ :: _ as delegates -> return delegates

let insert_block
    cctxt ?max_priority state (bi: Client_mining_blocks.block_info) =
  begin
    safe_get_unrevealed_nonces cctxt (`Hash bi.hash) >>= fun nonces ->
    Client_mining_revelation.forge_seed_nonce_revelation
      cctxt ~force:true (`Hash bi.hash) (List.map snd nonces)
  end >>= fun _ignore_error ->
  if Fitness.compare state.best.fitness bi.fitness < 0 then begin
    state.best <- bi ;
    drop_old_slots
      ~before:(Time.add state.best.timestamp (-1800L)) state ;
  end ;
  get_delegates cctxt state >>=? fun delegates ->
  get_mining_slot cctxt.rpc_config ?max_priority bi delegates >>= function
  | None ->
      lwt_debug
        "Can't compute slot for %a" Block_hash.pp_short bi.hash >>= fun () ->
      return ()
  | Some ((timestamp, (_,_,delegate)) as slot) ->
      Client_keys.Public_key_hash.name cctxt delegate >>=? fun name ->
      lwt_log_info "New mining slot at %a for %s after %a"
        Time.pp_hum timestamp
        name
        Block_hash.pp_short bi.hash >>= fun () ->
      state.future_slots <- insert_mining_slot slot state.future_slots ;
      return ()

let pop_mining_slots state =
  let now = Time.now () in
  let rec pop acc = function
    | [] -> List.rev acc, []
    | ((timestamp,_) :: _) as slots when Time.compare now timestamp < 0 ->
        List.rev acc, slots
    | slot :: slots -> pop (slot :: acc) slots in
  let slots, future_slots = pop [] state.future_slots in
  state.future_slots <- future_slots ;
  slots

let insert_blocks cctxt ?max_priority state bis =
  iter_s (insert_block cctxt ?max_priority state) bis >>= function
  | Ok () ->
      Lwt.return_unit
  | Error err ->
      Format.eprintf "Error: %a" pp_print_error err  ;
      Lwt.return_unit

let mine cctxt state =
  let slots = pop_mining_slots state in
  map_p
    (fun (timestamp, (bi, prio, delegate)) ->
       let block = `Hash bi.Client_mining_blocks.hash in
       let timestamp =
         if Block_hash.equal bi.Client_mining_blocks.hash state.genesis then
           Time.now ()
         else
           timestamp in
       Client_keys.Public_key_hash.name cctxt delegate >>=? fun name ->
       lwt_debug "Try mining after %a (slot %d) for %s (%a)"
         Block_hash.pp_short bi.hash
         prio name Time.pp_hum timestamp >>= fun () ->
       Client_node_rpcs.Blocks.pending_operations cctxt.rpc_config
         block >>=? fun (res, ops) ->
       let operations =
         let open Operation_hash.Set in
         elements (union ops (Prevalidation.preapply_result_operations res)) in
       let request = List.length operations in
       Client_node_rpcs.Blocks.preapply cctxt.rpc_config block
         ~timestamp ~sort:true operations >>= function
       | Error errs ->
           lwt_log_error "Error while prevalidating operations:\n%a"
             pp_print_error
             errs >>= fun () ->
           return None
       | Ok { operations ; fitness ; timestamp } ->
           lwt_debug
             "Computed condidate block after %a (slot %d): %d/%d fitness: %a"
             Block_hash.pp_short bi.hash prio
             (List.length operations.applied) request
             Fitness.pp fitness
           >>= fun () ->
           return
             (Some (bi, prio, fitness, timestamp, operations, delegate)))
    slots >>=? fun candidates ->
  let candidates =
    List.sort
      (fun (_,_,f1,_,_,_) (_,_,f2,_,_,_) -> ~- (Fitness.compare f1 f2))
      (Utils.unopt_list candidates) in
  match candidates with
  | (bi, priority, fitness, timestamp, operations, delegate) :: _
    when Fitness.compare state.best.fitness fitness < 0 -> begin
      let level = Raw_level.succ bi.level.level in
      cctxt.message
        "Select candidate block after %a (slot %d) fitness: %a"
        Block_hash.pp_short bi.hash priority
        Fitness.pp fitness >>= fun () ->
      let seed_nonce = generate_seed_nonce () in
      Client_keys.get_key cctxt delegate >>=? fun (_,_,src_sk) ->
      inject_block cctxt.rpc_config
        ~force:true ~src_sk ~priority ~timestamp ~fitness ~seed_nonce
        (`Hash bi.hash) [operations.applied]
      |> trace_exn (Failure "Error while injecting block") >>=? fun block_hash ->
      State.record_block cctxt level block_hash seed_nonce
      |> trace_exn (Failure "Error while recording block") >>=? fun () ->
      Client_keys.Public_key_hash.name cctxt delegate >>=? fun name ->
      cctxt.message
        "Injected block %a for %s after %a \
        \ (level %a, slot %d, fitness %a, operations %d)"
        Block_hash.pp_short block_hash
        name
        Block_hash.pp_short bi.hash
        Raw_level.pp level priority
        Fitness.pp fitness
        (List.length operations.applied) >>= fun () ->
      return ()
    end
  | _ ->
      lwt_debug "No valid candidates." >>= fun () ->
      return ()

let create
    cctxt ?max_priority delegates
    (block_stream:
       Client_mining_blocks.block_info list tzresult Lwt_stream.t)
    (endorsement_stream:
       Client_mining_operations.valid_endorsement tzresult Lwt_stream.t) =
  Lwt_stream.get block_stream >>= function
  | None | Some (Ok [] | Error _) ->
      cctxt.Client_commands.error "Can't fetch the current block head."
  | Some (Ok (bi :: _ as initial_heads)) ->
      Client_node_rpcs.Blocks.hash cctxt.rpc_config `Genesis >>=? fun genesis_hash ->
      let last_get_block = ref None in
      let get_block () =
        match !last_get_block with
        | None ->
            let t = Lwt_stream.get block_stream in
            last_get_block := Some t ;
            t
        | Some t -> t in
      let last_get_endorsement = ref None in
      let get_endorsement () =
        match !last_get_endorsement with
        | None ->
            let t = Lwt_stream.get endorsement_stream in
            last_get_endorsement := Some t ;
            t
        | Some t -> t in
      let state = create_state genesis_hash delegates bi in
      insert_blocks cctxt ?max_priority state initial_heads >>= fun () ->
      let rec worker_loop () =
        let timeout = compute_timeout state in
        Lwt.choose [ (timeout >|= fun () -> `Timeout) ;
                     (get_block () >|= fun b -> `Hash b) ;
                     (get_endorsement () >|= fun e -> `Endorsement e) ;
                   ] >>= function
        | `Hash (None | Some (Error _))
        | `Endorsement (None | Some (Error _)) ->
            Lwt.return_unit
        | `Hash (Some (Ok bis)) -> begin
            Lwt.cancel timeout ;
            last_get_block := None ;
            lwt_debug
              "@[<hov 2>Discoverer blocks:@ %a@]"
              (Format.pp_print_list
                 (fun ppf bi ->
                    Block_hash.pp_short ppf bi.Client_mining_blocks.hash))
              bis
              >>= fun () ->
              insert_blocks cctxt ?max_priority state bis >>= fun () ->
              worker_loop ()
          end
        | `Endorsement (Some (Ok e)) ->
            Lwt.cancel timeout ;
            last_get_endorsement := None ;
            Client_keys.Public_key_hash.name cctxt
              e.Client_mining_operations.source >>= fun _source ->
            (* TODO *)
            worker_loop ()
        | `Timeout ->
            lwt_debug "Waking up for mining..." >>= fun () ->
            begin
              mine cctxt state >>= function
              | Ok () -> Lwt.return_unit
              | Error errs ->
                  lwt_log_error "Error while mining:\n%a"
                    pp_print_error
                    errs >>= fun () ->
                  Lwt.return_unit
            end >>= fun () ->
            worker_loop () in
  lwt_log_info "Starting mining daemon" >>= fun () ->
  worker_loop () >>= fun () ->
  return ()

(* FIXME bug in ocamldep ?? *)
open Level
