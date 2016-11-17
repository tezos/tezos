(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Logging.Client.Mining
module Ed25519 = Environment.Ed25519

let generate_proof_of_work_nonce () =
  Sodium.Random.Bigbytes.generate Constants.proof_of_work_nonce_size

let generate_seed_nonce () =
  match Nonce.of_bytes @@
    Sodium.Random.Bigbytes.generate Constants.nonce_length with
  | Error _ -> assert false
  | Ok nonce -> nonce

let rec compute_stamp block delegate_sk shell mining_slot seed_nonce_hash =
  Client_proto_rpcs.Constants.stamp_threshold block >>=? fun stamp_threshold ->
  let rec loop () =
    let proof_of_work_nonce = generate_proof_of_work_nonce () in
    let unsigned_header =
      Tezos_context.Block.forge_header
        shell { mining_slot ; seed_nonce_hash ; proof_of_work_nonce } in
    let signed_header =
      Ed25519.append_signature delegate_sk unsigned_header in
    let block_hash = Block_hash.hash_bytes [signed_header] in
    if Mining.check_hash block_hash stamp_threshold then
      proof_of_work_nonce
    else
      loop () in
  return (loop ())

let inject_block block
    ?force
    ~priority ~timestamp ~fitness ~seed_nonce
    ~src_sk operations =
  let block = match block with `Prevalidation -> `Head 0 | block -> block in
  Client_node_rpcs.Blocks.info block >>= fun bi ->
  let seed_nonce_hash = Nonce.hash seed_nonce in
  Client_proto_rpcs.Context.next_level block >>=? fun level ->
  let shell =
    { Store.net_id = bi.net ; predecessor = bi.hash ;
      timestamp ; fitness ; operations } in
  let slot = level.level, Int32.of_int priority in
  compute_stamp block
    src_sk shell slot seed_nonce_hash >>=? fun proof_of_work_nonce ->
  Client_proto_rpcs.Helpers.Forge.block
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
  let signed_header = Ed25519.append_signature src_sk unsigned_header in
  Client_node_rpcs.inject_block
    ~wait:true ?force signed_header >>=? fun block_hash ->
  return block_hash

let forge_block block
    ?force
    ?operations ?(best_effort = operations = None) ?(sort = best_effort)
    ?timestamp ?max_priority ?priority
    ~seed_nonce ~src_sk src_pkh =
  let block =
    match block with
    | `Prevalidation -> `Head 0
    | `Test_prevalidation -> `Test_head 0
    | block -> block in
  Client_proto_rpcs.Context.level block >>=? fun level ->
  let level = Raw_level.succ level.level in
  begin
    match operations with
    | None ->
        Client_node_rpcs.Blocks.pending_operations block >|= fun (ops, pendings) ->
        Operation_hash_set.elements @@
        Operation_hash_set.union (Updater.operations ops) pendings
    | Some operations -> Lwt.return operations
  end >>= fun operations ->
  begin
    match priority with
    | Some prio -> begin
        Client_proto_rpcs.Helpers.minimal_time block ~prio () >>=? fun time ->
        return (prio, Some time)
      end
    | None ->
        Client_proto_rpcs.Helpers.Rights.mining_rights_for_delegate
          ?max_priority
          ~first_level:level
          ~last_level:level
          block src_pkh () >>=? fun possibilities ->
        try
          let _, prio, time =
            List.find (fun (l,_,_) -> l = level) possibilities in
          return (prio, time)
        with Not_found ->
          failwith "No slot found at level %a" Raw_level.pp level
  end >>=? fun (priority, minimal_timestamp) ->
  lwt_log_info "Mining block at level %a prio %d"
    Raw_level.pp level priority >>= fun () ->
  begin
    match timestamp, minimal_timestamp with
    | None, None -> failwith "Can't compute the expected timestamp"
    | None, timestamp | timestamp, None -> return timestamp
    | Some timestamp, Some minimal_timestamp ->
        if timestamp < minimal_timestamp then
          failwith
            "Proposed timestamp %a is earlier than minimal timestamp %a"
            Time.pp_hum timestamp
            Time.pp_hum minimal_timestamp
        else
          return (Some timestamp)
  end >>=? fun timestamp ->
  let request = List.length operations in
  Client_node_rpcs.Blocks.preapply block ?timestamp ~sort operations >>=?
  fun { operations ; fitness ; timestamp } ->
  let valid = List.length operations.applied in
  lwt_log_info "Found %d valid operations (%d refused) for timestamp %a"
    valid (request - valid)
    Time.pp_hum timestamp >>= fun () ->
  lwt_log_info "Computed fitness %a" Fitness.pp fitness >>= fun () ->
  if best_effort
     || ( Operation_hash_map.is_empty operations.refused
          && Operation_hash_map.is_empty operations.branch_refused
          && Operation_hash_map.is_empty operations.branch_delayed ) then
    inject_block ?force ~src_sk
       ~priority ~timestamp ~fitness ~seed_nonce block operations.applied
  else
    failwith "Cannot (fully) validate the given operations."


(** Worker *)

module State : sig

  val get_block:
    Raw_level.t -> Block_hash.t list tzresult Lwt.t

  val record_block:
    Raw_level.t -> Block_hash.t -> Nonce.t -> unit tzresult Lwt.t

end = struct

  module LevelMap = Map.Make(Raw_level)

  type t = Block_hash.t list LevelMap.t
  let encoding : t Data_encoding.t =
    let open Data_encoding in
    conv
      (fun x -> LevelMap.bindings x)
      (fun l -> List.fold_left (fun x (y, z) -> LevelMap.add y z x) LevelMap.empty l)
      (list (obj2
               (req "level" Raw_level.encoding)
               (req "blocks" (list Block_hash.encoding))))

  let filename () =
    Client_config.(base_dir#get // "blocks")

  let load () =
    let filename = filename () in
    if not (Sys.file_exists filename) then return LevelMap.empty else
      Data_encoding.Json.read_file filename >>= function
      | None ->
          failwith "couldn't to read the block file"
      | Some json ->
          match Data_encoding.Json.destruct encoding json with
          | exception _ -> (* TODO print_error *)
              failwith "didn't understand the block file"
          | map ->
              return map

  let save map =
    Lwt.catch
      (fun () ->
         let dirname = Client_config.base_dir#get in
         (if not (Sys.file_exists dirname) then Utils.create_dir dirname
          else Lwt.return ()) >>= fun () ->
         let filename = filename () in
         let json = Data_encoding.Json.construct encoding map in
         Data_encoding.Json.write_file filename json >>= function
         | false -> failwith "Json.write_file"
         | true -> return ())
      (fun exn ->
         failwith
           "could not write the block file: %s."
           (Printexc.to_string exn))

  let lock = Lwt_mutex.create ()

  let get_block level =
    Lwt_mutex.with_lock lock
      (fun () ->
         load () >>=? fun map ->
         try
           let blocks = LevelMap.find level map in
           return blocks
         with Not_found -> return [])

  let record_block level hash nonce =
    Lwt_mutex.with_lock lock
      (fun () ->
         load () >>=? fun map ->
         let previous =
           try LevelMap.find level map
           with Not_found -> [] in
         save
           (LevelMap.add level (hash :: previous) map)) >>=? fun () ->
    Client_proto_nonces.add hash nonce

end

let get_mining_slot
    ?max_priority (bi: Client_mining_blocks.block_info) delegates =
  let block = `Hash bi.hash in
  let level = Raw_level.succ bi.level.level in
  Lwt_list.filter_map_p
    (fun delegate ->
       Client_proto_rpcs.Helpers.Rights.mining_rights_for_delegate
         ?max_priority
         ~first_level:level
         ~last_level:level
         block delegate () >>= function
       | Error errs ->
           log_error "Error while fetching mining possibilities:\n%a"
             pp_print_error errs ;
           Lwt.return_none
       | Ok slots ->
           let convert = function
             | (_,_,None) -> None
             | (_lvl, slot, Some timestamp) ->
                 Some (timestamp, (bi, slot, delegate)) in
           Lwt.return (Some (Utils.filter_map convert slots)))
    delegates >>= fun slots ->
  let sorted_slots =
    List.sort (fun (t1,_) (t2,_) -> Time.compare t1 t2) (List.flatten slots) in
  match sorted_slots with
  | [] -> Lwt.return None
  | slot :: _ -> Lwt.return (Some slot)

let rec insert_mining_slot slot = function
  | [] -> [slot]
  | ((timestamp,_) :: _) as slots when Time.(fst slot < timestamp) -> slot :: slots
  | slot' :: slots -> slot' :: insert_mining_slot slot slots

type state = {
  genesis: Block_hash.t ;
  delegates: public_key_hash list ;
  mutable best_fitness: Fitness.t ;
  mutable future_slots:
    (Time.t * (Client_mining_blocks.block_info * int * public_key_hash)) list ;
}

let create_state genesis delegates best_fitness =
  { genesis ;
    delegates ;
    best_fitness ;
    future_slots = [] ;
  }

let compute_timeout { future_slots } =
  match future_slots with
  | [] ->
      Lwt_utils.never_ending
  | (timestamp, _) :: _ ->
      let now = Time.now () in
      let delay = Time.diff timestamp now in
      if delay <= 0L then
        Lwt.return_unit
      else
        Lwt_unix.sleep (Int64.to_float delay)

let insert_block ?max_priority state (bi: Client_mining_blocks.block_info) =
  if Fitness.compare state.best_fitness bi.fitness < 0 then
    state.best_fitness <- bi.fitness ;
  get_mining_slot ?max_priority bi state.delegates >>= function
  | None ->
      lwt_debug
        "Can't compute slot for %a" Block_hash.pp_short bi.hash >>= fun () ->
      Lwt.return_unit
  | Some ((timestamp, (_,_,delegate)) as slot) ->
      Client_keys.Public_key_hash.name delegate >>= fun name ->
      lwt_log_info "New mining slot at %a for %s after %a"
        Time.pp_hum timestamp
        name
        Block_hash.pp_short bi.hash >>= fun () ->
      state.future_slots <- insert_mining_slot slot state.future_slots ;
      Lwt.return_unit

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

let insert_blocks ?max_priority state bis =
  Lwt_list.iter_s (insert_block ?max_priority state) bis

let mine state =
  let slots = pop_mining_slots state in
  Lwt_list.map_p
    (fun (timestamp, (bi, prio, delegate)) ->
       let block = `Hash bi.Client_mining_blocks.hash in
       let timestamp =
         if Block_hash.equal bi.Client_mining_blocks.hash state.genesis then
           Time.now ()
         else
           timestamp in
       Client_keys.Public_key_hash.name delegate >>= fun name ->
       lwt_debug "Try mining after %a (slot %d) for %s (%a)"
         Block_hash.pp_short bi.hash
         prio name Time.pp_hum timestamp >>= fun () ->
       Client_node_rpcs.Blocks.pending_operations
         block >>= fun (res, ops) ->
       let operations =
         let open Operation_hash_set in
         elements (union ops (Updater.operations res)) in
       let request = List.length operations in
       Client_node_rpcs.Blocks.preapply block
         ~timestamp ~sort:true operations >>= function
       | Error errs ->
           lwt_log_error "Error while prevalidating operations:\n%a"
             pp_print_error
             errs >>= fun () ->
           Lwt.return_none
       | Ok { operations ; fitness ; timestamp } ->
           lwt_debug
             "Computed condidate block after %a (slot %d): %d/%d fitness: %a"
             Block_hash.pp_short bi.hash prio
             (List.length operations.applied) request
             Fitness.pp fitness
           >>= fun () ->
           Lwt.return
             (Some (bi, prio, fitness, timestamp, operations, delegate)))
    slots >>= fun candidates ->
  let candidates =
    List.sort
      (fun (_,_,f1,_,_,_) (_,_,f2,_,_,_) -> ~- (Fitness.compare f1 f2))
      (Utils.unopt_list candidates) in
  match candidates with
  | (bi, priority, fitness, timestamp, operations, delegate) :: _
    when Fitness.compare state.best_fitness fitness < 0 -> begin
      let level = Raw_level.succ bi.level.level in
      lwt_log_info
        "Select candidate block after %a (slot %d) fitness: %a"
        Block_hash.pp_short bi.hash priority
        Fitness.pp fitness >>= fun () ->
      let seed_nonce = generate_seed_nonce () in
      Client_keys.get_key delegate >>=? fun (_,_,src_sk) ->
      inject_block ~force:true ~src_sk ~priority ~timestamp ~fitness ~seed_nonce
        (`Hash bi.hash) operations.applied
      |> trace_exn (Failure "Error while injecting block") >>=? fun block_hash ->
      State.record_block level block_hash seed_nonce
      |> trace_exn (Failure "Error while recording block") >>=? fun () ->
      Client_keys.Public_key_hash.name delegate >>= fun name ->
      Cli_entries.message
        "Injected block %a for %s after %a \
        \ (level %a, slot %d, fitness %a, operations %d)"
        Block_hash.pp_short block_hash
        name
        Block_hash.pp_short bi.hash
        Raw_level.pp level priority
        Fitness.pp fitness
        (List.length operations.applied) ;
      return ()
    end
  | _ ->
      lwt_debug "No valid candidates." >>= fun () ->
      return ()

let create ?max_priority delegates
    (block_stream: Client_mining_blocks.block_info list Lwt_stream.t)
    (endorsement_stream: Client_mining_operations.valid_endorsement Lwt_stream.t) =
  Lwt_stream.get block_stream >>= function
  | None | Some [] ->
      Cli_entries.error "Can't fetch the current block head."
  | Some ({ Client_mining_blocks.fitness } :: _ as initial_heads) ->
      Client_node_rpcs.Blocks.hash `Genesis >>= fun genesis_hash ->
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
      let state = create_state genesis_hash delegates fitness in
      insert_blocks ?max_priority state initial_heads >>= fun () ->
      let rec worker_loop () =
        let timeout = compute_timeout state in
        Lwt.choose [ (timeout >|= fun () -> `Timeout) ;
                     (get_block () >|= fun b -> `Hash b) ;
                     (get_endorsement () >|= fun e -> `Endorsement e) ;
                   ] >>= function
        | `Hash None
        | `Endorsement None ->
            Lwt.return_unit
        | `Hash (Some bis) -> begin
            Lwt.cancel timeout ;
            last_get_block := None ;
            lwt_debug
              "@[<hov 2>Discoverer blocks:@ %a@]"
              (Format.pp_print_list
                 (fun ppf bi ->
                    Block_hash.pp_short ppf bi.Client_mining_blocks.hash))
              bis
              >>= fun () ->
              insert_blocks ?max_priority state bis >>= fun () ->
              worker_loop ()
          end
        | `Endorsement (Some e) ->
            Lwt.cancel timeout ;
            last_get_endorsement := None ;
            Client_keys.Public_key_hash.name
              e.Client_mining_operations.source >>= fun _source ->
            (* TODO *)
            worker_loop ()
        | `Timeout ->
            lwt_debug "Waking up for mining..." >>= fun () ->
            begin
              mine state >>= function
              | Ok () -> Lwt.return_unit
              | Error errs ->
                  lwt_log_error "Error while mining:\n%a"
                    pp_print_error
                    errs >>= fun () ->
                  Lwt.return_unit
            end >>= fun () ->
            worker_loop () in
  lwt_log_info "Starting mining daemon" >>= fun () ->
  worker_loop ()

(* FIXME bug in ocamldep ?? *)
open Level
