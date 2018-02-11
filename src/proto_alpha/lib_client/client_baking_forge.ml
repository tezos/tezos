(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Proto_alpha
open Alpha_context

open Logging.Client.Baking

let generate_proof_of_work_nonce () =
  Rand.generate Constants.proof_of_work_nonce_size

let generate_seed_nonce () =
  match Nonce.of_bytes @@
    Rand.generate Constants.nonce_length with
  | Error _ -> assert false
  | Ok nonce -> nonce

let forge_block_header
    cctxt block delegate_sk shell priority seed_nonce_hash =
  Client_proto_rpcs.Constants.stamp_threshold
    cctxt block >>=? fun stamp_threshold ->
  let rec loop () =
    let proof_of_work_nonce = generate_proof_of_work_nonce () in
    let unsigned_header =
      Alpha_context.Block_header.forge_unsigned
        shell { priority ; seed_nonce_hash ; proof_of_work_nonce } in
    Client_keys.append delegate_sk unsigned_header >>=? fun signed_header ->
    let block_hash = Block_hash.hash_bytes [signed_header] in
    if Baking.check_hash block_hash stamp_threshold then
      return signed_header
    else
      loop () in
  loop ()

let empty_proof_of_work_nonce =
  MBytes.of_string
    (String.make Constants_repr.proof_of_work_nonce_size  '\000')

let forge_faked_proto_header ~priority ~seed_nonce_hash =
  Alpha_context.Block_header.forge_unsigned_proto_header
    { priority ; seed_nonce_hash ;
      proof_of_work_nonce = empty_proof_of_work_nonce }

let assert_valid_operations_hash shell_header operations =
  let operations_hash =
    Operation_list_list_hash.compute
      (List.map Operation_list_hash.compute
         (List.map
            (List.map Tezos_base.Operation.hash) operations)) in
  fail_unless
    (Operation_list_list_hash.equal
       operations_hash shell_header.Tezos_base.Block_header.operations_hash)
    (failure
       "Client_baking_forge.inject_block: \
        inconsistent header.")

let inject_block cctxt
    ?force ?net_id
    ~shell_header ~priority ~seed_nonce_hash ~src_sk operations =
  assert_valid_operations_hash shell_header operations >>=? fun () ->
  let block = `Hash shell_header.Tezos_base.Block_header.predecessor in
  forge_block_header cctxt block
    src_sk shell_header priority seed_nonce_hash >>=? fun signed_header ->
  Shell_services.inject_block cctxt
    ?force ?net_id signed_header operations >>=? fun block_hash ->
  return block_hash

type error +=
  | Failed_to_preapply of Tezos_base.Operation.t * error list

let () =
  register_error_kind
    `Permanent
    ~id:"Client_baking_forge.failed_to_preapply"
    ~title: "Fail to preapply an operation"
    ~description: ""
    ~pp:(fun ppf (op, err) ->
        let h = Tezos_base.Operation.hash op in
        Format.fprintf ppf "@[Failed to preapply %a:@ %a@]"
          Operation_hash.pp_short h
          pp_print_error err)
    Data_encoding.
      (obj2
         (req "operation" (dynamic_size Tezos_base.Operation.encoding))
         (req "error" RPC_error.encoding))
    (function
      | Failed_to_preapply (hash, err) -> Some (hash, err)
      | _ -> None)
    (fun (hash, err) -> Failed_to_preapply (hash, err))

let classify_operations (ops: Operation.raw list) =
  let t = Array.make (List.length Proto_alpha.Main.validation_passes) [] in
  List.iter
    (fun op ->
       let h = Operation.hash_raw op in
       match Operation.parse h op with
       | Ok o ->
           List.iter
             (fun pass -> t.(pass) <- op :: t.(pass))
             (Proto_alpha.Main.acceptable_passes o)
       | Error _ -> ())
    ops ;
  Array.fold_right (fun ops acc -> List.rev ops :: acc) t []

let forge_block cctxt block
    ?force
    ?operations ?(best_effort = operations = None) ?(sort = best_effort)
    ?timestamp
    ~priority
    ~seed_nonce_hash ~src_sk () =
  let block = Block_services.last_baked_block block in
  begin
    match operations with
    | None ->
        Block_services.pending_operations
          cctxt block >>=? fun (ops, pendings) ->
        let ops =
          List.map snd @@
          Operation_hash.Map.bindings @@
          Operation_hash.Map.fold
            Operation_hash.Map.add
            (Preapply_result.operations ops)
            pendings in
        return ops
    | Some operations ->
        return operations
  end >>=? fun operations ->
  begin
    match priority with
    | `Set prio -> begin
        Client_proto_rpcs.Helpers.minimal_time
          cctxt block ~prio () >>=? fun time ->
        return (prio, time)
      end
    | `Auto (src_pkh, max_priority, free_baking) ->
        Client_proto_rpcs.Context.next_level cctxt block >>=? fun { level } ->
        Client_proto_rpcs.Helpers.Rights.baking_rights_for_delegate cctxt
          ?max_priority
          ~first_level:level
          ~last_level:level
          block src_pkh () >>=? fun possibilities ->
        try
          begin
            if free_baking then
              Client_proto_rpcs.Constants.first_free_baking_slot cctxt block
            else
              return 0
          end >>=? fun min_prio ->
          let _, prio, time =
            List.find (fun (l,p,_) -> l = level && p >= min_prio) possibilities in
          return (prio, time)
        with Not_found ->
          failwith "No slot found at level %a" Raw_level.pp level
  end >>=? fun (priority, minimal_timestamp) ->
  (* lwt_log_info "Baking block at level %a prio %d" *)
  (* Raw_level.pp level priority >>= fun () -> *)
  begin
    match timestamp, minimal_timestamp with
    | None, timestamp -> return timestamp
    | Some timestamp, minimal_timestamp ->
        if timestamp < minimal_timestamp then
          failwith
            "Proposed timestamp %a is earlier than minimal timestamp %a"
            Time.pp_hum timestamp
            Time.pp_hum minimal_timestamp
        else
          return timestamp
  end >>=? fun timestamp ->
  let request = List.length operations in
  let proto_header = forge_faked_proto_header ~priority ~seed_nonce_hash in
  let operations = classify_operations operations in
  Block_services.preapply
    cctxt block ~timestamp ~sort ~proto_header operations >>=?
  fun { operations = result ; shell_header } ->
  let valid = List.fold_left (fun acc r -> acc + List.length r.Preapply_result.applied) 0 result in
  lwt_log_info "Found %d valid operations (%d refused) for timestamp %a"
    valid (request - valid)
    Time.pp_hum timestamp >>= fun () ->
  lwt_log_info "Computed fitness %a"
    Fitness.pp shell_header.fitness >>= fun () ->
  if best_effort
  || List.for_all (fun l ->
         Operation_hash.Map.is_empty l.Preapply_result.refused
         && Operation_hash.Map.is_empty l.branch_refused
         && Operation_hash.Map.is_empty l.branch_delayed )
       result
  then
    let operations =
      if not best_effort then operations
      else List.map (fun l -> List.map snd l.Preapply_result.applied) result in
    Block_services.info cctxt block >>=? fun {net_id} ->
    inject_block cctxt
      ?force ~net_id ~shell_header ~priority ~seed_nonce_hash ~src_sk
      operations
  else
    let result =
      let merge old neu =
        let open Preapply_result in
        let merge _key a b =
          match a, b with
          | None, None -> None
          | Some x, None -> Some x
          | _, Some y -> Some y in
        { applied = [] ;
          refused =
            Operation_hash.Map.merge merge
              old.refused
              neu.refused ;
          branch_refused =
            Operation_hash.Map.merge merge
              old.branch_refused
              neu.branch_refused ;
          branch_delayed =
            Operation_hash.Map.merge merge
              old.branch_delayed
              neu.branch_delayed } in
      List.fold_left merge Preapply_result.empty result in
    Lwt.return_error @@
    List.filter_map
      (fun op ->
         let h = Tezos_base.Operation.hash op in
         try Some (Failed_to_preapply
                     (op, snd @@ Operation_hash.Map.find h result.refused))
         with Not_found ->
         try Some (Failed_to_preapply
                     (op, snd @@ Operation_hash.Map.find h result.branch_refused))
         with Not_found ->
         try Some (Failed_to_preapply
                     (op, snd @@ Operation_hash.Map.find h result.branch_delayed))
         with Not_found -> None)
      (List.concat operations)

(** Worker *)

module State : sig

  val get_block:
    #Client_commands.wallet ->
    Raw_level.t -> Block_hash.t list tzresult Lwt.t

  val record_block:
    #Client_commands.wallet ->
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

  let name =
    "blocks"

  let load (wallet : #Client_commands.wallet) =
    wallet#load name ~default:LevelMap.empty encoding

  let save (wallet : #Client_commands.wallet) map =
    wallet#write name map encoding

  let lock = Lwt_mutex.create ()

  let get_block (cctxt : #Client_commands.wallet) level =
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

let get_baking_slot cctxt
    ?max_priority (bi: Client_baking_blocks.block_info) delegates =
  let block = `Hash bi.hash in
  let level = Raw_level.succ bi.level.level in
  Lwt_list.filter_map_p
    (fun delegate ->
       Client_proto_rpcs.Helpers.Rights.baking_rights_for_delegate cctxt
         ?max_priority
         ~first_level:level
         ~last_level:level
         block delegate () >>= function
       | Error errs ->
           log_error "Error while fetching baking possibilities:\n%a"
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

let rec insert_baking_slot slot = function
  | [] -> [slot]
  | ((timestamp,_) :: _) as slots when Time.(fst slot < timestamp) ->
      slot :: slots
  | slot' :: slots -> slot' :: insert_baking_slot slot slots

type state = {
  genesis: Block_hash.t ;
  delegates: public_key_hash list ;
  mutable best: Client_baking_blocks.block_info ;
  mutable future_slots:
    (Time.t * (Client_baking_blocks.block_info * int * public_key_hash)) list ;
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

let get_unrevealed_nonces (cctxt : #Client_commands.full_context) ?(force = false) block =
  Client_proto_rpcs.Context.next_level cctxt block >>=? fun level ->
  let cur_cycle = level.cycle in
  match Cycle.pred cur_cycle with
  | None -> return []
  | Some cycle ->
      Client_baking_blocks.blocks_from_cycle
        cctxt block cycle >>=? fun blocks ->
      filter_map_s (fun hash ->
          Client_proto_nonces.find cctxt hash >>=? function
          | None -> return None
          | Some nonce ->
              Client_proto_rpcs.Context.level
                cctxt (`Hash hash) >>=? fun level ->
              if force then
                return (Some (hash, (level.level, nonce)))
              else
                Client_proto_rpcs.Context.Nonce.get
                  cctxt block level.level >>=? function
                | Missing nonce_hash
                  when Nonce.check_hash nonce nonce_hash ->
                    cctxt#warning "Found nonce for %a (level: %a)@."
                      Block_hash.pp_short hash
                      Level.pp level >>= fun () ->
                    return (Some (hash, (level.level, nonce)))
                | Missing _nonce_hash ->
                    cctxt#error "Incoherent nonce for level %a"
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
    (cctxt : #Client_commands.full_context) ?max_priority state (bi: Client_baking_blocks.block_info) =
  begin
    safe_get_unrevealed_nonces cctxt (`Hash bi.hash) >>= fun nonces ->
    Client_baking_revelation.forge_seed_nonce_revelation
      cctxt (`Hash bi.hash) (List.map snd nonces)
  end >>= fun _ignore_error ->
  if Fitness.compare state.best.fitness bi.fitness < 0 then begin
    state.best <- bi ;
    drop_old_slots
      ~before:(Time.add state.best.timestamp (-1800L)) state ;
  end ;
  get_delegates cctxt state >>=? fun delegates ->
  get_baking_slot cctxt ?max_priority bi delegates >>= function
  | None ->
      lwt_debug
        "Can't compute slot for %a" Block_hash.pp_short bi.hash >>= fun () ->
      return ()
  | Some ((timestamp, (_,_,delegate)) as slot) ->
      Client_keys.Public_key_hash.name cctxt delegate >>=? fun name ->
      lwt_log_info "New baking slot at %a for %s after %a"
        Time.pp_hum timestamp
        name
        Block_hash.pp_short bi.hash >>= fun () ->
      state.future_slots <- insert_baking_slot slot state.future_slots ;
      return ()

let pop_baking_slots state =
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

let bake (cctxt : #Client_commands.full_context) state =
  let slots = pop_baking_slots state in
  let seed_nonce = generate_seed_nonce () in
  let seed_nonce_hash = Nonce.hash seed_nonce in
  filter_map_s
    (fun (timestamp, (bi, priority, delegate)) ->
       let block = `Hash bi.Client_baking_blocks.hash in
       let timestamp =
         if Block_hash.equal bi.Client_baking_blocks.hash state.genesis then
           Time.now ()
         else
           timestamp in
       Client_keys.Public_key_hash.name cctxt delegate >>=? fun name ->
       lwt_debug "Try baking after %a (slot %d) for %s (%a)"
         Block_hash.pp_short bi.hash
         priority name Time.pp_hum timestamp >>= fun () ->
       Block_services.pending_operations cctxt
         block >>=? fun (res, ops) ->
       let operations =
         List.map snd @@
         Operation_hash.Map.bindings @@
         Operation_hash.Map.(fold add)
           ops (Preapply_result.operations res) in
       let request = List.length operations in
       let proto_header =
         forge_faked_proto_header ~priority ~seed_nonce_hash in
       Block_services.preapply cctxt block
         ~timestamp ~sort:true ~proto_header [operations] >>= function
       | Error errs ->
           lwt_log_error "Error while prevalidating operations:\n%a"
             pp_print_error
             errs >>= fun () ->
           return None
       | Ok { operations ; shell_header } ->
           let operations = List.hd operations in
           lwt_debug
             "Computed condidate block after %a (slot %d): %d/%d fitness: %a"
             Block_hash.pp_short bi.hash priority
             (List.length operations.applied) request
             Fitness.pp shell_header.fitness
           >>= fun () ->
           return
             (Some (bi, priority, shell_header, operations, delegate)))
    slots >>=? fun candidates ->
  let candidates =
    List.sort
      (fun (_,_,h1,_,_) (_,_,h2,_,_) ->
         match
           Fitness.compare h1.Tezos_base.Block_header.fitness h2.fitness
         with
         | 0 ->
             Time.compare h1.timestamp h2.timestamp
         | cmp -> ~- cmp)
      candidates in
  match candidates with
  | (bi, priority, shell_header, operations, delegate) :: _
    when Fitness.compare state.best.fitness shell_header.fitness < 0 ||
         (Fitness.compare state.best.fitness shell_header.fitness = 0 &&
          Time.compare shell_header.timestamp state.best.timestamp < 0) -> begin
      let level = Raw_level.succ bi.level.level in
      cctxt#message
        "Select candidate block after %a (slot %d) fitness: %a"
        Block_hash.pp_short bi.hash priority
        Fitness.pp shell_header.fitness >>= fun () ->
      Client_keys.get_key cctxt delegate >>=? fun (_,_,src_sk) ->
      inject_block cctxt
        ~force:true ~net_id:bi.net_id
        ~shell_header ~priority ~seed_nonce_hash ~src_sk
        [List.map snd operations.applied]
      |> trace_exn (Failure "Error while injecting block") >>=? fun block_hash ->
      State.record_block cctxt level block_hash seed_nonce
      |> trace_exn (Failure "Error while recording block") >>=? fun () ->
      Client_keys.Public_key_hash.name cctxt delegate >>=? fun name ->
      cctxt#message
        "Injected block %a for %s after %a \
        \ (level %a, slot %d, fitness %a, operations %d)"
        Block_hash.pp_short block_hash
        name
        Block_hash.pp_short bi.hash
        Raw_level.pp level priority
        Fitness.pp shell_header.fitness
        (List.length operations.applied) >>= fun () ->
      return ()
    end
  | _ ->
      lwt_debug "No valid candidates." >>= fun () ->
      return ()

let create
    (cctxt : #Client_commands.full_context) ?max_priority delegates
    (block_stream:
       Client_baking_blocks.block_info list tzresult Lwt_stream.t)
    (endorsement_stream:
       Client_baking_operations.valid_endorsement tzresult Lwt_stream.t) =
  Lwt_stream.get block_stream >>= function
  | None | Some (Ok [] | Error _) ->
      cctxt#error "Can't fetch the current block head."
  | Some (Ok (bi :: _ as initial_heads)) ->
      Block_services.hash cctxt `Genesis >>=? fun genesis_hash ->
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
                    Block_hash.pp_short ppf bi.Client_baking_blocks.hash))
              bis
            >>= fun () ->
            insert_blocks cctxt ?max_priority state bis >>= fun () ->
            worker_loop ()
          end
        | `Endorsement (Some (Ok e)) ->
            Lwt.cancel timeout ;
            last_get_endorsement := None ;
            Client_keys.Public_key_hash.name cctxt
              e.Client_baking_operations.source >>= fun _source ->
            (* TODO *)
            worker_loop ()
        | `Timeout ->
            lwt_debug "Waking up for baking..." >>= fun () ->
            begin
              bake cctxt state >>= function
              | Ok () -> Lwt.return_unit
              | Error errs ->
                  lwt_log_error "Error while baking:\n%a"
                    pp_print_error
                    errs >>= fun () ->
                  Lwt.return_unit
            end >>= fun () ->
            worker_loop () in
      lwt_log_info "Starting baking daemon" >>= fun () ->
      worker_loop () >>= fun () ->
      return ()
