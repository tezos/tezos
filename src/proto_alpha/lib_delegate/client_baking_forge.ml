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

include Logging.Make(struct let name = "client.baking" end)

let generate_proof_of_work_nonce () =
  Rand.generate Constants.proof_of_work_nonce_size

let generate_seed_nonce () =
  match Nonce.of_bytes @@
    Rand.generate Constants.nonce_length with
  | Error _ -> assert false
  | Ok nonce -> nonce


let forge_block_header
    (cctxt : #Proto_alpha.full)
    ?(chain = `Main) block delegate_sk shell priority seed_nonce_hash =
  Alpha_services.Constants.all cctxt
    (chain, block) >>=? fun { parametric = {
      proof_of_work_threshold = stamp_threshold ;
    } } ->
  let rec loop () =
    let proof_of_work_nonce = generate_proof_of_work_nonce () in
    let contents =
      { Block_header.priority ; seed_nonce_hash ; proof_of_work_nonce } in
    if Baking.check_header_proof_of_work_stamp shell contents stamp_threshold then
      let unsigned_header =
        Data_encoding.Binary.to_bytes_exn
          Alpha_context.Block_header.unsigned_encoding
          (shell, contents) in
      Client_keys.append cctxt delegate_sk ~watermark:Block_header unsigned_header
    else
      loop () in
  loop ()


let empty_proof_of_work_nonce =
  MBytes.of_string
    (String.make Constants_repr.proof_of_work_nonce_size  '\000')


let forge_faked_protocol_data ~priority ~seed_nonce_hash =
  Alpha_context.Block_header.{
    contents = { priority ; seed_nonce_hash ;
                 proof_of_work_nonce = empty_proof_of_work_nonce } ;
    signature = Signature.zero
  }


let assert_valid_operations_hash shell_header operations =
  let operations_hash =
    Operation_list_list_hash.compute
      (List.map Operation_list_hash.compute
         (List.map
            (List.map Tezos_base.Operation.hash) operations)) in
  fail_unless
    (Operation_list_list_hash.equal
       operations_hash shell_header.Tezos_base.Block_header.operations_hash)
    (failure "Client_baking_forge.inject_block: inconsistent header.")


let inject_block cctxt
    ?force ?(chain = `Main)
    ~shell_header ~priority ?seed_nonce_hash ~src_sk operations =
  assert_valid_operations_hash shell_header operations >>=? fun () ->
  let block = `Hash (shell_header.Tezos_base.Block_header.predecessor, 0) in
  forge_block_header cctxt ~chain block
    src_sk shell_header priority seed_nonce_hash >>=? fun signed_header ->
  Shell_services.Injection.block cctxt
    ?force ~chain signed_header operations >>=? fun block_hash ->
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


let classify_operations (ops: Proto_alpha.operation list) =
  let t = Array.make (List.length Proto_alpha.Main.validation_passes) [] in
  List.iter
    (fun (op: Proto_alpha.operation) ->
       List.iter
         (fun pass -> t.(pass) <- op :: t.(pass))
         (Proto_alpha.Main.acceptable_passes op))
    ops ;
  Array.fold_right (fun ops acc -> List.rev ops :: acc) t []


let parse (op : Operation.raw) : Operation.packed =
  let protocol_data =
    Data_encoding.Binary.of_bytes_exn
      Alpha_context.Operation.protocol_data_encoding
      op.proto in
  {
    shell = op.shell ;
    protocol_data ;
  }

let forge (op : Operation.packed) : Operation.raw = {
  shell = op.shell ;
  proto =
    Data_encoding.Binary.to_bytes_exn
      Alpha_context.Operation.protocol_data_encoding
      op.protocol_data
}

let ops_of_mempool (ops : Alpha_block_services.Mempool.t) =
  List.map (fun (_, op) -> op) ops.applied @
  Operation_hash.Map.fold (fun _ (op, _) acc -> op :: acc) ops.refused [] @
  Operation_hash.Map.fold (fun _ (op, _) acc -> op :: acc) ops.branch_refused [] @
  Operation_hash.Map.fold (fun _ (op, _) acc -> op :: acc) ops.branch_delayed [] @
  Operation_hash.Map.fold (fun _ op acc -> op :: acc) ops.unprocessed []

let unopt_operations cctxt chain = function
  | None ->
      Alpha_block_services.Mempool.pending_operations cctxt ~chain () >>=? fun mpool ->
      let ops = ops_of_mempool mpool in
      return ops
  | Some operations ->
      return operations

let all_ops_valid (results: error Preapply_result.t list) =
  let open Operation_hash.Map in
  List.for_all (fun (result: error Preapply_result.t) ->
      is_empty result.refused
      && is_empty result.branch_refused
      && is_empty result.branch_delayed)
    results

let decode_priority cctxt chain block = function
  | `Set priority -> begin
      Alpha_services.Delegate.Baking_rights.get cctxt
        ~all:true ~max_priority:(priority+1) (chain, block) >>=? fun rights ->
      let time =
        Option.apply
          ~f:(fun r -> r.Alpha_services.Delegate.Baking_rights.timestamp)
          (List.nth_opt rights priority) in
      return (priority, time)
    end
  | `Auto (src_pkh, max_priority) ->
      Alpha_services.Helpers.current_level
        cctxt ~offset:1l (chain, block)>>=? fun { level } ->
      Alpha_services.Delegate.Baking_rights.get cctxt
        ?max_priority
        ~levels:[level]
        ~delegates:[src_pkh]
        (chain, block)  >>=? fun possibilities ->
      try
        let { Alpha_services.Delegate.Baking_rights.priority = prio ;
              timestamp = time } =
          List.find
            (fun p -> p.Alpha_services.Delegate.Baking_rights.level = level)
            possibilities in
        return (prio, time)
      with Not_found ->
        failwith "No slot found at level %a" Raw_level.pp level

let unopt_timestamp timestamp minimal_timestamp =
  match timestamp, minimal_timestamp with
  | None, None -> return (Time.now ())
  | None, Some timestamp -> return timestamp
  | Some timestamp, None -> return timestamp
  | Some timestamp, Some minimal_timestamp ->
      if timestamp < minimal_timestamp then
        failwith
          "Proposed timestamp %a is earlier than minimal timestamp %a"
          Time.pp_hum timestamp
          Time.pp_hum minimal_timestamp
      else
        return timestamp

let merge_preapps (old: error Preapply_result.t) (neu: error Preapply_result.t) =
  let merge _ a b = (* merge ops *)
    match a, b with
    | None, None -> None
    | Some x, None -> Some x
    | _, Some y -> Some y in
  let merge = Operation_hash.Map.merge merge in (* merge op maps *)
  (* merge preapplies *)
  { Preapply_result.applied = [] ;
    refused = merge old.refused neu.refused ;
    branch_refused = merge old.branch_refused neu.branch_refused ;
    branch_delayed = merge old.branch_delayed neu.branch_delayed }

let error_of_op (result: error Preapply_result.t) op =
  let op = forge op in
  let h = Tezos_base.Operation.hash op in
  try Some (Failed_to_preapply (op, snd @@ Operation_hash.Map.find h result.refused))
  with Not_found ->
  try Some (Failed_to_preapply (op, snd @@ Operation_hash.Map.find h result.branch_refused))
  with Not_found ->
  try Some (Failed_to_preapply (op, snd @@ Operation_hash.Map.find h result.branch_delayed))
  with Not_found -> None


let forge_block cctxt ?(chain = `Main) block
    ?force
    ?operations ?(best_effort = operations = None) ?(sort = best_effort)
    ?timestamp
    ~priority
    ?seed_nonce_hash ~src_sk () =

  (* making the arguments usable *)
  unopt_operations cctxt chain operations >>=? fun operations_arg ->
  decode_priority cctxt chain block priority >>=? fun (priority, minimal_timestamp) ->
  unopt_timestamp timestamp minimal_timestamp >>=? fun timestamp ->

  (* get basic building blocks *)
  let protocol_data = forge_faked_protocol_data ~priority ~seed_nonce_hash in
  let operations = classify_operations operations_arg in
  Alpha_block_services.Helpers.Preapply.block
    cctxt ~block ~timestamp ~sort ~protocol_data operations >>=? fun (shell_header, result) ->

  (* now for some logging *)
  let total_op_count = List.length operations_arg in
  let valid_op_count =
    List.fold_left
      (fun acc r -> acc + List.length r.Preapply_result.applied)
      0 result in
  lwt_log_info "Found %d valid operations (%d refused) for timestamp %a"
    valid_op_count (total_op_count - valid_op_count)
    Time.pp_hum timestamp >>= fun () ->
  lwt_log_info "Computed fitness %a"
    Fitness.pp shell_header.fitness >>= fun () ->

  (* everything went well (or we don't care about errors): GO! *)
  if best_effort || all_ops_valid result then
    let operations =
      if best_effort then
        List.map (fun l -> List.map snd l.Preapply_result.applied) result
      else
        List.map (List.map forge) operations in
    inject_block cctxt
      ?force ~chain ~shell_header ~priority ?seed_nonce_hash ~src_sk
      operations

  (* some errors (and we care about them) *)
  else
    let result = List.fold_left merge_preapps Preapply_result.empty result in
    Lwt.return_error @@
    List.filter_map (error_of_op result) (List.concat operations)

(** Worker *)

module State : sig
  (* TODO: only [record_block] is ever used, and only once. Simplify. *)

  val get_block:
    #Client_context.wallet ->
    Raw_level.t -> Block_hash.t list tzresult Lwt.t

  val record_block:
    #Client_context.wallet ->
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

  let name = "blocks"

  let load (wallet : #Client_context.wallet) =
    wallet#load name ~default:LevelMap.empty encoding

  let save (wallet : #Client_context.wallet) map =
    wallet#write name map encoding

  let lock = Lwt_mutex.create ()

  let get_block (cctxt : #Client_context.wallet) level =
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
    Client_baking_nonces.add cctxt hash nonce

end

let get_baking_slot cctxt
    ?max_priority (bi: Client_baking_blocks.block_info) delegates =
  let chain = `Hash bi.chain_id in
  let block = `Hash (bi.hash, 0) in
  let level = Raw_level.succ bi.level.level in
  Alpha_services.Delegate.Baking_rights.get cctxt
    ?max_priority
    ~levels:[level]
    ~delegates
    (chain, block) >>= function
  | Error errs ->
      log_error "Error while fetching baking possibilities:\n%a"
        pp_print_error errs ;
      Lwt.return_none
  | Ok [] ->
      Lwt.return_none
  | Ok ((slot : Alpha_services.Delegate.Baking_rights.t) :: _) ->
      match slot.timestamp with
      | None -> Lwt.return_none
      | Some timestamp ->
          Lwt.return_some (timestamp, (bi, slot.priority, slot.delegate))

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

let get_unrevealed_nonces
    (cctxt : #Proto_alpha.full) ?(force = false) ?(chain = `Main) block =
  Client_baking_blocks.blocks_from_current_cycle
    cctxt block ~offset:(-1l) () >>=? fun blocks ->
  filter_map_s (fun hash ->
      Client_baking_nonces.find cctxt hash >>=? function
      | None -> return None
      | Some nonce ->
          Alpha_block_services.metadata
            cctxt ~chain ~block:(`Hash (hash, 0)) () >>=? fun { protocol_data = { level } } ->
          if force then
            return (Some (hash, (level.level, nonce)))
          else
            Alpha_services.Nonce.get
              cctxt (chain, block) level.level >>=? function
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
    (cctxt : #Proto_alpha.full) ?max_priority state (bi: Client_baking_blocks.block_info) =
  begin
    safe_get_unrevealed_nonces cctxt (`Hash (bi.hash, 0)) >>= fun nonces ->
    Client_baking_revelation.forge_seed_nonce_revelation
      cctxt (`Hash (bi.hash, 0)) (List.map snd nonces)
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

let insert_blocks cctxt ?max_priority state bi =
  insert_block cctxt ?max_priority state bi >>= function
  | Ok () ->
      Lwt.return_unit
  | Error err ->
      lwt_log_error "Error: %a" pp_print_error err

let bake_slot
    cctxt
    state
    seed_nonce_hash
    (timestamp, (bi, priority, delegate)) (* baking slot *)
  =
  let chain = `Hash bi.Client_baking_blocks.chain_id in
  let block = `Hash (bi.hash, 0) in
  Alpha_services.Helpers.current_level cctxt
    ~offset:1l (chain, block) >>=? fun next_level ->
  let timestamp =
    if Block_hash.equal bi.Client_baking_blocks.hash state.genesis then
      Time.now ()
    else
      timestamp in
  Client_keys.Public_key_hash.name cctxt delegate >>=? fun name ->
  lwt_debug "Try baking after %a (slot %d) for %s (%a)"
    Block_hash.pp_short bi.hash
    priority
    name
    Time.pp_hum timestamp >>= fun () ->

  (* get and process operations *)
  Alpha_block_services.Mempool.pending_operations cctxt ~chain () >>=? fun mpool ->
  let operations = ops_of_mempool mpool in
  let total_op_count = List.length operations in
  let seed_nonce_hash =
    if next_level.expected_commitment then
      Some seed_nonce_hash
    else
      None in
  let protocol_data =
    forge_faked_protocol_data ~priority ~seed_nonce_hash in
  let operations = classify_operations operations in
  Alpha_block_services.Helpers.Preapply.block
    cctxt ~chain ~block
    ~timestamp ~sort:true ~protocol_data operations >>= function
  | Error errs ->
      lwt_log_error "Error while prevalidating operations:@\n%a"
        pp_print_error
        errs >>= fun () ->
      return None
  | Ok (shell_header, operations) ->
      lwt_debug
        "Computed candidate block after %a (slot %d): %a/%d fitness: %a"
        Block_hash.pp_short bi.hash priority
        (Format.pp_print_list
           ~pp_sep:(fun ppf () -> Format.fprintf ppf "+")
           (fun ppf operations -> Format.fprintf ppf "%d" (List.length operations.Preapply_result.applied)))
        operations
        total_op_count
        Fitness.pp shell_header.fitness >>= fun () ->
      let operations =
        List.map (fun l -> List.map snd l.Preapply_result.applied) operations in
      return
        (Some (bi, priority, shell_header, operations, delegate, seed_nonce_hash))

let fittest
    (_, _, (h1: Block_header.shell_header), _, _, _)
    (_, _, (h2: Block_header.shell_header), _, _, _) =
  match Fitness.compare h1.fitness h2.fitness with
  | 0 -> Time.compare h1.timestamp h2.timestamp
  | cmp -> ~- cmp

let fit_enough (state: state) (shell_header: Block_header.shell_header) =
  Fitness.compare state.best.fitness shell_header.fitness < 0
  || (Fitness.compare state.best.fitness shell_header.fitness = 0
      && Time.compare shell_header.timestamp state.best.timestamp < 0)

let record_nonce_hash cctxt level block_hash seed_nonce seed_nonce_hash =
  if seed_nonce_hash <> None then
    State.record_block cctxt level block_hash seed_nonce
    |> trace_exn (Failure "Error while recording block")
  else
    return ()

let pp_operation_list_list =
  Format.pp_print_list
    ~pp_sep:(fun ppf () -> Format.fprintf ppf "+")
    (fun ppf operations -> Format.fprintf ppf "%d" (List.length operations))

(* [bake] create a single block when woken up to do so. All the necessary
   information (e.g., slot) is available in the [state]. *)
let bake (cctxt : #Proto_alpha.full) state =
  let slots = pop_baking_slots state in
  let seed_nonce = generate_seed_nonce () in
  let seed_nonce_hash = Nonce.hash seed_nonce in

  (* baking for each slot *)
  filter_map_s (bake_slot cctxt state seed_nonce_hash) slots >>=? fun candidates ->

  (* selecting the candidate baked block *)
  let candidates = List.sort fittest candidates in
  match candidates with
  | (bi, priority, shell_header, operations, delegate, seed_nonce_hash) :: _
    when fit_enough state shell_header -> begin
      let level = Raw_level.succ bi.level.level in
      cctxt#message
        "Select candidate block after %a (slot %d) fitness: %a"
        Block_hash.pp_short bi.hash priority
        Fitness.pp shell_header.fitness >>= fun () ->

      (* core function *)
      Client_keys.get_key cctxt delegate >>=? fun (_,_,src_sk) ->
      let chain = `Hash bi.Client_baking_blocks.chain_id in
      inject_block cctxt
        ~force:true ~chain
        ~shell_header ~priority ?seed_nonce_hash ~src_sk
        operations
      (* /core function; back to logging and info *)

      |> trace_exn (Failure "Error while injecting block") >>=? fun block_hash ->
      record_nonce_hash cctxt level block_hash seed_nonce seed_nonce_hash >>=? fun () ->
      Client_keys.Public_key_hash.name cctxt delegate >>=? fun name ->
      cctxt#message
        "Injected block %a for %s after %a (level %a, slot %d, fitness %a, operations %a)"
        Block_hash.pp_short block_hash
        name
        Block_hash.pp_short bi.hash
        Raw_level.pp level priority
        Fitness.pp shell_header.fitness
        pp_operation_list_list operations >>= fun () ->
      return ()
    end

  | _ ->
      lwt_debug "No valid candidates." >>= fun () ->
      return ()


(* [create] starts the main loop of the baker. The loop monitors new blocks and
   starts individual baking operations when baking-slots are available to any of
   the [delegates] *)
let create
    (cctxt : #Proto_alpha.full)
    ?max_priority
    (delegates: public_key_hash list)
    (block_stream: Client_baking_blocks.block_info tzresult Lwt_stream.t)
    (bi: Client_baking_blocks.block_info) =

  Shell_services.Blocks.hash cctxt ~block:`Genesis () >>=? fun genesis_hash ->

  (* statefulness *)
  let last_get_block = ref None in
  let get_block () =
    match !last_get_block with
    | None ->
        let t = Lwt_stream.get block_stream in
        last_get_block := Some t ;
        t
    | Some t -> t in
  let state = create_state genesis_hash delegates bi in
  insert_blocks cctxt ?max_priority state bi >>= fun () ->

  (* main loop *)
  let rec worker_loop () =
    begin
      (* event construction *)
      let timeout = compute_timeout state in
      Lwt.choose [ (timeout >|= fun () -> `Timeout) ;
                   (get_block () >|= fun b -> `Hash b) ;
                 ] >>= function
        (* event matching *)
      | `Hash (None | Some (Error _)) ->
          (* return to restart *)
          Lwt.return_unit

      | `Hash (Some (Ok bi)) -> begin
          (* new block: cancel everything and bake on the new head *)
          Lwt.cancel timeout ;
          last_get_block := None ;
          lwt_debug
            "Discoverered block: %a"
            Block_hash.pp_short bi.Client_baking_blocks.hash >>= fun () ->
          insert_blocks cctxt ?max_priority state bi
        end

      | `Timeout ->
          (* main event: it's baking time *)
          lwt_debug "Waking up for baking..." >>= fun () ->
          begin
            (* core functionality *)
            bake cctxt state >>= function
            | Ok () -> Lwt.return_unit
            | Error errs -> lwt_log_error "Error while baking:@\n%a" pp_print_error errs
          end

    end >>= fun () ->
    (* and restart *)
    worker_loop () in

  (* ignition *)
  lwt_log_info "Starting baking daemon" >>= fun () ->
  worker_loop ()



(* Wrapper around previous [create] function that handles the case of
   unavailable blocks (empty block chain). *)
let create
    (cctxt : #Proto_alpha.full)
    ?max_priority
    (delegates: public_key_hash list)
    (block_stream: Client_baking_blocks.block_info tzresult Lwt_stream.t) =
  let rec wait_for_first_block () =
    Lwt_stream.get block_stream >>= function
    | None | Some (Error _) ->
        cctxt#message "Can't fetch the current block head. Retrying soon." >>= fun () ->
        (* NOTE: this is not a tight loop because of Lwt_stream.get *)
        wait_for_first_block ()
    | Some (Ok bi) ->
        create
          cctxt ?max_priority delegates
          block_stream bi
  in
  wait_for_first_block ()

