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


(* The index of the different components of the protocol's validation passes *)
(* TODO: ideally, we would like this to be more abstract and possibly part of
   the protocol, while retaining the generality of lists *)
let endorsements_index = 0
let votes_index = 1
let anonymous_index = 2
let managers_index = 3


type state = {
  genesis: Block_hash.t ;
  index : Context.index ;

  (* lazy-initialisation with retry-on-error *)
  delegates: public_key_hash list tzlazy ;
  constants: Constants.t tzlazy ;

  (* truly mutable *)
  mutable best: Client_baking_blocks.block_info ;
  mutable future_slots:
    (Time.t * (Client_baking_blocks.block_info * int * public_key_hash)) list ;
}

let create_state genesis index delegates constants best =
  { genesis ;
    index ;
    delegates ;
    constants ;
    best ;
    future_slots = [] ;
  }

let generate_seed_nonce () =
  match Nonce.of_bytes @@
    Rand.generate Constants.nonce_length with
  | Error _errs -> assert false
  | Ok nonce -> nonce

let forge_block_header
    (cctxt : #Proto_alpha.full)
    ?(chain = `Main) block delegate_sk shell priority seed_nonce_hash =
  Client_baking_pow.mine
    cctxt chain block shell
    (fun proof_of_work_nonce ->
       { Block_header.priority ;
         seed_nonce_hash ;
         proof_of_work_nonce ;
       }) >>=? fun contents ->
  let unsigned_header =
    Data_encoding.Binary.to_bytes_exn
      Alpha_context.Block_header.unsigned_encoding
      (shell, contents) in
  Client_keys.append cctxt delegate_sk ~watermark:Block_header unsigned_header

let forge_faked_protocol_data ~priority ~seed_nonce_hash =
  Alpha_context.Block_header.{
    contents = { priority ; seed_nonce_hash ;
                 proof_of_work_nonce = Client_baking_pow.empty_proof_of_work_nonce } ;
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

let get_operation_fee op =
  let { protocol_data = Operation_data { contents } ; _ } = op in
  let open Operation in
  let l = to_list (Contents_list contents) in
  fold_left_s (fun total_fee -> function
      | Contents (Manager_operation { fee ; _ })
        when Tez.(fee > zero) ->
          Lwt.return @@ Alpha_environment.wrap_error @@
          Tez.(total_fee +? fee)
      | _ -> return total_fee) Tez.zero l

let sort_operations_by_fee ?(threshold = Tez.zero) (operations : Proto_alpha.operation list) =
  filter_map_s
    (fun op ->
       get_operation_fee op >>=? fun fee ->
       if Tez.(<) fee threshold then
         return None
       else
         return (Some (op, fee)))
    operations >>=? fun operations ->
  let compare_fee (_, fee1) (_, fee2) =
    (* NOTE: inverted fee comparison to invert the order of sort *)
    Tez.compare fee2 fee1 in
  return @@ List.map fst (List.sort compare_fee operations)

let retain_operations_up_to_quota operations max_quota =
  let exception Full of packed_operation list in
  let operations = try
      List.fold_left (fun (ops, size) op ->
          let operation_size =
            Data_encoding.Binary.length Alpha_context.Operation.encoding op
          in
          let new_size = size + operation_size in
          if new_size > max_quota then
            raise (Full ops)
          else
            (op :: ops, new_size)
        ) ([], 0) operations |> fst
    with
    | Full ops -> ops in
  List.rev operations

let classify_operations ?threshold (ops: Proto_alpha.operation list) =
  let t = Array.make (List.length Proto_alpha.Main.validation_passes) [] in
  List.iter
    (fun (op: Proto_alpha.operation) ->
       List.iter
         (fun pass -> t.(pass) <- op :: t.(pass))
         (Proto_alpha.Main.acceptable_passes op))
    ops ;
  let t = Array.map List.rev t in
  (* Retrieve the maximum paying manager operations *)
  let manager_operations = t.(managers_index) in
  let { Alpha_environment.Updater.max_size } =
    List.nth Proto_alpha.Main.validation_passes managers_index in
  sort_operations_by_fee ?threshold manager_operations >>=? fun ordered_operations ->
  let max_operations =
    retain_operations_up_to_quota ordered_operations max_size
  in
  (* TODO ? : should preserve mempool order  *)
  t.(managers_index) <- max_operations;
  return @@ Array.fold_right (fun ops acc -> ops :: acc) t []

let parse (op : Operation.raw) : Operation.packed =
  let protocol_data =
    Data_encoding.Binary.of_bytes_exn
      Alpha_context.Operation.protocol_data_encoding
      op.proto in
  { shell = op.shell ;
    protocol_data ;
  }

let forge (op : Operation.packed) : Operation.raw =
  { shell = op.shell ;
    proto = Data_encoding.Binary.to_bytes_exn
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
    ?threshold
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
  classify_operations ?threshold operations_arg >>=? fun operations ->
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

module State = Daemon_state.Make(struct let name = "block" end)

let previously_baked_level cctxt pkh new_lvl  =
  State.get cctxt pkh  >>=? function
  | None -> return false
  | Some last_lvl ->
      return (Raw_level.(last_lvl >= new_lvl))

let get_baking_slot cctxt
    ?max_priority (bi: Client_baking_blocks.block_info) delegates =
  let chain = `Hash bi.chain_id in
  let block = `Hash (bi.hash, 0) in
  let level = Raw_level.succ bi.level in
  Alpha_services.Delegate.Baking_rights.get cctxt
    ?max_priority
    ~levels:[level]
    ~delegates
    (chain, block) >>= function
  | Error errs ->
      lwt_log_error "Error while fetching baking possibilities:\n%a"
        pp_print_error errs >>= fun () ->
      Lwt.return []
  | Ok [] ->
      lwt_log_info "Found no baking rights for level %a"
        Raw_level.pp level >>= fun () ->
      Lwt.return []
  | Ok slots ->
      let slots =
        List.filter_map
          (function
            | { Alpha_services.Delegate.Baking_rights.timestamp = None } -> None
            | { timestamp = Some timestamp ; priority ; delegate } ->
                Some (timestamp, (bi, priority, delegate))
          )
          slots
      in
      Lwt.return slots

let rec insert_baking_slot slot = function
  (* This is just a sorted-insert *)
  | [] -> [slot]
  | ((timestamp,_) :: _) as slots when Time.(fst slot < timestamp) ->
      slot :: slots
  | slot' :: slots -> slot' :: insert_baking_slot slot slots

let drop_old_slots ~before state =
  state.future_slots <-
    List.filter
      (fun (t, _slot) -> Time.compare before t <= 0)
      state.future_slots

let compute_timeout { future_slots } =
  match future_slots with
  | [] ->
      (* No slots, just wait for new blocks which will give more info *)
      Lwt_utils.never_ending ()
  | (timestamp, _) :: _ ->
      match Client_baking_scheduling.sleep_until timestamp with
      | None ->
          Lwt.return ()
      | Some timeout ->
          timeout

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



let insert_block
    ?max_priority
    ()
    (cctxt: #Proto_alpha.full)
    state
    (bi: Client_baking_blocks.block_info) =
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
  tzforce state.delegates >>=? fun delegates ->
  get_baking_slot cctxt ?max_priority bi delegates >>= function
  | [] ->
      lwt_debug
        "Can't compute slots for %a" Block_hash.pp_short bi.hash >>= fun () ->
      return ()
  | (_ :: _) as slots ->
      iter_p
        (fun ((timestamp, (_, _, delegate)) as slot) ->
           Client_keys.Public_key_hash.name cctxt delegate >>=? fun name ->
           lwt_log_info "New baking slot at %a for %s after %a"
             Time.pp_hum timestamp
             name
             Block_hash.pp_short bi.hash >>= fun () ->
           state.future_slots <- insert_baking_slot slot state.future_slots ;
           return ()
        )
        slots

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


let filter_invalid_operations (cctxt : #full) state block_info (operations : packed_operation list list) =
  let open Client_baking_simulator in
  lwt_debug "Starting client-side validation %a"
    Block_hash.pp block_info.Client_baking_blocks.hash >>= fun () ->
  begin_construction cctxt state.index block_info >>=? fun initial_inc ->
  let endorsements = List.nth operations endorsements_index in
  let votes = List.nth operations votes_index in
  let anonymous = List.nth operations anonymous_index in
  let managers = List.nth operations managers_index in
  let validate_operation inc op =
    add_operation inc op >>= function
    | Error errs ->
        lwt_log_info "Client-side validation: invalid operation filtered %a\n%a"
          Operation_hash.pp (Operation.hash_packed op)
          pp_print_error errs
        >>= fun () ->
        return None
    | Ok inc -> return (Some inc)
  in
  let filter_valid_operations inc ops =
    fold_left_s (fun (inc, acc) op ->
        validate_operation inc op >>=? function
        | None -> return (inc, acc)
        | Some inc' -> return (inc', op :: acc)
      ) (inc, []) ops
  in
  (* Invalid endorsements are detected during block finalization *)
  let is_valid_endorsement inc endorsement =
    validate_operation inc endorsement >>=? function
    | None -> return None
    | Some inc' -> finalize_construction inc' >>= begin function
        | Ok _ -> return (Some endorsement)
        | Error _ -> return None
      end
  in
  filter_valid_operations initial_inc votes >>=? fun (inc, votes) ->
  filter_valid_operations inc anonymous >>=? fun (inc, anonymous) ->
  filter_valid_operations inc managers >>=? fun (inc, managers) ->
  (* Gives a chance to the endorser to fund their deposit in the current block *)
  filter_map_s (is_valid_endorsement inc) endorsements >>=? fun endorsements ->
  finalize_construction inc >>= function
  | Error errs ->
      lwt_log_error "Client-side validation: invalid block built. Building an empty block...\n%a"
        pp_print_error errs >>= fun () ->
      return [ [] ; [] ; [] ; [] ]
  | Ok () ->
      let quota : Alpha_environment.Updater.quota list = Main.validation_passes in
      (* This shouldn't happen *)
      tzforce state.constants >>=? fun constants ->
      let endorsements =
        List.sub (List.rev endorsements) constants.Constants.parametric.endorsers_per_block
      in
      let votes =
        retain_operations_up_to_quota
          (List.rev votes)
          (List.nth quota votes_index).max_size in
      let anonymous =
        retain_operations_up_to_quota
          (List.rev anonymous)
          (List.nth quota anonymous_index).max_size in
      (* manager operations size check already occured in classify operations *)
      return @@ List.map List.rev [ endorsements ; votes ; anonymous ; managers ]

let bake_slot
    cctxt
    state
    ?threshold
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
  classify_operations ?threshold operations >>=? fun operations ->
  begin
    (* Don't load an alpha context if the chain is still in genesis *)
    if Protocol_hash.(bi.protocol = bi.next_protocol) then
      filter_invalid_operations cctxt state bi operations
    else
      return operations
  end >>= function
  | Error errs ->
      lwt_log_error "Client-side validation: error while filtering invalid operations :@\n%a"
        pp_print_error
        errs >>= fun () ->
      return None
  | Ok operations ->
      Tezos_stdlib_unix.Lwt_utils_unix.retry
        ~log:(fun errs ->
            lwt_log_error
              "Error while prevalidating operations\n%a\nRetrying..."
              pp_print_error errs
          )
        (fun () ->
           Alpha_block_services.Helpers.Preapply.block
             cctxt ~chain ~block
             ~timestamp ~sort:true ~protocol_data operations)
      >>= function
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
               (fun ppf operations -> Format.fprintf ppf "%d"
                   (List.length operations.Preapply_result.applied)))
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

let record_nonce_hash cctxt block_hash seed_nonce seed_nonce_hash =
  if seed_nonce_hash <> None then
    Client_baking_nonces.add cctxt block_hash seed_nonce
    |> trace_exn (Failure "Error while recording block")
  else
    return ()

let pp_operation_list_list =
  Format.pp_print_list
    ~pp_sep:(fun ppf () -> Format.fprintf ppf "+")
    (fun ppf operations -> Format.fprintf ppf "%d" (List.length operations))

(* [bake] create a single block when woken up to do so. All the necessary
   information (e.g., slot) is available in the [state]. *)
let bake
    ?threshold
    ()
    (cctxt : #Proto_alpha.full)
    state
    () =
  let slots = pop_baking_slots state in
  lwt_log_info "Found %d current slots and %d future slots."
    (List.length slots)
    (List.length state.future_slots) >>= fun () ->
  let seed_nonce = generate_seed_nonce () in
  let seed_nonce_hash = Nonce.hash seed_nonce in

  (* baking for each slot *)
  filter_map_s
    (bake_slot cctxt ?threshold state seed_nonce_hash)
    slots >>=? fun candidates ->

  (* FIXME: pick one block per-delegate *)
  (* selecting the candidate baked block *)
  let candidates = List.sort fittest candidates in
  match candidates with
  | (bi, priority, shell_header, operations, delegate, seed_nonce_hash) :: _
    when fit_enough state shell_header -> begin
      let level = Raw_level.succ bi.level in
      cctxt#message
        "Select candidate block after %a (slot %d) fitness: %a"
        Block_hash.pp_short bi.hash priority
        Fitness.pp shell_header.fitness >>= fun () ->

      (* core function *)
      Client_keys.get_key cctxt delegate >>=? fun (_,src_pk,src_sk) ->
      let src_pkh = Signature.Public_key.hash src_pk in
      let chain = `Hash bi.Client_baking_blocks.chain_id in

      (* avoid double baking *)
      previously_baked_level cctxt src_pkh level >>=? function
      | true ->  lwt_log_error "Level %a : previously baked"
                   Raw_level.pp level  >>= return
      | false ->

          inject_block cctxt
            ~force:true ~chain
            ~shell_header ~priority ?seed_nonce_hash ~src_sk
            operations
          (* /core function; back to logging and info *)

          |> trace_exn (Failure "Error while injecting block") >>=? fun block_hash ->
          State.record cctxt src_pkh level >>=? fun () ->
          record_nonce_hash cctxt block_hash seed_nonce seed_nonce_hash >>=? fun () ->
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

  | _ -> (* no candidates, or none fit-enough *)
      lwt_debug "No valid candidates." >>= fun () ->
      return ()



(* [create] starts the main loop of the baker. The loop monitors new blocks and
   starts individual baking operations when baking-slots are available to any of
   the [delegates] *)
let create
    (cctxt : #Proto_alpha.full)
    ?threshold
    ?max_priority
    ~(context_path: string)
    (delegates: public_key_hash list)
    (block_stream: Client_baking_blocks.block_info tzresult Lwt_stream.t)
  =

  let state_maker genesis_hash bi =
    let delegates = match delegates with
      | [] ->
          tzlazy (fun () ->
              Client_keys.get_keys cctxt >>=? fun keys ->
              let delegates = List.map (fun (_,pkh,_,_) -> pkh) keys in
              return delegates
            )
      | _ :: _ -> tzlazy (fun () -> return delegates) in
    let constants =
      tzlazy (fun () -> Alpha_services.Constants.all cctxt (`Main, `Head 0)) in
    Client_baking_simulator.load_context ~context_path >>= fun index ->
    let state = create_state genesis_hash index delegates constants bi in
    return state
  in

  Client_baking_scheduling.main
    ~name:"baker"
    ~cctxt
    ~stream:block_stream
    ~state_maker
    ~pre_loop:(insert_block ?max_priority ())
    ~compute_timeout
    ~timeout_k:(bake ?threshold ())
    ~event_k:(insert_block ?max_priority ())

