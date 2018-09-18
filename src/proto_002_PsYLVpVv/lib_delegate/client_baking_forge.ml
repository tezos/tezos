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

open Proto_alpha
open Alpha_context

include Tezos_stdlib.Logging.Make_semantic(struct let name = "client.baking" end)
open Logging

(* The index of the different components of the protocol's validation passes *)
(* TODO: ideally, we would like this to be more abstract and possibly part of
   the protocol, while retaining the generality of lists *)
let endorsements_index = 0
let votes_index = 1
let anonymous_index = 2
let managers_index = 3

type state = {
  genesis: Block_hash.t ;
  context_path: string ;
  mutable index : Context.index ;

  (* see [get_delegates] below to find delegates when the list is empty *)
  delegates: public_key_hash list ;

  (* lazy-initialisation with retry-on-error *)
  constants: Constants.t tzlazy ;

  (* truly mutable *)
  mutable best: Client_baking_blocks.block_info ;
  mutable future_slots:
    (Time.t * (Client_baking_blocks.block_info * int * public_key_hash)) list ;
}

let create_state genesis context_path index delegates constants best =
  { genesis ;
    context_path ;
    index ;
    delegates ;
    constants ;
    best ;
    future_slots = [] ;
  }

let get_delegates cctxt state = match state.delegates with
  | [] ->
      Client_keys.get_keys cctxt >>=? fun keys ->
      let delegates = List.map (fun (_,pkh,_,_) -> pkh) keys in
      return delegates
  | (_ :: _) as delegates -> return delegates

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
  Shell_services.Chain.chain_id cctxt ~chain () >>=? fun chain_id ->
  Client_keys.append cctxt delegate_sk ~watermark:(Block_header chain_id) unsigned_header

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

let get_manager_operation_gas_and_fee op =
  let { protocol_data = Operation_data { contents } ; _ } = op in
  let open Operation in
  let l = to_list (Contents_list contents) in
  fold_left_s (fun ((total_fee, total_gas) as acc) -> function
      | Contents (Manager_operation { fee ; gas_limit ; _ }) ->
          Lwt.return @@ Alpha_environment.wrap_error @@
          Tez.(total_fee +? fee) >>=? fun total_fee ->
          return (total_fee, (Z.add total_gas gas_limit))
      | _ -> return acc) (Tez.zero, Z.zero) l

(* Sort operation consisdering potential gas and storage usage.
   Weight = fee / (max ( (size/size_total), (gas/gas_total))) *)
let sort_manager_operations
    ~max_size
    ~hard_gas_limit_per_block
    ?(threshold = Tez.zero)
    (operations : Proto_alpha.operation list)
  =
  let compute_weight op (fee, gas) =
    let size = Data_encoding.Binary.length Operation.encoding op in
    let size_f = Q.of_int size in
    let gas_f = Q.of_bigint gas in
    let fee_f = Q.of_int64 (Tez.to_mutez fee) in
    let size_ratio = Q.(size_f / (Q.of_int max_size)) in
    let gas_ratio = Q.(gas_f / (Q.of_bigint hard_gas_limit_per_block)) in
    (size, gas, Q.(fee_f / (max size_ratio gas_ratio)))
  in
  filter_map_s
    (fun op ->
       get_manager_operation_gas_and_fee op >>=? fun (fee, gas) ->
       if Tez.(<) fee threshold then
         return_none
       else
         return (Some (op, (compute_weight op (fee, gas))))
    ) operations >>=? fun operations ->
  (* We sort by the biggest weight *)
  return
    (List.sort (fun (_, (_, _, w)) (_, (_, _, w')) -> Q.compare w' w) operations)

let retain_operations_up_to_quota operations quota =
  let { T.max_op ; max_size } = quota in
  let operations = match max_op with
    | Some n -> List.sub operations n
    | None -> operations
  in
  let exception Full of packed_operation list in
  let operations = try
      List.fold_left (fun (ops, size) op ->
          let operation_size =
            Data_encoding.Binary.length Alpha_context.Operation.encoding op
          in
          let new_size = size + operation_size in
          if new_size > max_size then
            raise (Full ops)
          else
            (op :: ops, new_size)
        ) ([], 0) operations |> fst
    with
    | Full ops -> ops in
  List.rev operations

let trim_manager_operations ~max_size ~hard_gas_limit_per_block manager_operations =
  map_s (fun op ->
      get_manager_operation_gas_and_fee op >>=? fun (_fee, gas) ->
      let size = Data_encoding.Binary.length Operation.encoding op in
      return (op, (size, gas))) manager_operations >>=? fun manager_operations ->
  List.fold_left
    (fun (total_size, total_gas, (good_ops, bad_ops)) (op, (size, gas)) ->
       let new_size = total_size + size in
       let new_gas = Z.(total_gas + gas) in
       if new_size > max_size || (Z.gt new_gas hard_gas_limit_per_block) then
         (new_size, new_gas, (good_ops, op :: bad_ops))
       else
         (new_size, new_gas, (op :: good_ops, bad_ops))
    ) (0, Z.zero, ([], [])) manager_operations |> fun (_, _, (good_ops, bad_ops)) ->
  (* We keep the overflowing operations, it may be used for client-side validation *)
  return ((List.rev good_ops), (List.rev bad_ops))

(* We classify operations, sort managers operation by interest and add bad ones at the end *)
(* Hypothesis : we suppose that the received manager operations have a valid gas_limit *)
let classify_operations
    (cctxt : #Proto_alpha.full)
    ?threshold
    ~block
    ~hard_gas_limit_per_block
    (ops: Proto_alpha.operation list) =
  Alpha_block_services.live_blocks cctxt ~chain:`Main ~block ()
  >>=? fun live_blocks ->
  (* Remove operations that are too old for the mempool *)
  let ops =
    List.filter (fun { shell = { branch } } ->
        Block_hash.Set.mem branch live_blocks
      ) ops
  in
  let t = Array.make (List.length Proto_alpha.Main.validation_passes) [] in
  List.iter
    (fun (op: Proto_alpha.operation) ->
       List.iter
         (fun pass -> t.(pass) <- op :: t.(pass))
         (Main.acceptable_passes op))
    ops ;
  let t = Array.map List.rev t in

  (* Retrieve the optimist maximum paying manager operations *)
  let manager_operations = t.(managers_index) in
  let { Alpha_environment.Updater.max_size } =
    List.nth Proto_alpha.Main.validation_passes managers_index in
  sort_manager_operations ~max_size ~hard_gas_limit_per_block ?threshold manager_operations
  >>=? fun ordered_operations ->
  (* Greedy heuristic *)
  trim_manager_operations ~max_size ~hard_gas_limit_per_block (List.map fst ordered_operations)
  >>=? fun (desired_manager_operations, overflowing_manager_operations) ->
  t.(managers_index) <- desired_manager_operations ;
  return @@ (Array.fold_right (fun ops acc -> ops :: acc) t [ overflowing_manager_operations ])

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
  (* We only retain the applied, unprocessed and delayed operations *)
  List.rev (
    Operation_hash.Map.fold (fun _ op acc -> op :: acc) ops.unprocessed @@
    Operation_hash.Map.fold (fun _ (op, _) acc -> op :: acc) ops.branch_delayed @@
    List.rev_map (fun (_, op) -> op) ops.applied
  )

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
  Alpha_services.Constants.all cctxt (`Main, block) >>=?
  fun Constants.{ parametric = { hard_gas_limit_per_block ; endorsers_per_block } } ->
  classify_operations cctxt ~hard_gas_limit_per_block ~block:block ?threshold operations_arg >>=? fun operations ->
  (* Ensure that we retain operations up to the quota *)
  let quota : Alpha_environment.Updater.quota list = Main.validation_passes in
  let endorsements = List.sub
      (List.nth operations endorsements_index)
      endorsers_per_block in
  let votes = retain_operations_up_to_quota
      (List.nth operations votes_index)
      (List.nth quota votes_index) in
  let anonymous =
    retain_operations_up_to_quota
      (List.nth operations anonymous_index)
      (List.nth quota anonymous_index) in
  (* Size/Gas check already occured in classify operations *)
  let managers = List.nth operations managers_index in
  let operations = [ endorsements ; votes ; anonymous ; managers ] in
  Alpha_block_services.Helpers.Preapply.block
    cctxt ~block ~timestamp ~sort ~protocol_data operations >>=? fun (shell_header, result) ->

  (* Now for some logging *)
  let total_op_count = List.length operations_arg in
  let valid_op_count =
    List.fold_left
      (fun acc r -> acc + List.length r.Preapply_result.applied)
      0 result in
  lwt_log_info Tag.DSL.(fun f ->
      f "Found %d valid operations (%d refused) for timestamp %a@.Computed fitness %a"
      -% t event "found_valid_operations"
      -% s valid_ops valid_op_count
      -% s refused_ops (total_op_count - valid_op_count)
      -% a timestamp_tag timestamp
      -% a fitness_tag shell_header.fitness) >>= fun () ->

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
  | None -> return_false
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
      lwt_log_error Tag.DSL.(fun f ->
          f "Error while fetching baking possibilities:\n%a"
          -% t event "baking_slot_fetch_errors"
          -% a errs_tag errs) >>= fun () ->
      Lwt.return_nil
  | Ok [] ->
      lwt_log_info Tag.DSL.(fun f ->
          f "Found no baking rights for level %a"
          -% t event "no_baking_rights"
          -% a level_tag level) >>= fun () ->
      Lwt.return_nil
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
          Lwt.return_unit
      | Some timeout ->
          timeout

let get_unrevealed_nonces
    (cctxt : #Proto_alpha.full) ?(force = false) ?(chain = `Main) block =
  Client_baking_blocks.blocks_from_current_cycle
    cctxt block ~offset:(-1l) () >>=? fun blocks ->
  filter_map_s (fun hash ->
      Client_baking_nonces.find cctxt hash >>=? function
      | None -> return_none
      | Some nonce ->
          Alpha_block_services.metadata
            cctxt ~chain ~block:(`Hash (hash, 0)) () >>=? fun { protocol_data = { level } } ->
          if force then
            return_some (hash, (level.level, nonce))
          else
            Alpha_services.Nonce.get
              cctxt (chain, block) level.level >>=? function
            | Missing nonce_hash
              when Nonce.check_hash nonce nonce_hash ->
                cctxt#warning "Found nonce for %a (level: %a)@."
                  Block_hash.pp_short hash
                  Level.pp level >>= fun () ->
                return_some (hash, (level.level, nonce))
            | Missing _nonce_hash ->
                cctxt#error "Incoherent nonce for level %a"
                  Raw_level.pp level.level >>= fun () ->
                return_none
            | Forgotten -> return_none
            | Revealed _ -> return_none)
    blocks

let safe_get_unrevealed_nonces cctxt block =
  get_unrevealed_nonces cctxt block >>= function
  | Ok r -> Lwt.return r
  | Error err ->
      lwt_warn Tag.DSL.(fun f ->
          f "Cannot read nonces: %a@."
          -% t event "read_nonce_fail"
          -% a errs_tag err)
      >>= fun () ->
      Lwt.return_nil

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
  get_delegates cctxt state >>=? fun delegates ->
  get_baking_slot cctxt ?max_priority bi delegates >>= function
  | [] ->
      lwt_debug
        Tag.DSL.(fun f ->
            f "Can't compute slots for %a"
            -% t event "cannot_compute_slot"
            -% a Block_hash.Logging.tag bi.hash) >>= fun () ->
      return_unit
  | (_ :: _) as slots ->
      iter_p
        (fun ((timestamp, (_, _, delegate)) as slot) ->
           Client_keys.Public_key_hash.name cctxt delegate >>=? fun name ->
           lwt_log_info Tag.DSL.(fun f ->
               f "New baking slot at %a for %s after %a"
               -% t event "have_baking_slot"
               -% a timestamp_tag timestamp
               -% s Client_keys.Logging.tag name
               -% a Block_hash.Logging.tag bi.hash
               -% t Signature.Public_key_hash.Logging.tag delegate) >>= fun () ->
           state.future_slots <- insert_baking_slot slot state.future_slots ;
           return_unit
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

let filter_and_apply_operations
    state
    block_info
    ~timestamp
    ?protocol_data
    (operations : packed_operation list list) =
  let open Client_baking_simulator in
  lwt_debug Tag.DSL.(fun f ->
      f "Starting client-side validation %a"
      -% t event "baking_local_validation_start"
      -% a Block_hash.Logging.tag block_info.Client_baking_blocks.hash) >>= fun () ->
  begin begin_construction ~timestamp ?protocol_data state.index block_info >>= function
    | Ok inc -> return inc
    | Error errs ->
        lwt_log_error Tag.DSL.(fun f ->
            f "Error while fetching current context : %a"
            -% t event "context_fetch_error"
            -% a errs_tag errs) >>= fun () ->
        lwt_log_notice Tag.DSL.(fun f -> f "Retrying to open the context" -% t event "reopen_context") >>= fun () ->
        Client_baking_simulator.load_context ~context_path:state.context_path >>= fun index ->
        begin_construction ~timestamp ?protocol_data index block_info >>=? fun inc ->
        state.index <- index ;
        return inc
  end  >>=? fun initial_inc ->
  let endorsements = List.nth operations endorsements_index in
  let votes = List.nth operations votes_index in
  let anonymous = List.nth operations anonymous_index in
  let managers = List.nth operations managers_index in
  let bad_managers =
    if List.length operations > managers_index + 1 then
      List.nth operations (managers_index + 1)
    else []
  in
  let validate_operation inc op =
    add_operation inc op >>= function
    | Error errs ->
        lwt_log_info Tag.DSL.(fun f ->
            f "Client-side validation: invalid operation filtered %a\n%a"
            -% t event "baking_rejected_invalid_operation"
            -% a Operation_hash.Logging.tag (Operation.hash_packed op)
            -% a errs_tag errs)
        >>= fun () ->
        return_none
    | Ok inc -> return_some inc
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
    | None -> return_none
    | Some inc' -> finalize_construction inc' >>= begin function
        | Ok _ -> return_some endorsement
        | Error _ -> return_none
      end
  in
  filter_valid_operations initial_inc votes >>=? fun (inc, votes) ->
  filter_valid_operations inc anonymous >>=? fun (inc, anonymous) ->
  (* Retrieve the correct index order *)
  let managers = List.sort Proto_alpha.compare_operations managers in
  let bad_managers = List.sort Proto_alpha.compare_operations bad_managers in
  filter_valid_operations inc (managers @ bad_managers) >>=? fun (inc, managers) ->
  (* Gives a chance to the endorser to fund their deposit in the current block *)
  filter_map_s (is_valid_endorsement inc) endorsements >>=? fun endorsements ->
  finalize_construction inc >>=? fun _ ->
  let quota : Alpha_environment.Updater.quota list = Main.validation_passes in
  tzforce state.constants >>=? fun
    { Constants.parametric = { endorsers_per_block ; hard_gas_limit_per_block ; } } ->
  let endorsements =
    List.sub (List.rev endorsements) endorsers_per_block
  in
  let votes =
    retain_operations_up_to_quota
      (List.rev votes)
      (List.nth quota votes_index) in
  let anonymous =
    retain_operations_up_to_quota
      (List.rev anonymous)
      (List.nth quota anonymous_index) in
  let is_evidence  = function
    | { protocol_data = Operation_data { contents = Single (Double_baking_evidence _ ) } } -> true
    | { protocol_data = Operation_data { contents = Single (Double_endorsement_evidence _ ) } } -> true
    | _ -> false in
  let evidences, anonymous = List.partition is_evidence anonymous in
  trim_manager_operations ~max_size:(List.nth quota managers_index).max_size
    ~hard_gas_limit_per_block managers >>=? fun (accepted_managers, _overflowing_managers) ->
  (* Retrieve the correct index order *)
  let accepted_managers = List.sort Proto_alpha.compare_operations accepted_managers in
  (* Make sure we only keep valid operations *)
  filter_valid_operations initial_inc votes >>=? fun (inc, votes) ->
  filter_valid_operations inc anonymous >>=? fun (inc, anonymous) ->
  filter_valid_operations inc accepted_managers >>=? fun (inc, accepted_managers) ->
  filter_map_s (is_valid_endorsement inc) endorsements >>=? fun endorsements ->
  (* Endorsements won't fail now *)
  fold_left_s add_operation inc endorsements >>=? fun inc ->
  (* Endorsement and double baking/endorsement evidence do not commute:
     we apply denunciation operations after endorsements. *)
  filter_valid_operations inc evidences >>=? fun (final_inc, evidences) ->
  let operations = List.map List.rev [ endorsements ; votes ; anonymous @ evidences ; accepted_managers ] in
  finalize_construction final_inc >>=? fun (validation_result, metadata) ->
  return @@ (final_inc, (validation_result, metadata), operations)

(* Build the block header : mimics node prevalidation *)
let finalize_block_header
    (inc : Client_baking_simulator.incremental)
    ~timestamp
    (validation_result, _metadata)
    operations =
  let { T.context ; fitness ; message ; _ } = validation_result in
  let validation_passes = List.length LiftedMain.validation_passes in
  let operations_hash : Operation_list_list_hash.t = 
    Operation_list_list_hash.compute
      (List.map
         (fun sl ->
            Operation_list_hash.compute
              (List.map Operation.hash_packed sl)
         ) operations
      ) in
  Context.hash ~time:timestamp ?message context >>= fun context ->
  let header =
    { inc.header with
      level = Raw_level.to_int32 (Raw_level.succ inc.predecessor.level) ;
      validation_passes ;
      operations_hash ;
      fitness ;
      context ;
    } in
  return header

let shell_prevalidation
    (cctxt : #Proto_alpha.full)
    ~chain
    ~block
    seed_nonce_hash
    operations 
    (timestamp, (bi, priority, delegate)) =
  let protocol_data =
    forge_faked_protocol_data ~priority ~seed_nonce_hash in
  Alpha_block_services.Helpers.Preapply.block
    cctxt ~chain ~block
    ~timestamp ~sort:true ~protocol_data operations
  >>= function
  | Error errs ->
      lwt_log_error Tag.DSL.(fun f ->
          f "Shell-side validation: error while prevalidating operations:@\n%a"
          -% t event "built_invalid_block_error"
          -% a errs_tag errs) >>= fun () ->
      return None
  | Ok (shell_header, operations) ->
      let raw_ops =
        List.map (fun l ->
            List.map snd l.Preapply_result.applied) operations in
      return
        (Some (bi, priority, shell_header, raw_ops, delegate, seed_nonce_hash))

let bake_slot
    cctxt
    state
    ?threshold
    seed_nonce_hash
    ((timestamp, (bi, priority, delegate)) as slot)
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
  lwt_debug Tag.DSL.(fun f ->
      f "Try baking after %a (slot %d) for %s (%a)"
      -% t event "try_baking"
      -% a Block_hash.Logging.tag bi.hash
      -% s bake_priorty_tag priority
      -% s Client_keys.Logging.tag name
      -% a timestamp_tag timestamp) >>= fun () ->
  (* Retrieve pending operations *)
  Alpha_block_services.Mempool.pending_operations cctxt ~chain () >>=? fun mpool ->
  let operations = ops_of_mempool mpool in
  let seed_nonce_hash =
    if next_level.expected_commitment then
      Some seed_nonce_hash
    else
      None in
  tzforce state.constants >>=? fun Constants.{ parametric = { hard_gas_limit_per_block } } ->
  classify_operations cctxt ?threshold ~hard_gas_limit_per_block ~block operations >>=? fun operations ->
  let next_version =
    match Tezos_base.Block_header.get_forced_protocol_upgrade ~level:(Raw_level.to_int32 next_level.Level.level) with
    | None -> bi.next_protocol
    | Some hash -> hash
  in
  if Protocol_hash.(Proto_alpha.hash <> next_version) then
    (* Delegate validation to shell *)
    shell_prevalidation cctxt ~chain ~block seed_nonce_hash
      (List.sub operations 4) slot
  else
    let protocol_data = forge_faked_protocol_data ~priority ~seed_nonce_hash in
    filter_and_apply_operations ~timestamp ~protocol_data state bi operations >>= function
    | Error errs ->
        lwt_log_error Tag.DSL.(fun f ->
            f "Client-side validation: error while filtering invalid operations :@\n%a"
            -% t event "client_side_validation_error"
            -% a errs_tag errs) >>= fun () ->
        lwt_log_notice Tag.DSL.(fun f ->
            f "Building an empty block using shell validation"
            -% t event "shell_prevalidation_notice") >>= fun () ->
        shell_prevalidation cctxt ~chain ~block seed_nonce_hash operations slot
    | Ok (final_context, validation_result, operations) ->
        lwt_debug Tag.DSL.(fun f ->
            f "Try forging locally the block header for %a (slot %d) for %s (%a)"
            -% t event "try_forging"
            -% a Block_hash.Logging.tag bi.hash
            -% s bake_priorty_tag priority
            -% s Client_keys.Logging.tag name
            -% a timestamp_tag timestamp) >>= fun () ->
        finalize_block_header final_context ~timestamp validation_result operations >>=? fun shell_header ->
        let raw_ops = List.map (List.map forge) operations in
        return (Some (bi, priority, shell_header, raw_ops, delegate, seed_nonce_hash))

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
    return_unit

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
  lwt_log_info Tag.DSL.(fun f ->
      f "Found %d current slots and %d future slots."
      -% t event "pop_baking_slots"
      -% s current_slots_tag (List.length slots)
      -% s future_slots_tag (List.length state.future_slots)) >>= fun () ->
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
      | true ->  lwt_log_error Tag.DSL.(fun f ->
          f "Level %a : previously baked"
          -% t event "double_bake_near_miss"
          -% a level_tag level)  >>= return
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
          return_unit
    end
  | _ -> (* no candidates, or none fit-enough *)
      lwt_debug Tag.DSL.(fun f ->
          f "No valid candidates." -% t event "no_baking_candidates") >>= fun () ->
      return_unit

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
    let constants =
      tzlazy (fun () -> Alpha_services.Constants.all cctxt (`Main, `Head 0)) in
    Client_baking_simulator.load_context ~context_path >>= fun index ->
    let state = create_state genesis_hash context_path index delegates constants bi in
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
