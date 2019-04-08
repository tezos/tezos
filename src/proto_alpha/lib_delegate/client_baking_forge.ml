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

include Internal_event.Legacy_logging.Make_semantic(struct
    let name = Proto_alpha.Name.name ^ ".client.baking"
  end)

open Logging

(* Just proving a point *)
let [@warning "-32"] time_protocol__is__protocol_time
  : Alpha_context.Timestamp.t -> Time.Protocol.t
  = fun x -> x

(* The index of the different components of the protocol's validation passes *)
(* TODO: ideally, we would like this to be more abstract and possibly part of
   the protocol, while retaining the generality of lists *)
(* Hypothesis : we suppose [List.length Proto_alpha.Main.validation_passes = 4] *)
let endorsements_index = 0
let votes_index = 1
let anonymous_index = 2
let managers_index = 3

let default_max_priority = 64
let default_minimal_fees = Tez.zero
let default_minimal_nanotez_per_gas_unit = Z.of_int 10000
let default_minimal_nanotez_per_byte = Z.zero
let default_await_endorsements = true

type state = {
  context_path: string ;
  mutable index : Context.index ;
  (* Nonces file location *)
  nonces_location: [ `Nonce ] Client_baking_files.location ;
  (* see [get_delegates] below to find delegates when the list is empty *)
  delegates: public_key_hash list ;
  (* lazy-initialisation with retry-on-error *)
  constants: Constants.t ;
  (* Minimal operation fee required to include an operation in a block *)
  minimal_fees : Tez.t ;
  (* Minimal operation fee per gas required to include an operation in a block *)
  minimal_nanotez_per_gas_unit : Z.t ;
  (* Minimal operation fee per byte required to include an operation in a block *)
  minimal_nanotez_per_byte : Z.t ;
  (* Await endorsements *)
  await_endorsements: bool ;
  (* truly mutable *)
  mutable best_slot: (Time.Protocol.t * (Client_baking_blocks.block_info * int * public_key_hash)) option ;
}

let create_state
    ?(minimal_fees = default_minimal_fees)
    ?(minimal_nanotez_per_gas_unit = default_minimal_nanotez_per_gas_unit)
    ?(minimal_nanotez_per_byte = default_minimal_nanotez_per_byte)
    ?(await_endorsements = default_await_endorsements)
    context_path index nonces_location delegates constants =
  { context_path ;
    index ;
    nonces_location;
    delegates ;
    constants ;
    minimal_fees ;
    minimal_nanotez_per_gas_unit ;
    minimal_nanotez_per_byte ;
    await_endorsements ;
    best_slot = None ;
  }

let get_delegates cctxt state =
  match state.delegates with
  | [] ->
      Client_keys.get_keys cctxt >>=? fun keys ->
      return (List.map (fun (_,pkh,_,_) -> pkh) keys)
  | _ -> return state.delegates

let generate_seed_nonce () =
  match Nonce.of_bytes (Rand.generate Constants.nonce_length) with
  | Error _errs -> assert false
  | Ok nonce -> nonce

let forge_block_header
    (cctxt : #Proto_alpha.full)
    ~chain
    block
    delegate_sk
    shell
    priority
    seed_nonce_hash =
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

let inject_block
    cctxt
    ?(force = false)
    ?seed_nonce_hash
    ~chain
    ~shell_header
    ~priority
    ~delegate_pkh
    ~delegate_sk
    ~level
    operations =
  assert_valid_operations_hash shell_header operations >>=? fun () ->
  let block = `Hash (shell_header.Tezos_base.Block_header.predecessor, 0) in
  forge_block_header cctxt ~chain block
    delegate_sk shell_header priority seed_nonce_hash >>=? fun signed_header ->
  (* Record baked blocks to prevent double baking  *)
  let open Client_baking_highwatermarks in
  cctxt#with_lock begin fun () ->
    Client_baking_files.resolve_location cctxt ~chain `Block >>=? fun block_location ->
    may_inject_block cctxt block_location ~delegate:delegate_pkh level >>=? function
    | true ->
        record_block cctxt block_location ~delegate:delegate_pkh level >>=? fun () ->
        return_true
    | false ->
        lwt_log_error Tag.DSL.(fun f ->
            f "Level %a : previously baked"
            -% t event "double_bake_near_miss"
            -% a level_tag level)
        >>= fun () -> return force
  end >>=? function
  | false -> fail (Level_previously_baked level)
  | true ->
      Shell_services.Injection.block cctxt
        ~force ~chain signed_header operations >>=? fun block_hash ->
      lwt_log_info Tag.DSL.(fun f ->
          f "Client_baking_forge.inject_block: inject %a"
          -% t event "inject_baked_block"
          -% a Block_hash.Logging.tag block_hash
          -% t signed_header_tag signed_header
          -% t operations_tag operations) >>= fun () ->
      return block_hash

type error += Failed_to_preapply of Tezos_base.Operation.t * error list
type error += Forking_test_chain

let () = begin
  register_error_kind
    `Permanent
    ~id:"Client_baking_forge.failed_to_preapply"
    ~title: "Fail to preapply an operation"
    ~description: ""
    ~pp:(fun ppf (op, err) ->
        let h = Tezos_base.Operation.hash op in
        Format.fprintf ppf "@[Failed to preapply %a:@ @[<v 4>%a@]@]"
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
end

let get_manager_operation_gas_and_fee op =
  let { protocol_data = Operation_data { contents ; _ } ; _ } = op in
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
    ~minimal_fees
    ~minimal_nanotez_per_gas_unit
    ~minimal_nanotez_per_byte
    (operations : Proto_alpha.operation list) =
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
       if Tez.(fee < minimal_fees) then
         return_none
       else
         let (size, gas, _ratio) as weight = compute_weight op (fee, gas) in
         let open Alpha_environment in
         let fees_in_nanotez =
           Z.mul (Z.of_int64 (Tez.to_mutez fee)) (Z.of_int 1000) in
         let enough_fees_for_gas =
           let minimal_fees_in_nanotez =
             Z.mul minimal_nanotez_per_gas_unit gas in
           Z.compare minimal_fees_in_nanotez fees_in_nanotez  <= 0 in
         let enough_fees_for_size =
           let minimal_fees_in_nanotez =
             Z.mul minimal_nanotez_per_byte (Z.of_int size) in
           Z.compare minimal_fees_in_nanotez fees_in_nanotez  <= 0 in
         if enough_fees_for_size && enough_fees_for_gas then
           return_some (op, weight)
         else
           return_none
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
(** [classify_operations] classify the operation in 4 lists indexed as such :
    - 0 -> Endorsements
    - 1 -> Votes and proposals
    - 2 -> Anonymous operations
    - 3 -> High-priority manager operations.
    Returns two list :
    - A desired set of operations to be included
    - Potentially overflowing operations *)
let classify_operations
    (cctxt : #Proto_alpha.full)
    ~chain
    ~block
    ~hard_gas_limit_per_block
    ~minimal_fees
    ~minimal_nanotez_per_gas_unit
    ~minimal_nanotez_per_byte
    (ops: Proto_alpha.operation list) =
  Alpha_block_services.live_blocks cctxt ~chain ~block ()
  >>=? fun live_blocks ->
  (* Remove operations that are too old *)
  let ops =
    List.filter (fun { shell = { branch ; _ } ; _ } ->
        Block_hash.Set.mem branch live_blocks
      ) ops
  in
  let validation_passes_len = List.length Proto_alpha.Main.validation_passes in
  let t = Array.make validation_passes_len [] in
  List.iter
    (fun (op: Proto_alpha.operation) ->
       List.iter
         (fun pass -> t.(pass) <- op :: t.(pass))
         (Main.acceptable_passes op))
    ops ;
  let t = Array.map List.rev t in
  (* Retrieve the optimist maximum paying manager operations *)
  let manager_operations = t.(managers_index) in
  let { Alpha_environment.Updater.max_size ; _ } =
    List.nth Proto_alpha.Main.validation_passes managers_index in
  sort_manager_operations
    ~max_size
    ~hard_gas_limit_per_block
    ~minimal_fees
    ~minimal_nanotez_per_gas_unit
    ~minimal_nanotez_per_byte
    manager_operations
  >>=? fun ordered_operations ->
  (* Greedy heuristic *)
  trim_manager_operations ~max_size ~hard_gas_limit_per_block (List.map fst ordered_operations)
  >>=? fun (desired_manager_operations, overflowing_manager_operations) ->
  t.(managers_index) <- desired_manager_operations ;
  return ((Array.to_list t), overflowing_manager_operations)

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

let unopt_operations cctxt chain mempool = function
  | None -> begin
      match mempool with
      | None ->
          Alpha_block_services.Mempool.pending_operations cctxt ~chain () >>=? fun mpool ->
          let ops = ops_of_mempool mpool in
          return ops
      | Some file ->
          Tezos_stdlib_unix.Lwt_utils_unix.Json.read_file file >>=? fun json ->
          let mpool = Data_encoding.Json.destruct Alpha_block_services.S.Mempool.encoding json in
          let ops = ops_of_mempool mpool in
          return ops
    end
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
        cctxt ~offset:1l (chain, block)>>=? fun { level ; _ } ->
      Alpha_services.Delegate.Baking_rights.get cctxt
        ?max_priority
        ~levels:[level]
        ~delegates:[src_pkh]
        (chain, block)  >>=? fun possibilities ->
      try
        let { Alpha_services.Delegate.Baking_rights.priority = prio ;
              timestamp = time ; _ } =
          List.find
            (fun p -> p.Alpha_services.Delegate.Baking_rights.level = level)
            possibilities in
        return (prio, time)
      with Not_found ->
        failwith "No slot found at level %a" Raw_level.pp level

let unopt_timestamp timestamp minimal_timestamp =
  match timestamp, minimal_timestamp with
  | None, None -> return (Time.System.to_protocol (Systime_os.now ()))
  | None, Some timestamp -> return timestamp
  | Some timestamp, None -> return timestamp
  | Some timestamp, Some minimal_timestamp ->
      if timestamp < minimal_timestamp then
        failwith
          "Proposed timestamp %a is earlier than minimal timestamp %a"
          Time.Protocol.pp_hum timestamp
          Time.Protocol.pp_hum minimal_timestamp
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

let filter_and_apply_operations
    state
    block_info
    ~timestamp
    ?protocol_data
    ((operations : packed_operation list list), overflowing_operations) =
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
  let validate_operation inc op =
    add_operation inc op >>= function
    | Error errs ->
        lwt_debug Tag.DSL.(fun f ->
            f "@[<v 4>Client-side validation: invalid operation filtered %a@\n%a@]"
            -% t event "baking_rejected_invalid_operation"
            -% a Operation_hash.Logging.tag (Operation.hash_packed op)
            -% a errs_tag errs)
        >>= fun () ->
        Lwt.return_none
    | Ok (resulting_state, _receipt) ->
        Lwt.return_some resulting_state
  in
  let filter_valid_operations inc ops =
    Lwt_list.fold_left_s (fun (inc, acc) op ->
        validate_operation inc op >>= function
        | None -> Lwt.return (inc, acc)
        | Some inc' -> Lwt.return (inc', op :: acc)
      ) (inc, []) ops
  in
  (* Invalid endorsements are detected during block finalization *)
  let is_valid_endorsement inc endorsement =
    validate_operation inc endorsement >>= function
    | None -> Lwt.return_none
    | Some inc' -> finalize_construction inc' >>= begin function
        | Ok _ -> Lwt.return_some endorsement
        | Error _ -> Lwt.return_none
      end
  in
  filter_valid_operations initial_inc votes >>= fun (inc, votes) ->
  filter_valid_operations inc anonymous >>= fun (inc, anonymous) ->
  (* Retrieve the correct index order *)
  let managers = List.sort Proto_alpha.compare_operations managers in
  let overflowing_operations = List.sort Proto_alpha.compare_operations overflowing_operations in
  filter_valid_operations inc (managers @  overflowing_operations) >>= fun (inc, managers) ->
  (* Gives a chance to the endorser to fund their deposit in the current block *)
  Lwt_list.filter_map_s (is_valid_endorsement inc) endorsements >>= fun endorsements ->
  finalize_construction inc >>=? fun _ ->
  let quota : Alpha_environment.Updater.quota list = Main.validation_passes in
  let  { Constants.endorsers_per_block ; hard_gas_limit_per_block ; _ } =
    state.constants.parametric in
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
    | { protocol_data = Operation_data { contents = Single (Double_baking_evidence _ ) ; _ } ; _ } -> true
    | { protocol_data = Operation_data { contents = Single (Double_endorsement_evidence _ ) ; _ } ; _ } -> true
    | _ -> false in
  let evidences, anonymous = List.partition is_evidence anonymous in
  trim_manager_operations ~max_size:(List.nth quota managers_index).max_size
    ~hard_gas_limit_per_block managers >>=? fun (accepted_managers, _overflowing_managers) ->
  (* Retrieve the correct index order *)
  let accepted_managers = List.sort Proto_alpha.compare_operations accepted_managers in
  (* Make sure we only keep valid operations *)
  filter_valid_operations initial_inc votes >>= fun (inc, votes) ->
  filter_valid_operations inc anonymous >>= fun (inc, anonymous) ->
  filter_valid_operations inc accepted_managers >>= fun (inc, accepted_managers) ->
  Lwt_list.filter_map_s (is_valid_endorsement inc) endorsements >>= fun endorsements ->
  (* Endorsements won't fail now *)
  fold_left_s (fun inc op ->
      add_operation inc op >>=? fun (inc, _receipt) ->
      return inc) inc endorsements >>=? fun inc ->
  (* Endorsement and double baking/endorsement evidence do not commute:
     we apply denunciation operations after endorsements. *)
  filter_valid_operations inc evidences >>= fun (final_inc, evidences) ->
  let operations = List.map List.rev [ endorsements ; votes ; anonymous @ evidences ; accepted_managers ] in
  finalize_construction final_inc >>=? fun (validation_result, metadata) ->
  return (final_inc, (validation_result, metadata), operations)

(* Build the block header : mimics node prevalidation *)
let finalize_block_header
    pred_shell_header
    ~timestamp
    validation_result
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
  begin Context.get_test_chain context >>= function
    | Not_running -> return context
    | Running { expiration ; _ } ->
        if Time.Protocol.(expiration <= timestamp) then
          Context.set_test_chain context Not_running >>= fun context ->
          return context
        else
          return context
    | Forking _ -> fail Forking_test_chain
  end >>=? fun context ->
  Context.hash ~time:timestamp ?message context >>= fun context ->
  let header =
    Tezos_base.Block_header.
      { pred_shell_header with
        level = Int32.succ pred_shell_header.level ;
        validation_passes ;
        operations_hash ;
        fitness ;
        context ;
      } in
  return header

let forge_block
    cctxt
    ?force
    ?operations
    ?(best_effort = operations = None)
    ?(sort = best_effort)
    ?(minimal_fees = default_minimal_fees)
    ?(minimal_nanotez_per_gas_unit = default_minimal_nanotez_per_gas_unit)
    ?(minimal_nanotez_per_byte = default_minimal_nanotez_per_byte)
    ?(await_endorsements = default_await_endorsements)
    ?timestamp
    ?mempool
    ?context_path
    ?seed_nonce_hash
    ~chain
    ~priority
    ~delegate_pkh
    ~delegate_sk
    block =
  (* making the arguments usable *)
  unopt_operations cctxt chain mempool operations >>=? fun operations_arg ->
  decode_priority cctxt chain block priority >>=? fun (priority, minimal_timestamp) ->
  unopt_timestamp timestamp minimal_timestamp >>=? fun timestamp ->

  (* get basic building blocks *)
  let protocol_data = forge_faked_protocol_data ~priority ~seed_nonce_hash in
  Alpha_services.Constants.all cctxt (chain, block) >>=?
  fun Constants.{ parametric = { hard_gas_limit_per_block ; endorsers_per_block ; _ } ; _  } ->
  classify_operations
    cctxt
    ~chain
    ~hard_gas_limit_per_block
    ~block
    ~minimal_fees
    ~minimal_nanotez_per_gas_unit
    ~minimal_nanotez_per_byte
    operations_arg
  >>=? fun (operations, overflowing_ops) ->
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

  begin
    match context_path with
    | None ->
        Alpha_block_services.Helpers.Preapply.block
          cctxt ~chain ~block ~timestamp ~sort ~protocol_data operations >>=? fun (shell_header, result) ->
        let operations =
          List.map (fun l -> List.map snd l.Preapply_result.applied) result in
        (* everything went well (or we don't care about errors): GO! *)
        if best_effort || all_ops_valid result then
          return (shell_header, operations)
          (* some errors (and we care about them) *)
        else
          let result = List.fold_left merge_preapps Preapply_result.empty result in
          Lwt.return_error @@
          List.filter_map (error_of_op result) operations_arg
    | Some context_path ->
        assert sort ;
        assert best_effort ;
        Context.init ~readonly:true context_path >>= fun index ->
        Client_baking_blocks.info cctxt ~chain block >>=? fun bi ->
        Alpha_services.Constants.all cctxt (chain, `Head 0) >>=? fun constants ->
        Client_baking_files.resolve_location cctxt ~chain `Nonce >>=? fun nonces_location ->
        let state = {
          context_path ;
          index ;
          nonces_location ;
          constants ;
          delegates = [] ;
          best_slot = None ;
          await_endorsements ;
          minimal_fees = default_minimal_fees ;
          minimal_nanotez_per_gas_unit = default_minimal_nanotez_per_gas_unit ;
          minimal_nanotez_per_byte = default_minimal_nanotez_per_byte ;
        } in
        filter_and_apply_operations ~timestamp ~protocol_data state bi (operations, overflowing_ops)
        >>=? fun (final_context, (validation_result, _), operations) ->
        let current_protocol = bi.next_protocol in
        Context.get_protocol validation_result.context >>= fun next_protocol ->
        if Protocol_hash.equal current_protocol next_protocol then begin
          finalize_block_header final_context.header ~timestamp
            validation_result operations >>= function
          | Error [ Forking_test_chain ] ->
              Alpha_block_services.Helpers.Preapply.block
                cctxt ~chain ~block ~timestamp ~sort ~protocol_data operations >>=? fun (shell_header, _result) ->
              return (shell_header, List.map (List.map forge) operations)
          | Error _ as errs -> Lwt.return errs
          | Ok shell_header -> return (shell_header, List.map (List.map forge) operations)
        end else begin
          lwt_log_notice Tag.DSL.(fun f ->
              f "New protocol detected: using shell validation"
              -% t event "shell_prevalidation_notice") >>= fun () ->
          Alpha_block_services.Helpers.Preapply.block
            cctxt ~chain ~block ~timestamp ~sort ~protocol_data operations >>=? fun (shell_header, _result) ->
          return (shell_header, List.map (List.map forge) operations)
        end
  end >>=? fun (shell_header, operations) ->

  (* Now for some logging *)
  let total_op_count = List.length operations_arg in
  let valid_op_count = List.length (List.concat operations) in
  lwt_log_notice Tag.DSL.(fun f ->
      f "Found %d valid operations (%d refused) for timestamp %a. Computed fitness %a."
      -% t event "found_valid_operations"
      -% s valid_ops valid_op_count
      -% s refused_ops (total_op_count - valid_op_count)
      -% a timestamp_tag (Time.System.of_protocol_exn timestamp)
      -% a fitness_tag shell_header.fitness) >>= fun () ->

  begin match Alpha_environment.wrap_error (Raw_level.of_int32 shell_header.level) with
    | Ok level -> return level
    | (Error errs) as err ->
        lwt_log_error Tag.DSL.(fun f ->
            f "@[Error on raw_level conversion : %a@]"
            -% t event "block_injection_failed"
            -% a errs_tag errs
          ) >>= fun () ->
        Lwt.return err
  end >>=? fun level ->

  inject_block cctxt
    ?force ~chain ~shell_header ~priority ?seed_nonce_hash
    ~delegate_pkh
    ~delegate_sk
    ~level
    operations >>= function
  | Ok hash -> return hash
  | Error errs as error ->
      lwt_log_error Tag.DSL.(fun f ->
          f "@[<v 4>Error while injecting block@ @[Included operations : %a@]@ %a@]"
          -% t event "block_injection_failed"
          -% a raw_operations_tag (List.concat operations)
          -% a errs_tag errs
        ) >>= fun () ->
      Lwt.return error

let shell_prevalidation
    (cctxt : #Proto_alpha.full)
    ~chain
    ~block
    seed_nonce_hash
    operations
    ((timestamp, (bi, priority, delegate)) as _slot) =
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
      return_none
  | Ok (shell_header, operations) ->
      let raw_ops =
        List.map (fun l ->
            List.map snd l.Preapply_result.applied) operations in
      return_some (bi, priority, shell_header, raw_ops, delegate, seed_nonce_hash)

let filter_outdated_endorsements expected_level ops =
  List.filter (function
      | { Alpha_context.protocol_data =
            Operation_data { contents = Single (Endorsement { level ; _ }) ; _ } ; _} ->
          Raw_level.equal expected_level level
      | _ -> true
    ) ops

let next_baking_delay state priority =
  let { Constants.parametric = { time_between_blocks ; _ } ; _ } = state.constants in
  let rec associated_period durations prio =
    if List.length durations = 0 then
      (* Mimic [Baking.minimal_time] behaviour *)
      associated_period [ Period.one_minute ] prio
    else
      match durations with
      | [] -> assert false
      | [ last ] ->
          Period.to_seconds last
      | first :: durations ->
          if prio = 0 then
            Period.to_seconds first
          else
            associated_period durations (prio - 1)
  in
  let span = associated_period time_between_blocks (priority + 1) in
  return span

let count_slots_endorsements inc (_timestamp, (head, _priority, _delegate)) operations =
  Lwt_list.fold_left_s (fun acc -> function
      | { Alpha_context.protocol_data =
            Operation_data { contents = Single (Endorsement { level ; _ }) ; _ } ; _ } as op
        when Raw_level.(level = head.Client_baking_blocks.level) ->
          begin
            let open Apply_results in
            Client_baking_simulator.add_operation inc op >>= function
            | Ok (_inc,
                  Operation_metadata
                    { contents = Single_result (Endorsement_result { slots ; _ })} ) ->
                Lwt.return (acc + List.length slots)
            | Error _ | _ ->
                (* We do not handle errors here *)
                Lwt.return acc
          end
      | _ -> Lwt.return acc
    ) 0 operations

let rec filter_limits tnow limits =
  match limits with
  | [] -> []
  | (time, _) :: _ as limits when Time.Protocol.(tnow < time) -> limits
  | _ :: limits -> filter_limits tnow limits

(** [fetch_operations] retrieve the operations present in the
    mempool. If no endorsements are present in the initial set, it
    waits until [state.max_waiting_time] seconds after its injection range start date. *)
let fetch_operations
    (cctxt : #Proto_alpha.full)
    ~chain
    state
    (timestamp, (head, priority, _delegate) as slot)
  =
  Alpha_block_services.Mempool.monitor_operations cctxt ~chain
    ~applied:true ~branch_delayed:true
    ~refused:false ~branch_refused:false () >>=? fun (operation_stream, _stop) ->
  (* Hypothesis : the first call to the stream returns instantly, even if the mempool is empty. *)
  Lwt_stream.get operation_stream >>= function
  | None -> (* New head received : not supposed to happen. *)
      return_none
  | Some current_mempool ->
      let operations = ref (filter_outdated_endorsements head.Client_baking_blocks.level current_mempool) in
      Client_baking_simulator.begin_construction ~timestamp state.index head >>=? fun inc ->
      count_slots_endorsements inc slot !operations >>= fun nb_arrived_endorsements ->
      (* If 100% of the endorsements arrived, we don't need to wait *)
      let endorsers_per_block = state.constants.parametric.endorsers_per_block in
      if (not state.await_endorsements) || nb_arrived_endorsements = endorsers_per_block then
        return_some !operations
      else
        next_baking_delay state priority >>=? fun next_slot_delay ->
        let hard_delay = Int64.div next_slot_delay 2L in
        (* The time limit is defined as 1/2 of the next baking slot's time *)
        let limit_date = Time.Protocol.add timestamp hard_delay in
        (* Time limits :
           - We expect all of the endorsements until 1/3 of the time limit has passed ;
           - We expect 2/3 of the endorsements until 2/3 of the time limit has passed ;
           - We expect 1/3 of the endorsements until the time limit has passed ;
           - We bake with what we have when the time limit has been reached.
        *)
        let limits =
          [ (Time.Protocol.add timestamp (Int64.div hard_delay 3L), endorsers_per_block) ;
            (Time.Protocol.add timestamp (Int64.div (Int64.mul hard_delay 2L) 3L), 2 * endorsers_per_block / 3) ;
            (limit_date, endorsers_per_block / 3) ]
        in
        let timespan =
          let timespan =
            Ptime.diff
              (Time.System.of_protocol_exn limit_date) (Systime_os.now ()) in
          if Ptime.Span.compare timespan Ptime.Span.zero > 0 then
            timespan
          else
            Ptime.Span.zero in
        lwt_log_notice Tag.DSL.(fun f ->
            f "Waiting until %a (%a) for more endorsements in the \
               mempool (%a/%a arrived)."
            -% t event "waiting_operations"
            -% a timestamp_tag (Time.System.of_protocol_exn limit_date)
            -% a timespan_tag timespan
            -% a op_count nb_arrived_endorsements
            -% a op_count endorsers_per_block
          ) >>= fun () ->
        Shell_services.Mempool.request_operations cctxt ~chain () >>=? fun () ->
        let timeout = match Client_baking_scheduling.sleep_until limit_date with
          | None -> Lwt.return_unit
          | Some timeout -> timeout in
        let last_get_event = ref None in
        let get_event () =
          match !last_get_event with
          | None ->
              let t = Lwt_stream.get operation_stream in
              last_get_event := Some t ;
              t
          | Some t -> t in
        let rec loop nb_arrived_endorsements limits =
          Lwt.choose [ (timeout >|= fun () -> `Timeout) ;
                       (get_event () >|= fun e -> `Event e) ; ]
          >>= function
          | `Event (Some op_list) -> begin
              last_get_event := None ;
              operations := op_list @ !operations ;
              count_slots_endorsements inc slot op_list >>= fun new_endorsements ->
              let nb_arrived_endorsements = nb_arrived_endorsements + new_endorsements in
              let limits =
                filter_limits
                  (Time.System.to_protocol (Systime_os.now ()))
                  limits in
              let required =
                match limits with
                | [] -> 0 (* If we are late, we do not require endorsements *)
                | (_time, required) :: _ -> required in
              let enough = nb_arrived_endorsements >= required in
              if enough then
                return_some !operations
              else
                loop nb_arrived_endorsements limits
            end
          | `Timeout -> return_some !operations
          | `Event None ->
              (* New head received. Should not happen : let the
                 caller handle this case. *)
              return_none
        in
        loop nb_arrived_endorsements limits

(** Given a delegate baking slot [build_block] constructs a full block
    with consistent operations that went through the client-side
    validation *)
let build_block
    cctxt
    state
    seed_nonce_hash
    ((timestamp, (bi, priority, delegate)) as slot) =
  let chain = `Hash bi.Client_baking_blocks.chain_id in
  let block = `Hash (bi.hash, 0) in
  Alpha_services.Helpers.current_level cctxt
    ~offset:1l (chain, block) >>=? fun next_level ->
  let seed_nonce_hash =
    if next_level.Level.expected_commitment then
      Some seed_nonce_hash
    else
      None in
  Client_keys.Public_key_hash.name cctxt delegate >>=? fun name ->

  lwt_debug Tag.DSL.(fun f ->
      f "Try baking after %a (slot %d) for %s (%a)"
      -% t event "try_baking"
      -% a Block_hash.Logging.tag bi.hash
      -% s bake_priority_tag priority
      -% s Client_keys.Logging.tag name
      -% a timestamp_tag (Time.System.of_protocol_exn timestamp)) >>= fun () ->

  fetch_operations cctxt ~chain state slot >>=? function
  | None ->
      lwt_log_notice Tag.DSL.(fun f ->
          f "Received a new head while waiting for operations. Aborting this block."
          -% t event "new_head_received") >>= fun () ->
      return_none
  | Some operations ->
      let hard_gas_limit_per_block = state.constants.parametric.hard_gas_limit_per_block in
      classify_operations cctxt
        ~chain
        ~hard_gas_limit_per_block
        ~minimal_fees:state.minimal_fees
        ~minimal_nanotez_per_gas_unit:state.minimal_nanotez_per_gas_unit
        ~minimal_nanotez_per_byte:state.minimal_nanotez_per_byte
        ~block operations
      >>=? fun (operations, overflowing_ops) ->
      let next_version =
        match Tezos_base.Block_header.get_forced_protocol_upgrade
                ~level:(Raw_level.to_int32 next_level.Level.level) with
        | None -> bi.next_protocol
        | Some hash -> hash
      in
      if Protocol_hash.(Proto_alpha.hash <> next_version) then
        (* Let the shell validate this *)
        shell_prevalidation cctxt ~chain ~block seed_nonce_hash
          operations slot
      else
        let protocol_data = forge_faked_protocol_data ~priority ~seed_nonce_hash in
        filter_and_apply_operations ~timestamp ~protocol_data state bi (operations, overflowing_ops)
        >>= function
        | Error errs ->
            lwt_log_error Tag.DSL.(fun f ->
                f "Client-side validation: error while filtering invalid operations :@\n@[<v 4>%a@]"
                -% t event "client_side_validation_error"
                -% a errs_tag errs) >>= fun () ->
            lwt_log_notice Tag.DSL.(fun f ->
                f "Building a block using shell validation"
                -% t event "shell_prevalidation_notice") >>= fun () ->
            shell_prevalidation cctxt ~chain ~block seed_nonce_hash
              operations slot
        | Ok (final_context, (validation_result, _), operations) ->
            lwt_debug Tag.DSL.(fun f ->
                f "Try forging locally the block header for %a (slot %d) for %s (%a)"
                -% t event "try_forging"
                -% a Block_hash.Logging.tag bi.hash
                -% s bake_priority_tag priority
                -% s Client_keys.Logging.tag name
                -% a timestamp_tag (Time.System.of_protocol_exn timestamp)) >>= fun () ->
            let current_protocol = bi.next_protocol in
            Context.get_protocol validation_result.context >>= fun next_protocol ->
            if Protocol_hash.equal current_protocol next_protocol then begin
              finalize_block_header final_context.header ~timestamp validation_result operations >>= function
              | Error [ Forking_test_chain ] ->
                  shell_prevalidation cctxt ~chain ~block seed_nonce_hash
                    operations slot
              | Error _ as errs -> Lwt.return errs
              | Ok shell_header ->
                  let raw_ops = List.map (List.map forge) operations in
                  return (Some (bi, priority, shell_header, raw_ops, delegate, seed_nonce_hash))
            end else begin
              lwt_log_notice Tag.DSL.(fun f ->
                  f "New protocol detected: using shell validation"
                  -% t event "shell_prevalidation_notice") >>= fun () ->
              shell_prevalidation cctxt ~chain ~block seed_nonce_hash
                operations slot end

(** [bake cctxt state] create a single block when woken up to do
    so. All the necessary information is available in the
    [state.best_slot]. *)
let bake (cctxt : #Proto_alpha.full) ~chain state =
  begin match state.best_slot with
    | None -> assert false (* unreachable *)
    | Some slot -> return slot end >>=? fun slot ->
  let seed_nonce = generate_seed_nonce () in
  let seed_nonce_hash = Nonce.hash seed_nonce in

  build_block cctxt state seed_nonce_hash slot >>=? function
  | Some (head, priority, shell_header, operations, delegate, seed_nonce_hash) -> begin
      let level = Raw_level.succ head.level in
      Client_keys.Public_key_hash.name cctxt delegate >>=? fun name ->
      lwt_log_info Tag.DSL.(fun f ->
          f "Injecting block (priority %d, fitness %a) for %s after %a..."
          -% t event "start_injecting_block"
          -% s bake_priority_tag priority
          -% a fitness_tag shell_header.fitness
          -% s Client_keys.Logging.tag name
          -% a Block_hash.Logging.predecessor_tag shell_header.predecessor
          -% t Signature.Public_key_hash.Logging.tag delegate) >>= fun () ->

      Client_keys.get_key cctxt delegate >>=? fun (_, _, delegate_sk) ->
      inject_block cctxt ~chain ~force:false
        ~shell_header ~priority ?seed_nonce_hash ~delegate_pkh:delegate ~delegate_sk ~level operations >>= function
      | Error errs ->
          lwt_log_error Tag.DSL.(fun f ->
              f "@[<v 4>Error while injecting block@ @[Included operations : %a@]@ %a@]"
              -% t event "block_injection_failed"
              -% a raw_operations_tag (List.concat operations)
              -% a errs_tag errs
            ) >>= fun () -> return_unit

      | Ok block_hash ->
          lwt_log_notice Tag.DSL.(fun f ->
              f "Injected block %a for %s after %a (level %a, priority %d, fitness %a, operations %a)."
              -% t event "injected_block"
              -% a Block_hash.Logging.tag block_hash
              -% s Client_keys.Logging.tag name
              -% a Block_hash.Logging.tag shell_header.predecessor
              -% a level_tag level
              -% s bake_priority_tag priority
              -% a fitness_tag shell_header.fitness
              -% a operations_tag operations
            ) >>= fun () ->

          begin if seed_nonce_hash <> None then
              cctxt#with_lock begin fun () ->
                let open Client_baking_nonces in
                load cctxt state.nonces_location >>=? fun nonces ->
                let nonces = add nonces block_hash seed_nonce in
                save cctxt state.nonces_location nonces
              end
              |> trace_exn (Failure "Error while recording nonce")
            else return_unit end >>=? fun () ->
          return_unit
    end
  | None -> return_unit

(** [get_baking_slots] calls the node via RPC to retrieve the potential
    slots for the given delegates within a given range of priority *)
let get_baking_slots cctxt
    ?(max_priority = default_max_priority)
    new_head
    delegates =
  let chain = `Hash new_head.Client_baking_blocks.chain_id in
  let block = `Hash (new_head.hash, 0) in
  let level = Raw_level.succ new_head.level in
  Alpha_services.Delegate.Baking_rights.get cctxt
    ~max_priority
    ~levels:[level]
    ~delegates
    (chain, block) >>= function
  | Error errs ->
      lwt_log_error Tag.DSL.(fun f ->
          f "Error while fetching baking possibilities:\n%a"
          -% t event "baking_slot_fetch_errors"
          -% a errs_tag errs) >>= fun () ->
      Lwt.return_nil
  | Ok [] -> Lwt.return_nil
  | Ok slots ->
      let slots = List.filter_map
          (function
            | { Alpha_services.Delegate.Baking_rights.timestamp = None ; _ } -> None
            | { timestamp = Some timestamp ; priority ; delegate ; _ } ->
                Some (timestamp, (new_head, priority, delegate))
          )
          slots in
      Lwt.return slots

(** [compute_best_slot_on_current_level] retrieves, among the given
    delegates, the highest priority slot for the current level. Then,
    it registers this slot in the state so the timeout knows when to
    wake up. *)
let compute_best_slot_on_current_level
    ?max_priority
    (cctxt : #Proto_alpha.full)
    state
    new_head =
  get_delegates cctxt state >>=? fun delegates ->
  let level = Raw_level.succ new_head.Client_baking_blocks.level in
  get_baking_slots cctxt ?max_priority new_head delegates >>= function
  | [] ->
      lwt_log_notice Tag.DSL.(fun f ->
          let max_priority = Option.unopt ~default:default_max_priority max_priority in
          f "No slot found at level %a (max_priority = %d)"
          -% t event "no_slot_found"
          -% a level_tag level
          -% s bake_priority_tag max_priority) >>= fun () ->
      return_none (* No slot found *)
  | h::t ->
      (* One or more slot found, fetching the best (lowest) priority.
         We do not suppose that the received slots are sorted. *)
      let (timestamp, (_, priority, delegate) as best_slot) =
        List.fold_left
          (fun ((_, (_, priority, _)) as acc) ((_, (_, priority', _)) as slot) ->
             if priority < priority' then acc else slot
          ) h t
      in
      Client_keys.Public_key_hash.name cctxt delegate >>=? fun name ->
      lwt_log_notice Tag.DSL.(fun f ->
          f "New baking slot found (level %a, priority %d) at %a for %s after %a."
          -% t event "have_baking_slot"
          -% a level_tag level
          -% s bake_priority_tag priority
          -% a timestamp_tag (Time.System.of_protocol_exn timestamp)
          -% s Client_keys.Logging.tag name
          -% a Block_hash.Logging.tag new_head.hash
          -% t Signature.Public_key_hash.Logging.tag delegate) >>= fun () ->
      (* Found at least a slot *)
      return_some best_slot

(** [reveal_potential_nonces] reveal registered nonces *)
let reveal_potential_nonces (cctxt : #Client_context.full) constants ~chain ~block =
  cctxt#with_lock begin fun () ->
    Client_baking_files.resolve_location cctxt ~chain `Nonce >>=? fun nonces_location ->
    Client_baking_nonces.load cctxt nonces_location >>= function
    | Error err ->
        lwt_log_error Tag.DSL.(fun f ->
            f "Cannot read nonces: %a"
            -% t event "read_nonce_fail"
            -% a errs_tag err) >>= fun () ->
        return_unit
    | Ok nonces ->
        Client_baking_nonces.get_unrevealed_nonces cctxt nonces_location nonces >>= function
        | Error err ->
            lwt_log_error Tag.DSL.(fun f ->
                f "Cannot retrieve unrevealed nonces: %a"
                -% t event "nonce_retrieval_fail"
                -% a errs_tag err) >>= fun () ->
            return_unit
        | Ok []  -> return_unit
        | Ok nonces_to_reveal ->
            Client_baking_revelation.inject_seed_nonce_revelation
              cctxt ~chain ~block nonces_to_reveal >>= function
            | Error err ->
                lwt_log_error Tag.DSL.(fun f ->
                    f "Cannot inject nonces: %a"
                    -% t event "nonce_injection_fail"
                    -% a errs_tag err) >>= fun () ->
                return_unit
            | Ok () ->
                (* If some nonces are to be revealed it means:
                   - We entered a new cycle and we can clear old nonces ;
                   - A revelation was not included yet in the cycle beggining.
                   So, it is safe to only filter outdated_nonces there *)
                Client_baking_nonces.filter_outdated_nonces
                  cctxt ~constants nonces_location nonces >>=? fun live_nonces ->
                Client_baking_nonces.save cctxt nonces_location live_nonces >>=? fun () ->
                return_unit
  end

(** [create] starts the main loop of the baker. The loop monitors new blocks and
    starts individual baking operations when baking-slots are available to any of
    the [delegates] *)
let create
    (cctxt : #Proto_alpha.full)
    ?minimal_fees
    ?minimal_nanotez_per_gas_unit
    ?minimal_nanotez_per_byte
    ?await_endorsements
    ?max_priority
    ~chain
    ~context_path
    delegates
    block_stream =
  let state_maker bi =
    Alpha_services.Constants.all cctxt (chain, `Head 0) >>=? fun constants ->
    Client_baking_simulator.load_context ~context_path >>= fun index ->
    Client_baking_simulator.check_context_consistency
      index bi.Client_baking_blocks.context >>=? fun () ->
    Client_baking_files.resolve_location cctxt ~chain `Nonce >>=? fun nonces_location ->
    let state = create_state
        ?minimal_fees ?minimal_nanotez_per_gas_unit ?minimal_nanotez_per_byte
        ?await_endorsements
        context_path index nonces_location delegates constants in
    return state
  in

  let event_k cctxt state new_head =
    reveal_potential_nonces cctxt state.constants
      ~chain ~block:(`Hash (new_head.Client_baking_blocks.hash, 0)) >>= fun _ignore_nonce_err ->
    compute_best_slot_on_current_level ?max_priority cctxt state new_head >>=? fun slot ->
    state.best_slot <- slot ;
    return_unit
  in

  let compute_timeout state =
    match state.best_slot with
    | None ->
        (* No slot, just wait for new blocks which will give more info *)
        Lwt_utils.never_ending ()
    | Some (timestamp, _) ->
        match Client_baking_scheduling.sleep_until timestamp with
        | None -> Lwt.return_unit
        | Some timeout -> timeout
  in

  let timeout_k cctxt state () =
    bake cctxt ~chain state >>=? fun () ->
    (* Stopping the timeout and waiting for the next block *)
    state.best_slot <- None ;
    return_unit
  in

  Client_baking_scheduling.main
    ~name:"baker"
    ~cctxt
    ~stream:block_stream
    ~state_maker
    ~pre_loop:event_k
    ~compute_timeout
    ~timeout_k
    ~event_k
