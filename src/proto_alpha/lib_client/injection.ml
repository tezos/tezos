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
open Apply_operation_result

let get_branch (rpc_config: #Proto_alpha.full)
    ~chain ~(block : Block_services.block) branch =
  let branch = Option.unopt ~default:0 branch in (* TODO export parameter *)
  begin
    match block with
    | `Head n -> return (`Head (n+branch))
    | `Hash (h,n) -> return (`Hash (h,n+branch))
    | `Genesis -> return `Genesis
  end >>=? fun block ->
  Shell_services.Blocks.hash rpc_config ~chain ~block () >>=? fun hash ->
  return hash

type 'kind preapply_result =
  Operation_hash.t * 'kind operation * 'kind operation_metadata

type 'kind result_list =
  Operation_hash.t * 'kind contents_list * 'kind contents_result_list

type 'kind result =
  Operation_hash.t * 'kind contents * 'kind contents_result

let preapply (type t)
    (cctxt: #Proto_alpha.full) ~chain ~block
    ?branch ?src_sk (contents : t contents_list) =
  get_branch cctxt ~chain ~block branch >>=? fun branch ->
  let bytes =
    Data_encoding.Binary.to_bytes_exn
      Operation.unsigned_encoding
      ({ branch }, Contents_list contents) in
  let watermark =
    match contents with
    | Single (Endorsements _) -> Signature.Endorsement
    | _ -> Signature.Generic_operation in
  begin
    match src_sk with
    | None -> return None
    | Some src_sk ->
        Client_keys.sign
          ~watermark src_sk bytes >>=? fun signature ->
        return (Some signature)
  end >>=? fun signature ->
  let op : _ Operation.t =
    { shell = { branch } ;
      protocol_data = { contents ; signature } } in
  let oph = Operation.hash op in
  Alpha_block_services.Helpers.Preapply.operations
    cctxt ~chain ~block [Operation.pack op] >>=? function
  | [(Operation_data op', Operation_metadata result)] -> begin
      match Operation.equal
              op { shell = { branch } ; protocol_data = op' },
            Apply_operation_result.kind_equal_list contents result.contents with
      | Some Operation.Eq, Some Apply_operation_result.Eq ->
          return ((oph, op, result) : t preapply_result)
      | _ -> failwith "Unexpected result"
    end
  | _ -> failwith "Unexpected result"

let estimated_gas_single
    (type kind)
    (Manager_operation_result { operation_result ;
                                internal_operation_results }
     : kind Kind.manager contents_result) =
  let consumed_gas (type kind) (result : kind manager_operation_result) =
    match result with
    | Applied (Transaction_result { consumed_gas }) -> Ok consumed_gas
    | Applied (Origination_result { consumed_gas }) -> Ok consumed_gas
    | Applied Reveal_result -> Ok Z.zero
    | Applied Delegation_result -> Ok Z.zero
    | Skipped _ -> assert false
    | Failed (_, errs) -> Alpha_environment.wrap_error (Error errs) in
  List.fold_left
    (fun acc (Internal_operation_result (_, r)) ->
       acc >>? fun acc ->
       consumed_gas r >>? fun gas ->
       Ok (Z.add acc gas))
    (consumed_gas operation_result) internal_operation_results

let rec estimated_gas :
  type kind. kind Kind.manager contents_result_list -> _ =
  function
  | Single_result res -> estimated_gas_single res
  | Cons_result (res, rest) ->
      estimated_gas_single res >>? fun gas1 ->
      estimated_gas rest >>? fun gas2 ->
      Ok (Z.add gas1 gas2)

let estimated_storage_single
    (type kind)
    (Manager_operation_result { operation_result ;
                                internal_operation_results }
     : kind Kind.manager contents_result) =
  let storage_size_diff (type kind) (result : kind manager_operation_result) =
    match result with
    | Applied (Transaction_result { storage_size_diff }) -> Ok storage_size_diff
    | Applied (Origination_result { storage_size_diff }) -> Ok storage_size_diff
    | Applied Reveal_result -> Ok Z.zero
    | Applied Delegation_result -> Ok Z.zero
    | Skipped _ -> assert false
    | Failed (_, errs) -> Alpha_environment.wrap_error (Error errs) in
  List.fold_left
    (fun acc (Internal_operation_result (_, r)) ->
       acc >>? fun acc ->
       storage_size_diff r >>? fun storage ->
       Ok (Z.add acc storage))
    (storage_size_diff operation_result) internal_operation_results

let estimated_storage res =
  let rec estimated_storage :
    type kind. kind Kind.manager contents_result_list -> _ =
    function
    | Single_result res -> estimated_storage_single res
    | Cons_result (res, rest) ->
        estimated_storage_single res >>? fun storage1 ->
        estimated_storage rest >>? fun storage2 ->
        Ok (Z.add storage1 storage2) in
  estimated_storage res >>? fun diff ->
  Ok (max Z.zero diff)

let originated_contracts_single
    (type kind)
    (Manager_operation_result { operation_result ;
                                internal_operation_results }
     : kind Kind.manager contents_result) =
  let originated_contracts (type kind) (result : kind manager_operation_result) =
    match result with
    | Applied (Transaction_result { originated_contracts }) -> Ok originated_contracts
    | Applied (Origination_result { originated_contracts }) -> Ok originated_contracts
    | Applied Reveal_result -> Ok []
    | Applied Delegation_result -> Ok []
    | Skipped _ -> assert false
    | Failed (_, errs) -> Alpha_environment.wrap_error (Error errs) in
  List.fold_left
    (fun acc (Internal_operation_result (_, r)) ->
       acc >>? fun acc ->
       originated_contracts r >>? fun contracts ->
       Ok (List.rev_append contracts acc))
    (originated_contracts operation_result >|? List.rev)
    internal_operation_results

let rec originated_contracts :
  type kind. kind contents_result_list -> _ =
  function
  | Single_result (Manager_operation_result _ as res) ->
      originated_contracts_single res >|? List.rev
  | Single_result _ -> Ok []
  | Cons_result (res, rest) ->
      originated_contracts_single res >>? fun contracts1 ->
      originated_contracts rest >>? fun contracts2 ->
      Ok (List.rev_append contracts1 contracts2)

let detect_script_failure :
  type kind. kind operation_metadata -> _ =
  let rec detect_script_failure :
    type kind. kind contents_result_list -> _ =
    let detect_script_failure_single
        (type kind)
        (Manager_operation_result { operation_result ;
                                    internal_operation_results }
         : kind Kind.manager contents_result) =
      let detect_script_failure (type kind) (result : kind manager_operation_result) =
        match result with
        | Applied _ -> Ok ()
        | Skipped _ -> assert false
        | Failed (_, errs) ->
            record_trace
              (failure "The transfer simulation failed.")
              (Alpha_environment.wrap_error (Error errs)) in
      List.fold_left
        (fun acc (Internal_operation_result (_, r)) ->
           acc >>? fun () ->
           detect_script_failure r)
        (detect_script_failure operation_result)
        internal_operation_results in
    function
    | Single_result (Manager_operation_result _ as res) ->
        detect_script_failure_single res
    | Single_result _ ->
        Ok ()
    | Cons_result (res, rest) ->
        detect_script_failure_single res >>? fun () ->
        detect_script_failure rest in
  fun { contents } -> detect_script_failure contents


let may_patch_limits
    (type kind) (cctxt : #Proto_alpha.full) ~chain ~block ?branch
    ?src_sk (contents: kind contents_list) : kind contents_list tzresult Lwt.t =
  Alpha_services.Constants.all cctxt
    (chain, block) >>=? fun { parametric = {
      hard_gas_limit_per_operation = gas_limit ;
      hard_storage_limit_per_operation = storage_limit ;
    } } ->
  let may_need_patching_single
    : type kind. kind contents -> kind contents option = function
    | Manager_operation c
      when c.gas_limit < Z.zero || gas_limit < c.gas_limit
           || c.storage_limit < Z.zero || storage_limit < c.storage_limit ->
        let gas_limit =
          if c.gas_limit < Z.zero || gas_limit < c.gas_limit then
            gas_limit
          else
            c.gas_limit in
        let storage_limit =
          if c.storage_limit < Z.zero || storage_limit < c.storage_limit then
            storage_limit
          else
            c.storage_limit in
        Some (Manager_operation { c with gas_limit ; storage_limit })
    | _ -> None in
  let rec may_need_patching
    : type kind. kind contents_list -> kind contents_list option =
    function
    | Single (Manager_operation _ as c) -> begin
        match may_need_patching_single c with
        | None -> None
        | Some op -> Some (Single op)
      end
    | Single _ -> None
    | Cons (Manager_operation _ as c, rest) -> begin
        match may_need_patching_single c, may_need_patching rest with
        | None, None -> None
        | Some c, None -> Some (Cons (c, rest))
        | None, Some rest -> Some (Cons (c, rest))
        | Some c, Some rest -> Some (Cons (c, rest))
      end in

  let patch :
    type kind. kind contents * kind contents_result -> kind contents tzresult Lwt.t = function
    | Manager_operation c, (Manager_operation_result _ as result) ->
        begin
          if c.gas_limit < Z.zero || gas_limit < c.gas_limit then
            Lwt.return (estimated_gas_single result) >>=? fun gas ->
            begin
              if Z.equal gas Z.zero then
                cctxt#message "Estimated gas: none" >>= fun () ->
                return Z.zero
              else
                cctxt#message
                  "Estimated gas: %s units (will add 100 for safety)"
                  (Z.to_string gas) >>= fun () ->
                return (Z.add gas (Z.of_int 100))
            end
          else return c.gas_limit
        end >>=? fun gas_limit ->
        begin
          if c.storage_limit < Z.zero || storage_limit < c.storage_limit then
            Lwt.return (estimated_storage_single result) >>=? fun storage ->
            begin
              if Z.equal storage Z.zero then
                cctxt#message "Estimated storage: no bytes added" >>= fun () ->
                return Z.zero
              else
                cctxt#message
                  "Estimated storage: %s bytes added (will add 20 for safety)"
                  (Z.to_string storage) >>= fun () ->
                return (Z.add storage (Z.of_int 20))
            end
          else return c.storage_limit
        end >>=? fun storage_limit ->
        return (Manager_operation { c with gas_limit ; storage_limit })
    | (c, _) -> return c in
  let rec patch_list :
    type kind. kind contents_and_result_list -> kind contents_list tzresult Lwt.t =
    function
    | Single_and_result
        ((Manager_operation _ as op), (Manager_operation_result _ as res)) ->
        patch (op, res) >>=? fun op -> return (Single op)
    | Single_and_result (op, _) -> return (Single op)
    | Cons_and_result ((Manager_operation _ as op),
                       (Manager_operation_result _ as res), rest) -> begin
        patch (op, res) >>=? fun op ->
        patch_list rest >>=? fun rest ->
        return (Cons (op, rest))
      end in
  match may_need_patching contents with
  | Some contents ->
      preapply cctxt ~chain ~block
        ?branch ?src_sk contents >>=? fun (_, _, result) ->
      let res = pack_contents_list contents result.contents in
      patch_list res
  | None -> return contents

let inject_operation
    (type kind) cctxt ~chain ~block
    ?confirmations ?branch ?src_sk (contents: kind contents_list)  =
  may_patch_limits
    cctxt ~chain ~block ?branch ?src_sk contents >>=? fun contents ->
  preapply cctxt ~chain ~block
    ?branch ?src_sk contents >>=? fun (_oph, op, result) ->
  begin match detect_script_failure result with
    | Ok () -> return ()
    | Error _ as res ->
        cctxt#message
          "@[<v 2>This simulation failed:@,%a@]"
          Operation_result.pp_operation_result
          (op.protocol_data.contents, result.contents) >>= fun () ->
        Lwt.return res
  end >>=? fun () ->
  let bytes =
    Data_encoding.Binary.to_bytes_exn
      Operation.encoding (Operation.pack op) in
  Shell_services.Injection.operation cctxt ~chain bytes >>=? fun oph ->
  cctxt#message "Operation successfully injected in the node." >>= fun () ->
  cctxt#message "Operation hash is '%a'." Operation_hash.pp oph >>= fun () ->
  begin
    match confirmations with
    | None -> return result
    | Some confirmations ->
        cctxt#message "Waiting for the operation to be included..." >>= fun () ->
        Client_confirmations.wait_for_operation_inclusion
          ~confirmations cctxt ~chain oph >>=? fun (h, i , j) ->
        Alpha_block_services.Operation.operation
          cctxt ~block:(`Hash (h, 0)) i j >>=? fun op' ->
        match op'.receipt with
        | No_operation_metadata ->
            failwith "Internal error: unexpected receipt."
        | Operation_metadata receipt ->
            match Apply_operation_result.kind_equal_list contents receipt.contents
            with
            | Some Apply_operation_result.Eq ->
                return (receipt : kind operation_metadata)
            | None -> failwith "Internal error: unexpected receipt."
  end >>=? fun result ->
  cctxt#message
    "@[<v 2>This sequence of operations was run:@,%a@]"
    Operation_result.pp_operation_result
    (op.protocol_data.contents, result.contents) >>= fun () ->
  Lwt.return (originated_contracts result.contents) >>=? fun contracts ->
  Lwt_list.iter_s
    (fun c ->
       cctxt#message
         "New contract %a originated."
         Contract.pp c)
    contracts >>= fun () ->
  return (oph, op.protocol_data.contents, result.contents)

let inject_manager_operation
    cctxt ~chain ~block ?branch ?confirmations
    ~source ~src_pk ~src_sk ~fee ?(gas_limit = Z.minus_one) ?(storage_limit = (Z.of_int (-1)))
    (type kind) (operation : kind manager_operation)
  : (Operation_hash.t * kind Kind.manager contents *  kind Kind.manager contents_result) tzresult Lwt.t =
  Alpha_services.Contract.counter
    cctxt (chain, block) source >>=? fun pcounter ->
  let counter = Z.succ pcounter in
  Alpha_services.Contract.manager_key
    cctxt (chain, block) source >>=? fun (_, key) ->
  let is_reveal : type kind. kind manager_operation -> bool = function
    | Reveal _ -> true
    | _ -> false in
  match key with
  | None when not (is_reveal operation) -> begin
      let contents =
        Cons
          (Manager_operation { source ; fee = Tez.zero ; counter ;
                               gas_limit = Z.zero ; storage_limit = Z.zero ;
                               operation = Reveal src_pk },
           Single (Manager_operation { source ; fee ; counter = Z.succ counter ;
                                       gas_limit ; storage_limit ; operation })) in
      inject_operation cctxt ~chain ~block ?confirmations
        ?branch ~src_sk contents >>=? fun (oph, op, result) ->
      match pack_contents_list op result with
      | Cons_and_result (_, _, Single_and_result (op, result)) ->
          return (oph, op, result)
      | Single_and_result (Manager_operation _, _) -> .
      | _ -> assert false (* Grrr... *)
    end
  | _ ->
      let contents =
        Single (Manager_operation { source ; fee ; counter ;
                                    gas_limit ; storage_limit ; operation }) in
      inject_operation cctxt ~chain ~block ?confirmations
        ?branch ~src_sk contents >>=? fun (oph, op, result) ->
      match pack_contents_list op result with
      | Single_and_result (Manager_operation _ as op, result) ->
          return (oph, op, result)
      | _ -> assert false (* Grrr... *)
