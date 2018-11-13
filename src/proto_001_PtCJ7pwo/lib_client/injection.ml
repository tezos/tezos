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

open Proto_001_PtCJ7pwo
open Alpha_context
open Apply_results

let get_branch (rpc_config: #Proto_001_PtCJ7pwo.full)
    ~chain ~(block : Block_services.block) branch =
  let branch = Option.unopt ~default:0 branch in (* TODO export parameter *)
  begin
    match block with
    | `Head n -> return (`Head (n+branch))
    | `Hash (h,n) -> return (`Hash (h,n+branch))
    | `Genesis -> return `Genesis
    | `Level i -> return (`Level i)
  end >>=? fun block ->
  Shell_services.Blocks.hash rpc_config ~chain ~block () >>=? fun hash ->
  Shell_services.Chain.chain_id rpc_config ~chain () >>=? fun chain_id ->
  return (chain_id, hash)

type 'kind preapply_result =
  Operation_hash.t * 'kind operation * 'kind operation_metadata

type 'kind result_list =
  Operation_hash.t * 'kind contents_list * 'kind contents_result_list

type 'kind result =
  Operation_hash.t * 'kind contents * 'kind contents_result

let preapply (type t)
    (cctxt: #Proto_001_PtCJ7pwo.full) ~chain ~block
    ?branch ?src_sk (contents : t contents_list) =
  get_branch cctxt ~chain ~block branch >>=? fun (chain_id, branch) ->
  let bytes =
    Data_encoding.Binary.to_bytes_exn
      Operation.unsigned_encoding
      ({ branch }, Contents_list contents) in
  begin
    match src_sk with
    | None -> return_none
    | Some src_sk ->
        begin match contents with
          | Single (Endorsement _) ->
              Client_keys.sign cctxt
                ~watermark:Signature.(Endorsement chain_id) src_sk bytes
          | _ ->
              Client_keys.sign cctxt
                ~watermark:Signature.Generic_operation src_sk bytes
        end >>=? fun signature ->
        return_some signature
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
            Apply_results.kind_equal_list contents result.contents with
      | Some Operation.Eq, Some Apply_results.Eq ->
          return ((oph, op, result) : t preapply_result)
      | _ -> failwith "Unexpected result"
    end
  | _ -> failwith "Unexpected result"

let simulate (type t)
    (cctxt: #Proto_001_PtCJ7pwo.full) ~chain ~block
    ?branch (contents : t contents_list) =
  get_branch cctxt ~chain ~block branch >>=? fun (_chain_id, branch) ->
  let op : _ Operation.t =
    { shell = { branch } ;
      protocol_data = { contents ; signature = None } } in
  let oph = Operation.hash op in
  Alpha_services.Helpers.Scripts.run_operation
    cctxt (chain, block) (Operation.pack op) >>=? function
  | (Operation_data op', Operation_metadata result) -> begin
      match Operation.equal
              op { shell = { branch } ; protocol_data = op' },
            Apply_results.kind_equal_list contents result.contents with
      | Some Operation.Eq, Some Apply_results.Eq ->
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
    | Backtracked (_, None) -> Ok Z.zero (* there must be another error for this to happen *)
    | Backtracked (_, Some errs) -> Alpha_environment.wrap_error (Error errs)
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
    | Applied (Transaction_result { paid_storage_size_diff }) -> Ok paid_storage_size_diff
    | Applied (Origination_result { paid_storage_size_diff }) -> Ok paid_storage_size_diff
    | Applied Reveal_result -> Ok Z.zero
    | Applied Delegation_result -> Ok Z.zero
    | Skipped _ -> assert false
    | Backtracked (_, None) -> Ok Z.zero (* there must be another error for this to happen *)
    | Backtracked (_, Some errs) -> Alpha_environment.wrap_error (Error errs)
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
  Ok (Z.max Z.zero diff)

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
    | Backtracked (_, None) -> Ok [] (* there must be another error for this to happen *)
    | Backtracked (_, Some errs) -> Alpha_environment.wrap_error (Error errs)
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
        | Backtracked (_, None) -> (* there must be another error for this to happen *)
            Ok ()
        | Backtracked (_, Some errs) ->
            record_trace
              (failure "The transfer simulation failed.")
              (Alpha_environment.wrap_error (Error errs))
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
    (type kind) (cctxt : #Proto_001_PtCJ7pwo.full) ~chain ~block ?branch
    (contents: kind contents_list) : kind contents_list tzresult Lwt.t =
  Alpha_services.Constants.all cctxt
    (chain, block) >>=? fun { parametric = {
      hard_gas_limit_per_operation = gas_limit ;
      hard_storage_limit_per_operation = storage_limit ;
    } } ->
  let may_need_patching_single
    : type kind. kind contents -> kind contents option = function
    | Manager_operation c
      when c.gas_limit < Z.zero || gas_limit <= c.gas_limit
           || c.storage_limit < Z.zero || storage_limit <= c.storage_limit ->
        let gas_limit =
          if c.gas_limit < Z.zero || gas_limit <= c.gas_limit then
            gas_limit
          else
            c.gas_limit in
        let storage_limit =
          if c.storage_limit < Z.zero || storage_limit <= c.storage_limit then
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
          if c.gas_limit < Z.zero || gas_limit <= c.gas_limit then
            Lwt.return (estimated_gas_single result) >>=? fun gas ->
            begin
              if Z.equal gas Z.zero then
                cctxt#message "Estimated gas: none" >>= fun () ->
                return Z.zero
              else
                cctxt#message
                  "Estimated gas: %s units (will add 100 for safety)"
                  (Z.to_string gas) >>= fun () ->
                return (Z.min (Z.add gas (Z.of_int 100)) gas_limit)
            end
          else return c.gas_limit
        end >>=? fun gas_limit ->
        begin
          if c.storage_limit < Z.zero || storage_limit <= c.storage_limit then
            Lwt.return (estimated_storage_single result) >>=? fun storage ->
            begin
              if Z.equal storage Z.zero then
                cctxt#message "Estimated storage: no bytes added" >>= fun () ->
                return Z.zero
              else
                cctxt#message
                  "Estimated storage: %s bytes added (will add 20 for safety)"
                  (Z.to_string storage) >>= fun () ->
                return (Z.min (Z.add storage (Z.of_int 20)) storage_limit)
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
      simulate cctxt ~chain ~block ?branch contents >>=? fun (_, _, result) ->
      begin match detect_script_failure result with
        | Ok () -> return_unit
        | Error _ ->
            cctxt#message
              "@[<v 2>This simulation failed:@,%a@]"
              Operation_result.pp_operation_result
              (contents, result.contents) >>= fun () ->
            return_unit
      end >>=? fun () ->
      let res = pack_contents_list contents result.contents in
      patch_list res
  | None -> return contents

let inject_operation
    (type kind) cctxt ~chain ~block
    ?confirmations
    ?(dry_run = false)
    ?branch ?src_sk
    (contents: kind contents_list)  =
  Client_confirmations.wait_for_bootstrapped cctxt >>=? fun () ->
  may_patch_limits
    cctxt ~chain ~block ?branch contents >>=? fun contents ->
  preapply cctxt ~chain ~block
    ?branch ?src_sk contents >>=? fun (_oph, op, result) ->
  begin match detect_script_failure result with
    | Ok () -> return_unit
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
  if dry_run then
    let oph = Operation_hash.hash_bytes [bytes] in
    cctxt#message
      "@[<v 0>Operation: 0x%a@,\
       Operation hash: %a@]"
      MBytes.pp_hex bytes
      Operation_hash.pp oph >>= fun () ->
    cctxt#message
      "@[<v 2>Simulation result:@,%a@]"
      Operation_result.pp_operation_result
      (op.protocol_data.contents, result.contents) >>= fun () ->
    return (oph, op.protocol_data.contents, result.contents)
  else
    Shell_services.Injection.operation cctxt ~chain bytes >>=? fun oph ->
    cctxt#message "Operation successfully injected in the node." >>= fun () ->
    cctxt#message "Operation hash: %a" Operation_hash.pp oph >>= fun () ->
    begin
      match confirmations with
      | None ->
          cctxt#message "@[<v 0>NOT waiting for the operation to be included.@,\
                         Use command@,\
                        \  tezos-client wait for %a to be included --confirmations 30@,\
                         and/or an external block explorer to make sure that it has been included.@]"
            Operation_hash.pp oph >>= fun () ->
          return result
      | Some confirmations ->
          cctxt#message "Waiting for the operation to be included..." >>= fun () ->
          Client_confirmations.wait_for_operation_inclusion
            ~confirmations cctxt ~chain oph >>=? fun (h, i , j) ->
          Alpha_block_services.Operations.operation
            cctxt ~block:(`Hash (h, 0)) i j >>=? fun op' ->
          match op'.receipt with
          | No_operation_metadata ->
              failwith "Internal error: unexpected receipt."
          | Operation_metadata receipt ->
              match Apply_results.kind_equal_list contents receipt.contents
              with
              | Some Apply_results.Eq ->
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
    begin match confirmations with
      | None -> Lwt.return_unit
      | Some number ->
          if number >= 30 then
            cctxt#message
              "The operation was included in a block %d blocks ago."
              number
          else
            cctxt#message
              "@[<v 0>The operation has only been included %d blocks ago.@,\
               We recommend to wait more.@,\
               Use command@,\
              \  tezos-client wait for %a to be included --confirmations 30@,\
               and/or an external block explorer.@]"
              number Operation_hash.pp oph
    end >>= fun () ->
    return (oph, op.protocol_data.contents, result.contents)

let inject_manager_operation
    cctxt ~chain ~block ?branch ?confirmations ?dry_run
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
      inject_operation cctxt ~chain ~block ?confirmations ?dry_run
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
      inject_operation cctxt ~chain ~block ?confirmations ?dry_run
        ?branch ~src_sk contents >>=? fun (oph, op, result) ->
      match pack_contents_list op result with
      | Single_and_result (Manager_operation _ as op, result) ->
          return (oph, op, result)
      | _ -> assert false (* Grrr... *)
