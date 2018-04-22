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

type result = Operation_hash.t * operation * operation_result

let preapply
    (cctxt: #Proto_alpha.full) ~chain ~block
    ?branch ?src_sk contents =
  get_branch cctxt ~chain ~block branch >>=? fun branch ->
  let bytes =
    Data_encoding.Binary.to_bytes_exn
      Operation.unsigned_encoding
      ({ branch }, contents) in
  let watermark =
    match contents with
    | Sourced_operation (Consensus_operation (Endorsements _)) ->
        Signature.Endorsement
    | _ ->
        Signature.Generic_operation in
  begin
    match src_sk with
    | None -> return None
    | Some src_sk ->
        Client_keys.sign
          ~watermark src_sk bytes >>=? fun signature ->
        return (Some signature)
  end >>=? fun signature ->
  let op =
    { shell = { branch } ;
      protocol_data = { contents ; signature } } in
  let oph = Operation.hash op in
  Alpha_block_services.Helpers.Preapply.operations
    cctxt ~chain ~block [op] >>=? function
  | [result] -> return (oph, op, result)
  | _ -> failwith "Unexpected result"

let estimated_gas = function
  | Sourced_operation_result (Manager_operations_result { operation_results }) ->
      List.fold_left
        (fun acc (_, r) -> acc >>? fun acc ->
          match r with
          | Applied (Transaction_result { consumed_gas }
                    | Origination_result { consumed_gas }) ->
              Ok (Z.add consumed_gas acc)
          | Applied Reveal_result -> Ok acc
          | Applied Delegation_result -> Ok acc
          | Skipped -> assert false
          | Failed errs -> Alpha_environment.wrap_error (Error errs))
        (Ok Z.zero) operation_results
  | _ -> Ok Z.zero

let estimated_storage = function
  | Sourced_operation_result (Manager_operations_result { operation_results }) ->
      List.fold_left
        (fun acc (_, r) -> acc >>? fun acc ->
          match r with
          | Applied (Transaction_result { storage_size_diff }
                    | Origination_result { storage_size_diff }) ->
              Ok (Int64.add storage_size_diff acc)
          | Applied Reveal_result -> Ok acc
          | Applied Delegation_result -> Ok acc
          | Skipped -> assert false
          | Failed errs -> Alpha_environment.wrap_error (Error errs))
        (Ok 0L) operation_results >>? fun diff ->
      Ok (max 0L diff)
  | _ -> Ok 0L

let originated_contracts = function
  | Sourced_operation_result (Manager_operations_result { operation_results }) ->
      List.fold_left
        (fun acc (_, r) -> acc >>? fun acc ->
          match r with
          | Applied (Transaction_result { originated_contracts }
                    | Origination_result { originated_contracts }) ->
              Ok (originated_contracts @ acc)
          | Applied Reveal_result -> Ok acc
          | Applied Delegation_result -> Ok acc
          | Skipped -> assert false
          | Failed errs -> Alpha_environment.wrap_error (Error errs))
        (Ok []) operation_results
  | _ -> Ok []

let detect_script_failure = function
  | Sourced_operation_result (Manager_operations_result { operation_results }) ->
      List.fold_left
        (fun acc (_, r) -> acc >>? fun () ->
          match r with
          | Applied _ -> Ok ()
          | Skipped -> assert false
          | Failed errs ->
              record_trace
                (failure "The transfer simulation failed.")
                (Alpha_environment.wrap_error (Error errs)))
        (Ok ()) operation_results
  | _ -> Ok ()

let may_patch_limits
    (cctxt : #Proto_alpha.full) ~chain ~block ?branch
    ?src_sk contents =
  Alpha_services.Constants.hard_gas_limits cctxt (chain, block) >>=? fun (_, gas_limit) ->
  Alpha_services.Constants.hard_storage_limits cctxt (chain, block) >>=? fun (_, storage_limit) ->

  match contents with
  | Sourced_operation (Manager_operations c)
    when c.gas_limit < Z.zero || gas_limit < c.gas_limit
         || c.storage_limit < 0L || storage_limit < c.storage_limit ->
      let contents =
        Sourced_operation (Manager_operations { c with gas_limit ; storage_limit }) in
      preapply cctxt ~chain ~block ?branch ?src_sk contents >>=? fun (_, _, result) ->
      begin if c.gas_limit < Z.zero || gas_limit < c.gas_limit then
          Lwt.return (estimated_gas result) >>=? fun gas ->
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
      begin if c.storage_limit < 0L || storage_limit < c.storage_limit then
          Lwt.return (estimated_storage result) >>=? fun storage ->
          begin
            if Int64.equal storage 0L then
              cctxt#message "Estimated storage: no bytes added" >>= fun () ->
              return 0L
            else
              cctxt#message
                "Estimated storage: %Ld bytes added (will add 20 for safety)"
                storage >>= fun () ->
              return (Int64.add storage 20L)
          end
        else return c.storage_limit
      end >>=? fun storage_limit ->
      return (Sourced_operation (Manager_operations { c with gas_limit ; storage_limit }))
  | op -> return op

let inject_operation
    cctxt ~chain ~block
    ?confirmations ?branch ?src_sk contents =
  may_patch_limits
    cctxt ~chain ~block ?branch ?src_sk contents >>=? fun contents ->
  preapply cctxt ~chain ~block
    ?branch ?src_sk contents >>=? fun (_oph, op, result) ->
  begin match detect_script_failure result with
    | Ok () -> return ()
    | Error _ as res ->
        cctxt#message
          "@[<v 2>This simulation failed:@,%a@]"
          Operation_result.pp_operation_result (op, result) >>= fun () ->
        Lwt.return res
  end >>=? fun () ->
  let bytes = Data_encoding.Binary.to_bytes_exn Operation.encoding op in
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
          cctxt ~block:(`Hash (h, 0)) i j >>=? fun op ->
        return op.metadata
  end >>=? fun result ->
  cctxt#message
    "@[<v 2>This sequence of operations was run:@,%a@]"
    Operation_result.pp_operation_result (op, result) >>= fun () ->
  Lwt.return (originated_contracts result) >>=? fun contracts ->
  Lwt_list.iter_s
    (fun c ->
       cctxt#message
         "New contract %a originated."
         Contract.pp c)
    contracts >>= fun () ->
  return (oph, op, result)
