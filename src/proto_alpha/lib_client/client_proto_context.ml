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
open Tezos_micheline
open Client_proto_contracts
open Client_keys

let get_balance (rpc : #Proto_alpha.rpc_context) block contract =
  Alpha_services.Contract.balance rpc block contract

let get_storage (rpc : #Proto_alpha.rpc_context) block contract =
  Alpha_services.Contract.storage_opt rpc block contract

let rec find_predecessor rpc_config h n =
  if n <= 0 then
    return (`Hash h)
  else
    Block_services.predecessor rpc_config (`Hash h) >>=? fun h ->
    find_predecessor rpc_config h (n-1)

let get_branch rpc_config block branch =
  let branch = Option.unopt ~default:0 branch in (* TODO export parameter *)
  let block = Block_services.last_baked_block block in
  begin
    match block with
    | `Head n -> return (`Head (n+branch))
    | `Test_head n -> return (`Test_head (n+branch))
    | `Hash h -> find_predecessor rpc_config h branch
    | `Genesis -> return `Genesis
  end >>=? fun block ->
  Block_services.info rpc_config block >>=? fun { net_id ; hash } ->
  return (net_id, hash)

let parse_expression arg =
  Lwt.return
    (Micheline_parser.no_parsing_error
       (Michelson_v1_parser.parse_expression arg))

let transfer rpc_config
    block ?branch
    ~source ~src_pk ~src_sk ~destination ?arg ~amount ~fee () =
  get_branch rpc_config block branch >>=? fun (net_id, branch) ->
  begin match arg with
    | Some arg ->
        parse_expression arg >>=? fun { expanded = arg } ->
        return (Some arg)
    | None -> return None
  end >>=? fun parameters ->
  Alpha_services.Contract.counter
    rpc_config block source >>=? fun pcounter ->
  let counter = Int32.succ pcounter in
  Alpha_services.Forge.Manager.transaction
    rpc_config block
    ~branch ~source ~sourcePubKey:src_pk ~counter ~amount
    ~destination ?parameters ~fee () >>=? fun bytes ->
  Block_services.predecessor rpc_config block >>=? fun predecessor ->
  Client_keys.sign src_sk bytes >>=? fun signature ->
  let signed_bytes =
    MBytes.concat bytes (Ed25519.Signature.to_bytes signature) in
  let oph = Operation_hash.hash_bytes [ signed_bytes ] in
  Alpha_services.Helpers.apply_operation rpc_config block
    predecessor oph bytes (Some signature) >>=? fun contracts ->
  Shell_services.inject_operation
    rpc_config ~net_id signed_bytes >>=? fun injected_oph ->
  assert (Operation_hash.equal oph injected_oph) ;
  return (oph, contracts)

let originate rpc_config ?net_id ~block ?signature bytes =
  let signed_bytes =
    match signature with
    | None -> bytes
    | Some signature -> Ed25519.Signature.concat bytes signature in
  Block_services.predecessor rpc_config block >>=? fun predecessor ->
  let oph = Operation_hash.hash_bytes [ signed_bytes ] in
  Alpha_services.Helpers.apply_operation rpc_config block
    predecessor oph bytes signature >>=? function
  | [ contract ] ->
      Shell_services.inject_operation
        rpc_config ?net_id signed_bytes >>=? fun injected_oph ->
      assert (Operation_hash.equal oph injected_oph) ;
      return (oph, contract)
  | contracts ->
      failwith
        "The origination introduced %d contracts instead of one."
        (List.length contracts)

let operation_submitted_message (cctxt : #Client_context.logger) ?(contracts = []) oph =
  cctxt#message "Operation successfully injected in the node." >>= fun () ->
  cctxt#message "Operation hash is '%a'." Operation_hash.pp oph >>= fun () ->
  Lwt_list.iter_s
    (fun c ->
       cctxt#message
         "New contract %a originated from a smart contract."
         Contract.pp c)
    contracts >>= return

let originate_account ?branch
    ~source ~src_pk ~src_sk ~manager_pkh
    ?delegatable ?delegate ~balance ~fee block rpc_config () =
  get_branch rpc_config block branch >>=? fun (net_id, branch) ->
  Alpha_services.Contract.counter
    rpc_config block source >>=? fun pcounter ->
  let counter = Int32.succ pcounter in
  Alpha_services.Forge.Manager.origination rpc_config block
    ~branch ~source ~sourcePubKey:src_pk ~managerPubKey:manager_pkh
    ~counter ~balance ~spendable:true
    ?delegatable ?delegatePubKey:delegate ~fee () >>=? fun bytes ->
  Client_keys.sign src_sk bytes >>=? fun signature ->
  originate rpc_config ~block ~net_id ~signature bytes

let faucet ?branch ~manager_pkh block rpc_config () =
  get_branch rpc_config block branch >>=? fun (net_id, branch) ->
  let nonce = Rand.generate Constants_repr.nonce_length in
  Alpha_services.Forge.Anonymous.faucet
    rpc_config block ~branch ~id:manager_pkh ~nonce () >>=? fun bytes ->
  originate rpc_config ~net_id ~block bytes

let delegate_contract rpc_config
    block ?branch
    ~source ?src_pk ~manager_sk
    ~fee delegate_opt =
  get_branch rpc_config block branch >>=? fun (net_id, branch) ->
  Alpha_services.Contract.counter
    rpc_config block source >>=? fun pcounter ->
  let counter = Int32.succ pcounter in
  Alpha_services.Forge.Manager.delegation rpc_config block
    ~branch ~source ?sourcePubKey:src_pk ~counter ~fee delegate_opt
  >>=? fun bytes ->
  Client_keys.sign manager_sk bytes >>=? fun signature ->
  let signed_bytes = Ed25519.Signature.concat bytes signature in
  let oph = Operation_hash.hash_bytes [ signed_bytes ] in
  Shell_services.inject_operation
    rpc_config ~net_id signed_bytes >>=? fun injected_oph ->
  assert (Operation_hash.equal oph injected_oph) ;
  return oph

let list_contract_labels (cctxt : #Proto_alpha.full_context) block =
  Alpha_services.Contract.list
    cctxt block >>=? fun contracts ->
  map_s (fun h ->
      begin match Contract.is_default h with
        | Some m -> begin
            Public_key_hash.rev_find cctxt m >>=? function
            | None -> return ""
            | Some nm ->
                RawContractAlias.find_opt cctxt nm >>=? function
                | None -> return (" (known as " ^ nm ^ ")")
                | Some _ -> return (" (known as key:" ^ nm ^ ")")
          end
        | None -> begin
            RawContractAlias.rev_find cctxt h >>=? function
            | None -> return ""
            | Some nm ->  return (" (known as " ^ nm ^ ")")
          end
      end >>=? fun nm ->
      let kind = match Contract.is_default h with
        | Some _ -> " (default)"
        | None -> "" in
      let h_b58 = Contract.to_b58check h in
      return (nm, h_b58, kind))
    contracts

let message_added_contract (cctxt : #Proto_alpha.full_context) name =
  cctxt#message "Contract memorized as %s." name

let get_manager (cctxt : #Proto_alpha.full_context) block source =
  Client_proto_contracts.get_manager
    cctxt block source >>=? fun src_pkh ->
  Client_keys.get_key cctxt src_pkh >>=? fun (src_name, src_pk, src_sk) ->
  return (src_name, src_pkh, src_pk, src_sk)

let dictate rpc_config block command seckey =
  let block = Block_services.last_baked_block block in
  Block_services.info
    rpc_config block >>=? fun { net_id ; hash = branch } ->
  Alpha_services.Forge.Dictator.operation
    rpc_config block ~branch command >>=? fun bytes ->
  let signature = Ed25519.sign seckey bytes in
  let signed_bytes = Ed25519.Signature.concat bytes signature in
  let oph = Operation_hash.hash_bytes [ signed_bytes ] in
  Shell_services.inject_operation
    rpc_config ~net_id signed_bytes >>=? fun injected_oph ->
  assert (Operation_hash.equal oph injected_oph) ;
  return oph

let set_delegate (cctxt : #RPC_context.simple) block ~fee contract ~src_pk ~manager_sk opt_delegate =
  delegate_contract
    cctxt block ~source:contract
    ~src_pk ~manager_sk ~fee opt_delegate

let source_to_keys (wallet : #Proto_alpha.full_context) block source =
  get_manager wallet block source >>=? fun (_src_name, _src_pkh, src_pk, src_sk) ->
  return (src_pk, src_sk)

let save_contract ~force cctxt alias_name contract =
  RawContractAlias.add ~force cctxt alias_name contract >>=? fun () ->
  message_added_contract cctxt alias_name >>= fun () ->
  return ()

let originate_contract
    ~fee
    ~delegate
    ?(delegatable=true)
    ?(spendable=false)
    ~initial_storage
    ~manager
    ~balance
    ~source
    ~src_pk
    ~src_sk
    ~code
    (cctxt : #Proto_alpha.full_context) =
  Lwt.return (Michelson_v1_parser.parse_expression initial_storage) >>= fun result ->
  Lwt.return (Micheline_parser.no_parsing_error result) >>=?
  fun { Michelson_v1_parser.expanded = storage } ->
  let block = cctxt#block in
  Alpha_services.Contract.counter
    cctxt block source >>=? fun pcounter ->
  let counter = Int32.succ pcounter in
  get_branch cctxt block None >>=? fun (_net_id, branch) ->
  Alpha_services.Forge.Manager.origination cctxt block
    ~branch ~source ~sourcePubKey:src_pk ~managerPubKey:manager
    ~counter ~balance ~spendable:spendable
    ~delegatable ?delegatePubKey:delegate
    ~script:{ code ; storage } ~fee () >>=? fun bytes ->
  Client_keys.sign src_sk bytes >>=? fun signature ->
  originate cctxt ~block ~signature bytes
