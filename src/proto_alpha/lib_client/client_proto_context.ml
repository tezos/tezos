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

let get_branch rpc_config block branch =
  let branch = Option.unopt ~default:0 branch in (* TODO export parameter *)
  begin
    match block with
    | `Head n -> return (`Head (n+branch))
    | `Test_head n -> return (`Test_head (n+branch))
    | `Hash (h,n) -> return (`Hash (h,n+branch))
    | `Genesis -> return `Genesis
  end >>=? fun block ->
  Block_services.info rpc_config block >>=? fun { chain_id ; hash } ->
  return (chain_id, hash)

let parse_expression arg =
  Lwt.return
    (Micheline_parser.no_parsing_error
       (Michelson_v1_parser.parse_expression arg))

let pp_internal_operation ppf { source ; operation } =
  Format.fprintf ppf "@[<v 0>" ;
  begin match operation with
    | Alpha_context.Transaction { destination ; amount ; parameters } ->
        Format.fprintf ppf
          "@[<v 2>Transaction:@,\
           Of: %a@,\
           From: %a@,\
           To: %a"
          Tez.pp amount
          Contract.pp source
          Contract.pp destination ;
        begin match parameters with
          | None -> ()
          | Some expr ->
              Format.fprintf ppf
                "@,Parameter: @[<v 0>%a@]"
                Michelson_v1_printer.print_expr expr
        end ;
        Format.fprintf ppf "@]" ;
    | Origination { manager ; delegate ; credit ; spendable ; delegatable ; script } ->
        Format.fprintf ppf "@[<v 2>Origination:@,\
                            From: %a@,\
                            For: %a@,\
                            Credit: %a"
          Contract.pp source
          Signature.Public_key_hash.pp manager
          Tez.pp credit ;
        begin match script with
          | None -> Format.fprintf ppf "@,No script (accepts all transactions)"
          | Some { code ; storage } ->
              Format.fprintf ppf
                "@,@[<v 2>Script:@,%a\
                 @,@[<v 2>Initial storage:@,%a@]"
                Michelson_v1_printer.print_expr code
                Michelson_v1_printer.print_expr storage
        end ;
        begin match delegate with
          | None -> Format.fprintf ppf "@,Delegate is the manager"
          | Some delegate -> Format.fprintf ppf "@,Delegate: %a" Signature.Public_key_hash.pp delegate
        end ;
        if spendable then Format.fprintf ppf "@,Spendable by its manager" ;
        if delegatable then Format.fprintf ppf "@,Delegate can be changed later" ;
        Format.fprintf ppf "@]" ;
    | Reveal key ->
        Format.fprintf ppf
          "@[<v 2>Revelation of manager public key:@,\
           Contract: %a@,\
           Key: %a@]"
          Contract.pp source
          Signature.Public_key.pp key
    | Delegation None ->
        Format.fprintf ppf
          "@[<v 2>Delegation:@,\
           Contract: %a@,\
           To: nobody@]"
          Contract.pp source
    | Delegation (Some delegate) ->
        Format.fprintf ppf
          "@[<v 2>Delegation:@,\
           Contract: %a@,\
           To: %a@]"
          Contract.pp source
          Signature.Public_key_hash.pp delegate
  end ;
  Format.fprintf ppf "@]"

let transfer (cctxt : #Proto_alpha.full)
    block ?branch
    ~source ~src_pk ~src_sk ~destination ?arg ~amount ~fee ?gas_limit () =
  get_branch cctxt block branch >>=? fun (chain_id, branch) ->
  begin match arg with
    | Some arg ->
        parse_expression arg >>=? fun { expanded = arg } ->
        return (Some arg)
    | None -> return None
  end >>=? fun parameters ->
  Alpha_services.Contract.counter
    cctxt block source >>=? fun pcounter ->
  let counter = Int32.succ pcounter in
  Block_services.predecessor cctxt block >>=? fun predecessor ->
  begin match gas_limit with
    | Some gas_limit -> return gas_limit
    | None ->
        Alpha_services.Constants.hard_gas_limits cctxt block >>=? fun (_, max_gas) ->
        Alpha_services.Forge.Manager.transaction
          cctxt block
          ~branch ~source ~sourcePubKey:src_pk ~counter ~amount
          ~destination ?parameters ~fee ~gas_limit:max_gas () >>=? fun bytes ->
        Client_keys.sign
          src_sk ~watermark:Generic_operation bytes >>=? fun signature ->
        let signed_bytes = Signature.concat bytes signature in
        let oph = Operation_hash.hash_bytes [ signed_bytes ] in
        Alpha_services.Helpers.apply_operation cctxt block
          predecessor oph bytes (Some signature) >>=? fun (_, _, gas) ->
        match gas with
        | Limited { remaining } ->
            let gas = Z.sub max_gas remaining in
            if Z.equal gas Z.zero then
              cctxt#message "Estimated gas: none" >>= fun () ->
              return Z.zero
            else
              let gas = Z.sub max_gas remaining in
              cctxt#message "Estimated gas: %s units (will add 100 for safety)" (Z.to_string gas) >>= fun () ->
              return (Z.add gas (Z.of_int 100))
        | Unaccounted -> assert false
  end >>=? fun gas_limit ->
  Alpha_services.Forge.Manager.transaction
    cctxt block
    ~branch ~source ~sourcePubKey:src_pk ~counter ~amount
    ~destination ?parameters ~fee ~gas_limit () >>=? fun bytes ->
  Client_keys.sign
    src_sk ~watermark:Generic_operation bytes >>=? fun signature ->
  let signed_bytes = Signature.concat bytes signature in
  let oph = Operation_hash.hash_bytes [ signed_bytes ] in
  Alpha_services.Helpers.apply_operation cctxt block
    predecessor oph bytes (Some signature) >>=? fun (contracts, operations, _) ->
  cctxt#message "@[<v 2>This sequence of operations was run (including internal ones):@,%a@]"
    (Format.pp_print_list pp_internal_operation) operations >>= fun () ->
  Shell_services.inject_operation
    cctxt ~chain_id signed_bytes >>=? fun injected_oph ->
  assert (Operation_hash.equal oph injected_oph) ;
  return (oph, contracts)

let reveal cctxt
    block ?branch ~source ~src_pk ~src_sk ~fee () =
  get_branch cctxt block branch >>=? fun (chain_id, branch) ->
  Alpha_services.Contract.counter cctxt block source >>=? fun pcounter ->
  let counter = Int32.succ pcounter in
  Alpha_services.Forge.Manager.reveal
    cctxt block
    ~branch ~source ~sourcePubKey:src_pk ~counter ~fee () >>=? fun bytes ->
  Client_keys.sign
    src_sk ~watermark:Generic_operation bytes >>=? fun signature ->
  let signed_bytes = Signature.concat bytes signature in
  let oph = Operation_hash.hash_bytes [ signed_bytes ] in
  Shell_services.inject_operation
    cctxt ~chain_id signed_bytes >>=? fun injected_oph ->
  assert (Operation_hash.equal oph injected_oph) ;
  return oph

let originate (cctxt : #Client_context.full) ?chain_id ~block ?signature bytes =
  let signed_bytes =
    match signature with
    | None -> bytes
    | Some signature -> Signature.concat bytes signature in
  Block_services.predecessor cctxt block >>=? fun predecessor ->
  let oph = Operation_hash.hash_bytes [ signed_bytes ] in
  Alpha_services.Helpers.apply_operation cctxt block
    predecessor oph bytes signature >>=? function
  | [ contract ], operations, _ ->
      cctxt#message
        "@[<v 2>This sequence of operations was run (including internal ones):@,%a@]"
        (Format.pp_print_list pp_internal_operation) operations >>= fun () ->
      Shell_services.inject_operation
        cctxt ?chain_id signed_bytes >>=? fun injected_oph ->
      assert (Operation_hash.equal oph injected_oph) ;
      return (oph, contract)
  | contracts, _, _ ->
      failwith
        "The origination introduced %d contracts instead of one."
        (List.length contracts)

let operation_submitted_message (cctxt : #Client_context.printer) ?(contracts = []) oph =
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
    ?delegatable ?delegate ~balance ~fee block cctxt () =
  get_branch cctxt block branch >>=? fun (chain_id, branch) ->
  Alpha_services.Contract.counter
    cctxt block source >>=? fun pcounter ->
  let counter = Int32.succ pcounter in
  Alpha_services.Forge.Manager.origination cctxt block
    ~branch ~source ~sourcePubKey:src_pk ~managerPubKey:manager_pkh
    ~counter ~balance ~spendable:true
    ?delegatable ?delegatePubKey:delegate ~fee ~gas_limit:Z.zero () >>=? fun bytes ->
  Client_keys.sign
    src_sk ~watermark:Generic_operation bytes >>=? fun signature ->
  originate cctxt ~block ~chain_id ~signature bytes

let delegate_contract cctxt
    block ?branch
    ~source ?src_pk ~manager_sk
    ~fee delegate_opt =
  get_branch cctxt block branch >>=? fun (chain_id, branch) ->
  Alpha_services.Contract.counter
    cctxt block source >>=? fun pcounter ->
  let counter = Int32.succ pcounter in
  Alpha_services.Forge.Manager.delegation cctxt block
    ~branch ~source ?sourcePubKey:src_pk ~counter ~fee delegate_opt
  >>=? fun bytes ->
  Client_keys.sign
    manager_sk ~watermark:Generic_operation bytes >>=? fun signature ->
  let signed_bytes = Signature.concat bytes signature in
  let oph = Operation_hash.hash_bytes [ signed_bytes ] in
  Shell_services.inject_operation
    cctxt ~chain_id signed_bytes >>=? fun injected_oph ->
  assert (Operation_hash.equal oph injected_oph) ;
  return oph

let list_contract_labels (cctxt : #Proto_alpha.full) block =
  Alpha_services.Contract.list
    cctxt block >>=? fun contracts ->
  map_s (fun h ->
      begin match Contract.is_implicit h with
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
      let kind = match Contract.is_implicit h with
        | Some _ -> " (implicit)"
        | None -> "" in
      let h_b58 = Contract.to_b58check h in
      return (nm, h_b58, kind))
    contracts

let message_added_contract (cctxt : #Proto_alpha.full) name =
  cctxt#message "Contract memorized as %s." name

let get_manager (cctxt : #Proto_alpha.full) block source =
  Client_proto_contracts.get_manager
    cctxt block source >>=? fun src_pkh ->
  Client_keys.get_key cctxt src_pkh >>=? fun (src_name, src_pk, src_sk) ->
  return (src_name, src_pkh, src_pk, src_sk)

let dictate rpc_config block command seckey =
  Block_services.info
    rpc_config block >>=? fun { chain_id ; hash = branch } ->
  Alpha_services.Forge.Dictator.operation
    rpc_config block ~branch command >>=? fun bytes ->
  let signed_bytes =
    Signature.append ~watermark:Generic_operation seckey bytes in
  let oph = Operation_hash.hash_bytes [ signed_bytes ] in
  Shell_services.inject_operation
    rpc_config ~chain_id signed_bytes >>=? fun injected_oph ->
  assert (Operation_hash.equal oph injected_oph) ;
  return oph

let set_delegate cctxt block ~fee contract ~src_pk ~manager_sk opt_delegate =
  delegate_contract
    cctxt block ~source:contract ~src_pk ~manager_sk ~fee opt_delegate

let register_as_delegate cctxt block ~fee ~manager_sk src_pk =
  let source = Signature.Public_key.hash src_pk in
  delegate_contract
    cctxt block
    ~source:(Contract.implicit_contract source) ~src_pk ~manager_sk ~fee
    (Some source)

let source_to_keys (wallet : #Proto_alpha.full) block source =
  get_manager wallet block source >>=? fun (_src_name, _src_pkh, src_pk, src_sk) ->
  return (src_pk, src_sk)

let save_contract ~force cctxt alias_name contract =
  RawContractAlias.add ~force cctxt alias_name contract >>=? fun () ->
  message_added_contract cctxt alias_name >>= fun () ->
  return ()

let originate_contract
    ~fee
    ?gas_limit
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
    (cctxt : #Proto_alpha.full) =
  Lwt.return (Michelson_v1_parser.parse_expression initial_storage) >>= fun result ->
  Lwt.return (Micheline_parser.no_parsing_error result) >>=?
  fun { Michelson_v1_parser.expanded = storage } ->
  let block = cctxt#block in
  Alpha_services.Contract.counter
    cctxt block source >>=? fun pcounter ->
  let counter = Int32.succ pcounter in
  get_branch cctxt block None >>=? fun (_chain_id, branch) ->
  Block_services.predecessor cctxt block >>=? fun predecessor ->
  begin match gas_limit with
    | Some gas_limit -> return gas_limit
    | None ->
        Alpha_services.Constants.hard_gas_limits cctxt block >>=? fun (_, max_gas) ->
        Alpha_services.Forge.Manager.origination cctxt block
          ~branch ~source ~sourcePubKey:src_pk ~managerPubKey:manager
          ~counter ~balance ~spendable:spendable
          ~delegatable ?delegatePubKey:delegate
          ~script:{ code ; storage } ~fee  ~gas_limit:max_gas () >>=? fun bytes ->
        Client_keys.sign
          ~watermark:Generic_operation src_sk bytes >>=? fun signature ->
        let signed_bytes = Signature.concat bytes signature in
        let oph = Operation_hash.hash_bytes [ signed_bytes ] in
        Alpha_services.Helpers.apply_operation cctxt block
          predecessor oph bytes (Some signature) >>=? fun (_, _, gas) ->
        match gas with
        | Limited { remaining } ->
            let gas = Z.sub max_gas remaining in
            if Z.equal gas Z.zero then
              cctxt#message "Estimated gas: none" >>= fun () ->
              return Z.zero
            else
              cctxt#message "Estimated gas: %s units (will add 100 for safety)" (Z.to_string gas) >>= fun () ->
              return (Z.add gas (Z.of_int 100))
        | Unaccounted -> assert false
  end >>=? fun gas_limit ->
  Alpha_services.Forge.Manager.origination cctxt block
    ~branch ~source ~sourcePubKey:src_pk ~managerPubKey:manager
    ~counter ~balance ~spendable:spendable
    ~delegatable ?delegatePubKey:delegate
    ~script:{ code ; storage } ~fee ~gas_limit () >>=? fun bytes ->
  Client_keys.sign
    src_sk ~watermark:Generic_operation bytes >>=? fun signature ->
  originate cctxt ~block ~signature bytes

let wait_for_operation_inclusion
    (ctxt : #Proto_alpha.full)
    ?(predecessors = 10)
    ?(confirmations = 1)
    operation_hash =
  let confirmed_blocks = Hashtbl.create confirmations in
  Block_services.monitor ctxt ~length:predecessors >>=? fun (stream, stop) ->
  let stream = Lwt_stream.flatten @@ Lwt_stream.flatten @@ stream in
  Lwt_stream.find_s begin fun bi ->
    match Hashtbl.find_opt confirmed_blocks bi.Block_services.predecessor with
    | Some n ->
        ctxt#answer
          "Operation received %d confirmations as of block: %a"
          (n+1) Block_hash.pp bi.hash >>= fun () ->
        if n+1 < confirmations then begin
          Hashtbl.add confirmed_blocks bi.hash (n+1) ;
          Lwt.return_false
        end else
          Lwt.return_true
    | None ->
        Block_services.operations ctxt (`Hash (bi.hash, 0)) >>= fun operations ->
        let in_block =
          match operations with
          | Error _ -> false
          | Ok operations ->
              List.exists
                (List.exists
                   (fun (hash, _) ->
                      Operation_hash.equal operation_hash hash))
                operations in
        if not in_block then
          Lwt.return_false
        else begin
          ctxt#answer
            "Operation found in block: %a"
            Block_hash.pp bi.hash >>= fun () ->
          if confirmations <= 0 then
            Lwt.return_true
          else begin
            Hashtbl.add confirmed_blocks bi.hash 0 ;
            Lwt.return_false
          end
        end
  end stream >>= fun _ ->
  stop () ;
  return ()

type activation_key =
  { pkh : Ed25519.Public_key_hash.t ;
    amount : Tez.t ;
    secret : Blinded_public_key_hash.secret ;
    mnemonic : string list ;
    password : string ;
    email : string ;
  }

let activation_key_encoding =
  let open Data_encoding in
  conv
    (fun { pkh ; amount ; secret ; mnemonic ; password ; email } ->
       ( pkh, amount, secret, mnemonic, password, email ))
    (fun ( pkh, amount, secret, mnemonic, password, email ) ->
       { pkh ; amount ; secret ; mnemonic ; password ; email })
    (obj6
       (req "pkh" Ed25519.Public_key_hash.encoding)
       (req "amount" Tez.encoding)
       (req "secret" Blinded_public_key_hash.secret_encoding)
       (req "mnemonic" (list string))
       (req "password" string)
       (req "email" string))

let read_key key =
  match Bip39.of_words key.mnemonic with
  | None ->
      failwith ""
  | Some t ->
      (* TODO: unicode normalization (NFKD)... *)
      let sk = Bip39.to_seed ~passphrase:(key.email ^ key.password) t in
      let sk = Cstruct.(to_bigarray (sub sk 0 32)) in
      let sk : Signature.Secret_key.t =
        Ed25519 (Data_encoding.Binary.of_bytes_exn Ed25519.Secret_key.encoding sk) in
      let pk = Signature.Secret_key.to_public_key sk in
      let pkh = Signature.Public_key.hash pk in
      return (pkh, pk, sk)

let claim_commitment (cctxt : #Proto_alpha.full)
    ?(encrypted = false) ?confirmations ?force block key name =
  read_key key >>=? fun (pkh, pk, sk) ->
  fail_unless (Signature.Public_key_hash.equal pkh (Ed25519 key.pkh))
    (failure "@[<v 2>Inconsistent activation key:@ \
              Computed pkh: %a@ \
              Embedded pkh: %a @]"
       Signature.Public_key_hash.pp pkh
       Ed25519.Public_key_hash.pp key.pkh) >>=? fun () ->
  let op = [ Activation { id = key.pkh ; secret = key.secret } ] in
  Block_services.info cctxt block >>=? fun bi ->
  Alpha_services.Forge.Anonymous.operations
    cctxt block ~branch:bi.hash op >>=? fun bytes ->
  Shell_services.inject_operation
    cctxt ~chain_id:bi.chain_id bytes >>=? fun oph ->
  operation_submitted_message cctxt oph >>=? fun () ->
  let pk_uri = Tezos_signer_backends.Unencrypted.make_pk pk in
  begin
    if encrypted then
      Tezos_signer_backends.Encrypted.encrypt cctxt sk
    else
      return (Tezos_signer_backends.Unencrypted.make_sk sk)
  end >>=? fun sk_uri ->
  begin
    match confirmations with
    | None ->
        Client_keys.register_key cctxt ?force (pkh, pk_uri, sk_uri) name >>=? fun () ->
        return ()
    | Some confirmations ->
        cctxt#message "Waiting for the operation to be included..." >>= fun () ->
        wait_for_operation_inclusion ~confirmations cctxt oph >>=? fun () ->
        Client_keys.register_key cctxt ?force (pkh, pk_uri, sk_uri) name >>=? fun () ->
        Alpha_services.Contract.balance
          cctxt (`Head 0) (Contract.implicit_contract pkh) >>=? fun balance ->
        cctxt#message "Account %s (%a) created with %s%a."
          name
          Signature.Public_key_hash.pp pkh
          Client_proto_args.tez_sym
          Tez.pp balance >>= fun () ->
        return ()
  end

