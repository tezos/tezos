(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Client_proto_args
open Client_proto_contracts
open Client_proto_programs
open Client_keys
open Client_commands
module Ed25519 = Environment.Ed25519

let check_contract cctxt neu =
  RawContractAlias.mem cctxt neu >>=? function
  | true ->
      failwith "contract '%s' already exists" neu
  | false ->
      return ()

let get_delegate_pkh cctxt = function
  | None ->
      return None
  | Some delegate ->
      Public_key_hash.find_opt cctxt delegate

let get_timestamp cctxt block =
  Client_node_rpcs.Blocks.timestamp cctxt.rpc_config block >>=? fun v ->
  cctxt.message "%s" (Time.to_notation v) >>= fun () ->
  return ()

let list_contracts cctxt block =
  Client_proto_rpcs.Context.Contract.list cctxt.rpc_config block >>=? fun contracts ->
  map_s (fun h ->
      begin match Contract.is_default h with
        | Some m -> begin
            Public_key_hash.rev_find cctxt m >>=? function
            | None -> return ""
            | Some nm -> return nm
          end
        | None -> begin
            RawContractAlias.rev_find cctxt h >>=? function
            | None -> return ""
            | Some nm -> return nm
          end
      end >>=? fun alias ->
      return (alias, h, Contract.is_default h))
    contracts

let list_contract_labels cctxt block =
  Client_proto_rpcs.Context.Contract.list
    cctxt.rpc_config block >>=? fun contracts ->
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

let get_balance cctxt block contract =
  Client_proto_rpcs.Context.Contract.balance cctxt block contract

let transfer cctxt
    block ?force
    ~source ~src_pk ~src_sk ~destination ?arg ~amount ~fee () =
  let open Cli_entries in
  Client_node_rpcs.Blocks.net cctxt.rpc_config block >>=? fun net ->
  begin match arg with
    | Some arg ->
        Client_proto_programs.parse_data arg >>=? fun arg ->
        return (Some arg)
    | None -> return None
  end >>=? fun parameters ->
  Client_proto_rpcs.Context.Contract.counter cctxt.rpc_config block source >>=? fun pcounter ->
  let counter = Int32.succ pcounter in
  cctxt.message "Acquired the source's sequence counter (%ld -> %ld)."
    pcounter counter >>= fun () ->
  Client_proto_rpcs.Helpers.Forge.Manager.transaction cctxt.rpc_config block
    ~net ~source ~sourcePubKey:src_pk ~counter ~amount
    ~destination ?parameters ~fee () >>=? fun bytes ->
  cctxt.Client_commands.message "Forged the raw origination frame." >>= fun () ->
  Client_node_rpcs.Blocks.predecessor cctxt.rpc_config block >>=? fun predecessor ->
  let signature = Ed25519.sign src_sk bytes in
  let signed_bytes = MBytes.concat bytes signature in
  let oph = Operation_hash.hash_bytes [ signed_bytes ] in
  Client_proto_rpcs.Helpers.apply_operation cctxt.rpc_config block
    predecessor oph bytes (Some signature) >>=? fun contracts ->
  Client_node_rpcs.inject_operation cctxt.rpc_config ?force signed_bytes >>=? fun injected_oph ->
  assert (Operation_hash.equal oph injected_oph) ;
  cctxt.message "Operation successfully injected in the node." >>= fun () ->
  cctxt.message "Operation hash is '%a'." Operation_hash.pp oph >>= fun () ->
  return contracts

let originate cctxt ?force ~block ?signature bytes =
  cctxt.Client_commands.message "Forged the raw origination frame." >>= fun () ->
  let signed_bytes =
    match signature with
    | None -> bytes
    | Some signature -> MBytes.concat bytes signature in
  Client_node_rpcs.Blocks.predecessor cctxt.rpc_config block >>=? fun predecessor ->
  let oph = Operation_hash.hash_bytes [ signed_bytes ] in
  Client_proto_rpcs.Helpers.apply_operation cctxt.rpc_config block
    predecessor oph bytes signature >>=? function
  | [ contract ] ->
      Client_node_rpcs.inject_operation cctxt.rpc_config ?force signed_bytes >>=? fun injected_oph ->
      assert (Operation_hash.equal oph injected_oph) ;
      cctxt.message "Operation successfully injected in the node." >>= fun () ->
      cctxt.message "Operation hash is '%a'." Operation_hash.pp oph >>= fun () ->
      return contract
  | contracts ->
      cctxt.error "The origination introduced %d contracts instead of one." (List.length contracts)

let originate_account cctxt
    block ?force
    ~source ~src_pk ~src_sk ~manager_pkh ?delegatable ?spendable ?delegate ~balance ~fee () =
  Client_node_rpcs.Blocks.net cctxt.rpc_config block >>=? fun net ->
  Client_proto_rpcs.Context.Contract.counter cctxt.rpc_config block source >>=? fun pcounter ->
  let counter = Int32.succ pcounter in
  cctxt.message "Acquired the source's sequence counter (%ld -> %ld)."
    pcounter counter >>= fun () ->
  Client_proto_rpcs.Helpers.Forge.Manager.origination cctxt.rpc_config block
    ~net ~source ~sourcePubKey:src_pk ~managerPubKey:manager_pkh
    ~counter ~balance ?spendable
    ?delegatable ?delegatePubKey:delegate ~fee () >>=? fun bytes ->
  let signature = Ed25519.sign src_sk bytes in
  originate cctxt ?force ~block ~signature bytes

let originate_contract cctxt
    block ?force
    ~source ~src_pk ~src_sk ~manager_pkh ~balance ?delegatable ?delegatePubKey
    ~(code:Script.code) ~init ~fee () =
  Client_proto_programs.parse_data init >>=? fun storage ->
  let storage = Script.{ storage ; storage_type = code.storage_type } in
  Client_proto_rpcs.Context.Contract.counter cctxt.rpc_config block source >>=? fun pcounter ->
  let counter = Int32.succ pcounter in
  cctxt.message "Acquired the source's sequence counter (%ld -> %ld)."
    pcounter counter >>= fun () ->
  Client_node_rpcs.Blocks.net cctxt.rpc_config block >>=? fun net ->
  Client_proto_rpcs.Helpers.Forge.Manager.origination cctxt.rpc_config block
    ~net ~source ~sourcePubKey:src_pk ~managerPubKey:manager_pkh
    ~counter ~balance ~spendable:!spendable
    ?delegatable ?delegatePubKey
    ~script:{ code ; storage } ~fee () >>=? fun bytes ->
  let signature = Ed25519.sign src_sk bytes in
  originate cctxt ?force ~block ~signature bytes

let faucet cctxt block ?force ~manager_pkh () =
  Client_node_rpcs.Blocks.net cctxt.rpc_config block >>=? fun net ->
  Client_proto_rpcs.Helpers.Forge.Anonymous.faucet cctxt.rpc_config block
    ~net ~id:manager_pkh () >>=? fun bytes ->
  originate cctxt ?force ~block bytes

let delegate_contract cctxt
    block ?force
    ~source ?src_pk ~manager_sk
    ~fee delegate_opt =
  Client_node_rpcs.Blocks.net cctxt.rpc_config block >>=? fun net ->
  Client_proto_rpcs.Context.Contract.counter cctxt.rpc_config block source
  >>=? fun pcounter ->
  let counter = Int32.succ pcounter in
  cctxt.message "Acquired the source's sequence counter (%ld -> %ld)."
    pcounter counter >>= fun () ->
  Client_proto_rpcs.Helpers.Forge.Manager.delegation cctxt.rpc_config block
    ~net ~source ?sourcePubKey:src_pk ~counter ~fee delegate_opt
  >>=? fun bytes ->
  cctxt.Client_commands.message "Forged the raw origination frame." >>= fun () ->
  Client_node_rpcs.Blocks.predecessor cctxt.rpc_config block >>=? fun predecessor ->
  let signature = Environment.Ed25519.sign manager_sk bytes in
  let signed_bytes = MBytes.concat bytes signature in
  let oph = Operation_hash.hash_bytes [ signed_bytes ] in
  Client_proto_rpcs.Helpers.apply_operation cctxt.rpc_config block
    predecessor oph bytes (Some signature) >>=? function
  | [] ->
      Client_node_rpcs.inject_operation cctxt.rpc_config ?force signed_bytes >>=? fun injected_oph ->
      assert (Operation_hash.equal oph injected_oph) ;
      cctxt.message "Operation successfully injected in the node." >>= fun () ->
      cctxt.message "Operation hash is '%a'." Operation_hash.pp oph >>= fun () ->
      return ()
  | contracts ->
      cctxt.error "The origination introduced %d contracts instead of one." (List.length contracts)

let dictate cctxt block command seckey =
  Client_node_rpcs.Blocks.net cctxt.rpc_config block >>=? fun net ->
  Client_proto_rpcs.Helpers.Forge.Dictator.operation
    cctxt.rpc_config block ~net command >>=? fun bytes ->
  let signature = Ed25519.sign seckey bytes in
  let signed_bytes = MBytes.concat bytes signature in
  let oph = Operation_hash.hash_bytes [ signed_bytes ] in
  Client_node_rpcs.inject_operation cctxt.rpc_config signed_bytes >>=? fun injected_oph ->
  assert (Operation_hash.equal oph injected_oph) ;
  cctxt.message "Operation successfully injected in the node." >>= fun () ->
  cctxt.message "Operation hash is '%a'." Operation_hash.pp oph >>= fun () ->
  return ()

let group =
  { Cli_entries.name = "context" ;
    title = "Block contextual commands (see option -block)" }

let commands () =
  let open Cli_entries in
  let open Client_commands in
  [

    command ~group ~desc: "access the timestamp of the block"
      (fixed [ "get" ; "timestamp" ])
      (fun cctxt -> get_timestamp cctxt cctxt.config.block) ;

    command ~group ~desc: "lists all non empty contracts of the block"
      (fixed [ "list" ; "contracts" ])
      (fun cctxt ->
         list_contract_labels cctxt cctxt.config.block >>=? fun contracts ->
         Lwt_list.iter_s
           (fun (alias, hash, kind) -> cctxt.message "%s%s%s" hash kind alias)
           contracts >>= fun () ->
         return ()) ;

    command ~group ~desc: "get the balance of a contract"
      (prefixes [ "get" ; "balance" ]
       @@ ContractAlias.destination_param ~name:"src" ~desc:"source contract"
       @@ stop)
      (fun (_, contract) cctxt ->
         get_balance cctxt.rpc_config cctxt.config.block contract >>=? fun amount ->
         cctxt.answer "%a %s" Tez.pp amount tez_sym >>= fun () ->
         return ()) ;

    command ~group ~desc: "get the manager of a block"
      (prefixes [ "get" ; "manager" ]
       @@ ContractAlias.destination_param ~name:"src" ~desc:"source contract"
       @@ stop)
      (fun (_, contract) cctxt ->
         Client_proto_rpcs.Context.Contract.manager cctxt.rpc_config cctxt.config.block contract >>=? fun manager ->
         Public_key_hash.rev_find cctxt manager >>=? fun mn ->
         Public_key_hash.to_source cctxt manager >>=? fun m ->
         cctxt.message "%s (%s)" m
           (match mn with None -> "unknown" | Some n -> "known as " ^ n) >>= fun () ->
         return ());

    command ~group ~desc: "open a new account"
      ~args: ([ fee_arg ; delegate_arg ; force_arg ]
              @ delegatable_args @ spendable_args)
      (prefixes [ "originate" ; "account" ]
       @@ RawContractAlias.fresh_alias_param
         ~name: "new" ~desc: "name of the new contract"
       @@ prefix "for"
       @@ Public_key_hash.alias_param
         ~name: "mgr" ~desc: "manager of the new contract"
       @@ prefix "transfering"
       @@ tez_param
         ~name: "qty" ~desc: "amount taken from source"
       @@ prefix "from"
       @@ ContractAlias.alias_param
         ~name:"src" ~desc: "name of the source contract"
       @@ stop)
      (fun neu (_, manager) balance (_, source) cctxt ->
         check_contract cctxt neu >>=? fun () ->
         get_delegate_pkh cctxt !delegate >>=? fun delegate ->
         (Client_proto_contracts.get_manager cctxt.rpc_config cctxt.config.block source >>=? fun src_pkh ->
          Client_keys.get_key cctxt src_pkh
          >>=? fun (src_name, src_pk, src_sk) ->
          cctxt.message "Got the source's manager keys (%s)." src_name >>= fun () ->
          originate_account cctxt cctxt.config.block ~force:!force
            ~source ~src_pk ~src_sk ~manager_pkh:manager ~balance ~fee:!fee
            ~delegatable:!delegatable ~spendable:!spendable ?delegate:delegate
            ()) >>=? fun contract ->
         RawContractAlias.add cctxt neu contract) ;

    command ~group ~desc: "open a new scripted account"
      ~args: ([ fee_arg ; delegate_arg ; force_arg ] @
              delegatable_args @ spendable_args @ [ init_arg ])
      (prefixes [ "originate" ; "contract" ]
       @@ RawContractAlias.fresh_alias_param
         ~name: "new" ~desc: "name of the new contract"
       @@ prefix "for"
       @@ Public_key_hash.alias_param
         ~name: "mgr" ~desc: "manager of the new contract"
       @@ prefix "transfering"
       @@ tez_param
         ~name: "qty" ~desc: "amount taken from source"
       @@ prefix "from"
       @@ ContractAlias.alias_param
         ~name:"src" ~desc: "name of the source contract"
       @@ prefix "running"
       @@ Program.source_param
         ~name:"prg" ~desc: "script of the account\n\
                             combine with -init if the storage type is not unit"
       @@ stop)
      (fun neu (_, manager) balance (_, source) code cctxt ->
         check_contract cctxt neu >>=? fun () ->
         get_delegate_pkh cctxt !delegate >>=? fun delegate ->
         (Client_proto_contracts.get_manager cctxt.rpc_config cctxt.config.block source >>=? fun src_pkh ->
          Client_keys.get_key cctxt src_pkh
          >>=? fun (src_name, src_pk, src_sk) ->
          cctxt.message "Got the source's manager keys (%s)." src_name >>= fun () ->
          originate_contract cctxt cctxt.config.block ~force:!force
            ~source ~src_pk ~src_sk ~manager_pkh:manager ~balance ~fee:!fee
            ~delegatable:!delegatable ?delegatePubKey:delegate ~code ~init:!init
            ()) >>=? fun contract ->
         RawContractAlias.add cctxt neu contract) ;

    command ~group ~desc: "open a new (free) account"
      ~args: ([ fee_arg ; delegate_arg ; force_arg ]
              @ delegatable_args @ spendable_args)
      (prefixes [ "originate" ; "free" ; "account" ]
       @@ RawContractAlias.fresh_alias_param
         ~name: "new" ~desc: "name of the new contract"
       @@ prefix "for"
       @@ Public_key_hash.alias_param
         ~name: "mgr" ~desc: "manager of the new contract"
       @@ stop)
      (fun neu (_, manager) cctxt ->
         check_contract cctxt neu >>=? fun () ->
         faucet cctxt cctxt.config.block ~force:!force ~manager_pkh:manager () >>=? fun contract ->
         RawContractAlias.add cctxt neu contract) ;

    command ~group ~desc: "transfer tokens"
      ~args: [ fee_arg ; arg_arg ; force_arg ]
      (prefixes [ "transfer" ]
       @@ tez_param
         ~name: "qty" ~desc: "amount taken from source"
       @@ prefix "from"
       @@ ContractAlias.alias_param
         ~name: "src" ~desc: "name of the source contract"
       @@ prefix "to"
       @@ ContractAlias.destination_param
         ~name: "dst" ~desc: "name/literal of the destination contract"
       @@ stop)
      (fun amount (_, source) (_, destination) cctxt ->
         (Client_proto_contracts.get_manager cctxt.rpc_config cctxt.config.block source >>=? fun src_pkh ->
          Client_keys.get_key cctxt src_pkh
          >>=? fun (src_name, src_pk, src_sk) ->
          cctxt.message "Got the source's manager keys (%s)." src_name >>= fun () ->
          (transfer cctxt cctxt.config.block ~force:!force
             ~source ~src_pk ~src_sk ~destination ?arg:!arg ~amount ~fee:!fee ()) >>=? fun contracts ->
          Lwt_list.iter_s
            (fun c -> cctxt.message "New contract %a originated from a smart contract."
                Contract.pp c)
            contracts >>= fun () ->
          return ())) ;

    command ~desc: "Activate a protocol" begin
      prefixes [ "activate" ; "protocol" ] @@
      Protocol_hash.param ~name:"version" ~desc:"Protocol version (b58check)" @@
      prefixes [ "with" ; "key" ] @@
      Environment.Ed25519.Secret_key.param
        ~name:"password" ~desc:"Dictator's key" @@
        stop
    end
      (fun hash seckey cctxt ->
         dictate cctxt cctxt.config.block (Activate hash) seckey) ;

    command ~desc: "Fork a test protocol" begin
      prefixes [ "fork" ; "test" ; "protocol" ] @@
      Protocol_hash.param ~name:"version" ~desc:"Protocol version (b58check)" @@
      prefixes [ "with" ; "key" ] @@
      Environment.Ed25519.Secret_key.param
        ~name:"password" ~desc:"Dictator's key" @@
      stop
    end
      (fun hash seckey cctxt ->
         dictate cctxt cctxt.config.block (Activate_testnet hash) seckey) ;

  ]
