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

let get_balance cctxt block contract =
  Client_proto_rpcs.Context.Contract.balance cctxt block contract

let rec find_predecessor rpc_config h n =
  if n <= 0 then
    return (`Hash h)
  else
    Client_node_rpcs.Blocks.predecessor rpc_config (`Hash h) >>=? fun h ->
    find_predecessor rpc_config h (n-1)

let get_branch rpc_config block branch =
  let branch = Utils.unopt ~default:0 branch in (* TODO export parameter *)
  let block = Client_rpcs.last_mined_block block in
  begin
    match block with
    | `Head n -> return (`Head (n+branch))
    | `Test_head n -> return (`Test_head (n+branch))
    | `Hash h -> find_predecessor rpc_config h branch
    | `Genesis -> return `Genesis
  end >>=? fun block ->
  Client_node_rpcs.Blocks.info rpc_config block >>=? fun { net_id ; hash } ->
  return (net_id, hash)

let transfer rpc_config
    block ?force ?branch
    ~source ~src_pk ~src_sk ~destination ?arg ~amount ~fee () =
  let open Cli_entries in
  get_branch rpc_config block branch >>=? fun (net_id, branch) ->
  begin match arg with
    | Some arg ->
        Client_proto_programs.parse_data arg >>=? fun arg ->
        return (Some arg)
    | None -> return None
  end >>=? fun parameters ->
  Client_proto_rpcs.Context.Contract.counter
    rpc_config block source >>=? fun pcounter ->
  let counter = Int32.succ pcounter in
  Client_proto_rpcs.Helpers.Forge.Manager.transaction
    rpc_config block
    ~net_id ~branch ~source ~sourcePubKey:src_pk ~counter ~amount
    ~destination ?parameters ~fee () >>=? fun bytes ->
  Client_node_rpcs.Blocks.predecessor rpc_config block >>=? fun predecessor ->
  let signature = Ed25519.sign src_sk bytes in
  let signed_bytes = MBytes.concat bytes signature in
  let oph = Operation_hash.hash_bytes [ signed_bytes ] in
  Client_proto_rpcs.Helpers.apply_operation rpc_config block
    predecessor oph bytes (Some signature) >>=? fun contracts ->
  Client_node_rpcs.inject_operation
    rpc_config ?force signed_bytes >>=? fun injected_oph ->
  assert (Operation_hash.equal oph injected_oph) ;
  return (oph, contracts)

let originate rpc_config ?force ~block ?signature bytes =
  let signed_bytes =
    match signature with
    | None -> bytes
    | Some signature -> MBytes.concat bytes signature in
  Client_node_rpcs.Blocks.predecessor rpc_config block >>=? fun predecessor ->
  let oph = Operation_hash.hash_bytes [ signed_bytes ] in
  Client_proto_rpcs.Helpers.apply_operation rpc_config block
    predecessor oph bytes signature >>=? function
  | [ contract ] ->
      Client_node_rpcs.inject_operation
        rpc_config ?force signed_bytes >>=? fun injected_oph ->
      assert (Operation_hash.equal oph injected_oph) ;
      return (oph, contract)
  | contracts ->
      failwith
        "The origination introduced %d contracts instead of one."
        (List.length contracts)

let originate_account rpc_config
    block ?force ?branch
    ~source ~src_pk ~src_sk ~manager_pkh
    ?delegatable ?spendable ?delegate ~balance ~fee () =
  get_branch rpc_config block branch >>=? fun (net_id, branch) ->
  Client_proto_rpcs.Context.Contract.counter
    rpc_config block source >>=? fun pcounter ->
  let counter = Int32.succ pcounter in
  Client_proto_rpcs.Helpers.Forge.Manager.origination rpc_config block
    ~net_id ~branch ~source ~sourcePubKey:src_pk ~managerPubKey:manager_pkh
    ~counter ~balance ?spendable
    ?delegatable ?delegatePubKey:delegate ~fee () >>=? fun bytes ->
  let signature = Ed25519.sign src_sk bytes in
  originate rpc_config ?force ~block ~signature bytes

let originate_contract rpc_config
    block ?force ?branch
    ~source ~src_pk ~src_sk ~manager_pkh ~balance ?delegatable ?delegatePubKey
    ~(code:Script.code) ~init ~fee () =
  Client_proto_programs.parse_data init >>=? fun storage ->
  let storage = Script.{ storage ; storage_type = code.storage_type } in
  Client_proto_rpcs.Context.Contract.counter
    rpc_config block source >>=? fun pcounter ->
  let counter = Int32.succ pcounter in
  get_branch rpc_config block branch >>=? fun (net_id, branch) ->
  Client_proto_rpcs.Helpers.Forge.Manager.origination rpc_config block
    ~net_id ~branch ~source ~sourcePubKey:src_pk ~managerPubKey:manager_pkh
    ~counter ~balance ~spendable:!spendable
    ?delegatable ?delegatePubKey
    ~script:{ code ; storage } ~fee () >>=? fun bytes ->
  let signature = Ed25519.sign src_sk bytes in
  originate rpc_config ?force ~block ~signature bytes

let faucet rpc_config block ?force ?branch ~manager_pkh () =
  get_branch rpc_config block branch >>=? fun (net_id, branch) ->
  Client_proto_rpcs.Helpers.Forge.Anonymous.faucet
    rpc_config block ~net_id ~branch ~id:manager_pkh () >>=? fun bytes ->
  originate rpc_config ?force ~block bytes

let delegate_contract rpc_config
    block ?force ?branch
    ~source ?src_pk ~manager_sk
    ~fee delegate_opt =
  get_branch rpc_config block branch >>=? fun (net_id, branch) ->
  Client_proto_rpcs.Context.Contract.counter
    rpc_config block source >>=? fun pcounter ->
  let counter = Int32.succ pcounter in
  Client_proto_rpcs.Helpers.Forge.Manager.delegation rpc_config block
    ~net_id ~branch ~source ?sourcePubKey:src_pk ~counter ~fee delegate_opt
  >>=? fun bytes ->
  let signature = Environment.Ed25519.sign manager_sk bytes in
  let signed_bytes = MBytes.concat bytes signature in
  let oph = Operation_hash.hash_bytes [ signed_bytes ] in
  Client_node_rpcs.inject_operation
    rpc_config ?force signed_bytes >>=? fun injected_oph ->
  assert (Operation_hash.equal oph injected_oph) ;
  return oph

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

let message_injection cctxt ~force ?(contracts = []) oph =
  begin
    if not force then
      cctxt.message "Operation successfully injected in the node."
    else
      Lwt.return_unit
  end >>= fun () ->
  cctxt.message "Operation hash is '%a'." Operation_hash.pp oph >>= fun () ->
  Lwt_list.iter_s
    (fun c ->
       cctxt.message
         "New contract %a originated from a smart contract."
         Contract.pp c)
    contracts >>= fun () ->
  Lwt.return_unit

let message_added_contract cctxt name =
  cctxt.message "Contract memorized as %s." name

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

let get_manager cctxt source =
  Client_proto_contracts.get_manager
    cctxt.rpc_config cctxt.config.block source >>=? fun src_pkh ->
  Client_keys.get_key cctxt src_pkh >>=? fun (src_name, src_pk, src_sk) ->
  cctxt.message "Got the source's manager keys (%s)." src_name >>= fun () ->
  return (src_name, src_pkh, src_pk, src_sk)

let group =
  { Cli_entries.name = "context" ;
    title = "Block contextual commands (see option -block)" }

let dictate rpc_config block command seckey =
  let block = Client_rpcs.last_mined_block block in
  Client_node_rpcs.Blocks.info
    rpc_config block >>=? fun { net_id ; hash = branch } ->
  Client_proto_rpcs.Helpers.Forge.Dictator.operation
    rpc_config block ~net_id ~branch command >>=? fun bytes ->
  let signature = Ed25519.sign seckey bytes in
  let signed_bytes = MBytes.concat bytes signature in
  let oph = Operation_hash.hash_bytes [ signed_bytes ] in
  Client_node_rpcs.inject_operation
    rpc_config signed_bytes >>=? fun injected_oph ->
  assert (Operation_hash.equal oph injected_oph) ;
  return oph

let commands () =
  let open Cli_entries in
  let open Client_commands in
  [

    command ~group ~desc: "access the timestamp of the block" begin
      fixed [ "get" ; "timestamp" ]
    end begin fun cctxt ->
      Client_node_rpcs.Blocks.timestamp
        cctxt.rpc_config cctxt.config.block >>=? fun v ->
      cctxt.message "%s" (Time.to_notation v) >>= fun () ->
      return ()
    end ;

    command ~group ~desc: "lists all non empty contracts of the block" begin
      fixed [ "list" ; "contracts" ]
    end begin fun cctxt ->
      list_contract_labels cctxt cctxt.config.block >>=? fun contracts ->
      Lwt_list.iter_s
        (fun (alias, hash, kind) -> cctxt.message "%s%s%s" hash kind alias)
        contracts >>= fun () ->
      return ()
    end ;

    command ~group ~desc: "get the balance of a contract" begin
      prefixes [ "get" ; "balance" ; "for" ]
      @@ ContractAlias.destination_param ~name:"src" ~desc:"source contract"
      @@ stop
    end begin fun (_, contract) cctxt ->
      get_balance cctxt.rpc_config cctxt.config.block contract >>=? fun amount ->
      cctxt.answer "%a %s" Tez.pp amount tez_sym >>= fun () ->
      return ()
    end ;

    command ~group ~desc: "get the manager of a contract" begin
      prefixes [ "get" ; "manager" ; "for" ]
      @@ ContractAlias.destination_param ~name:"src" ~desc:"source contract"
      @@ stop
    end begin fun (_, contract) cctxt ->
      Client_proto_contracts.get_manager
        cctxt.rpc_config cctxt.config.block contract >>=? fun manager ->
      Public_key_hash.rev_find cctxt manager >>=? fun mn ->
      Public_key_hash.to_source cctxt manager >>=? fun m ->
      cctxt.message "%s (%s)" m
        (match mn with None -> "unknown" | Some n -> "known as " ^ n) >>= fun () ->
      return ()
    end ;

    command ~group ~desc: "get the delegate of a contract" begin
      prefixes [ "get" ; "delegate" ; "for" ]
      @@ ContractAlias.destination_param ~name:"src" ~desc:"source contract"
      @@ stop
    end begin fun (_, contract) cctxt ->
      Client_proto_contracts.get_delegate
        cctxt.rpc_config cctxt.config.block contract >>=? fun delegate ->
      Public_key_hash.rev_find cctxt delegate >>=? fun mn ->
      Public_key_hash.to_source cctxt delegate >>=? fun m ->
      cctxt.message "%s (%s)" m
        (match mn with None -> "unknown" | Some n -> "known as " ^ n) >>= fun () ->
      return ()
    end ;

    command ~group ~desc: "set the delegate of a contract"
      ~args: ([ fee_arg ; force_arg ]) begin
      prefixes [ "set" ; "delegate" ; "for" ]
      @@ ContractAlias.destination_param ~name:"src" ~desc:"source contract"
      @@ prefix "to"
      @@ Public_key_hash.alias_param
        ~name: "mgr" ~desc: "new delegate of the contract"
      @@ stop
    end begin fun (_, contract) (_, delegate) cctxt ->
      get_manager cctxt contract >>=? fun (_src_name, _src_pkh, src_pk, src_sk) ->
      delegate_contract
        cctxt.rpc_config cctxt.config.block ~source:contract
        ~src_pk ~manager_sk:src_sk ~fee:!fee (Some delegate)
      >>=? fun oph ->
      message_injection cctxt ~force:!force oph >>= fun () ->
      return ()
    end ;

    command ~group ~desc: "open a new account"
      ~args: ([ fee_arg ; delegate_arg ; force_arg ]
              @ delegatable_args @ spendable_args) begin
      prefixes [ "originate" ; "account" ]
      @@ RawContractAlias.fresh_alias_param
        ~name: "new" ~desc: "name of the new contract"
      @@ prefix "for"
      @@ Public_key_hash.alias_param
        ~name: "mgr" ~desc: "manager of the new contract"
      @@ prefix "transferring"
      @@ tez_param
        ~name: "qty" ~desc: "amount taken from source"
      @@ prefix "from"
      @@ ContractAlias.alias_param
        ~name:"src" ~desc: "name of the source contract"
      @@ stop
    end begin fun neu (_, manager) balance (_, source) cctxt ->
      check_contract cctxt neu >>=? fun () ->
      get_delegate_pkh cctxt !delegate >>=? fun delegate ->
      get_manager cctxt source >>=? fun (_src_name, _src_pkh, src_pk, src_sk) ->
      originate_account cctxt.rpc_config cctxt.config.block ~force:!force
        ~source ~src_pk ~src_sk ~manager_pkh:manager ~balance ~fee:!fee
        ~delegatable:!delegatable ~spendable:!spendable ?delegate:delegate
        () >>=? fun (oph, contract) ->
      message_injection cctxt
        ~force:!force ~contracts:[contract] oph >>= fun () ->
      RawContractAlias.add cctxt neu contract >>=? fun () ->
      message_added_contract cctxt neu >>= fun () ->
      return ()
    end ;

    command ~group ~desc: "open a new scripted account"
      ~args: ([ fee_arg ; delegate_arg ; force_arg ] @
              delegatable_args @ spendable_args @ [ init_arg ]) begin
      prefixes [ "originate" ; "contract" ]
      @@ RawContractAlias.fresh_alias_param
        ~name: "new" ~desc: "name of the new contract"
      @@ prefix "for"
      @@ Public_key_hash.alias_param
        ~name: "mgr" ~desc: "manager of the new contract"
      @@ prefix "transferring"
      @@ tez_param
        ~name: "qty" ~desc: "amount taken from source"
      @@ prefix "from"
      @@ ContractAlias.alias_param
        ~name:"src" ~desc: "name of the source contract"
      @@ prefix "running"
      @@ Program.source_param
        ~name:"prg" ~desc: "script of the account\n\
                            combine with -init if the storage type is not unit"
      @@ stop
    end begin fun neu (_, manager) balance (_, source) code cctxt ->
      check_contract cctxt neu >>=? fun () ->
      get_delegate_pkh cctxt !delegate >>=? fun delegate ->
      get_manager cctxt source >>=? fun (_src_name, _src_pkh, src_pk, src_sk) ->
      originate_contract cctxt.rpc_config cctxt.config.block ~force:!force
        ~source ~src_pk ~src_sk ~manager_pkh:manager ~balance ~fee:!fee
        ~delegatable:!delegatable ?delegatePubKey:delegate ~code ~init:!init
        () >>=? fun (oph, contract) ->
      message_injection cctxt
        ~force:!force ~contracts:[contract] oph >>= fun () ->
      RawContractAlias.add cctxt neu contract >>=? fun () ->
      message_added_contract cctxt neu >>= fun () ->
      return ()
    end ;

    command ~group ~desc: "open a new (free) account"
      ~args: ([ fee_arg ; delegate_arg ; force_arg ]
              @ delegatable_args @ spendable_args) begin
      prefixes [ "originate" ; "free" ; "account" ]
      @@ RawContractAlias.fresh_alias_param
        ~name: "new" ~desc: "name of the new contract"
      @@ prefix "for"
      @@ Public_key_hash.alias_param
        ~name: "mgr" ~desc: "manager of the new contract"
      @@ stop end
      begin fun neu (_, manager) cctxt ->
        check_contract cctxt neu >>=? fun () ->
        faucet cctxt.rpc_config cctxt.config.block
          ~force:!force ~manager_pkh:manager () >>=? fun (oph, contract) ->
        message_injection cctxt
          ~force:!force ~contracts:[contract] oph >>= fun () ->
        RawContractAlias.add cctxt neu contract >>=? fun () ->
        message_added_contract cctxt neu >>= fun () ->
        return ()
      end;

    command ~group ~desc: "transfer tokens"
      ~args: [ fee_arg ; arg_arg ; force_arg ] begin
      prefixes [ "transfer" ]
      @@ tez_param
        ~name: "qty" ~desc: "amount taken from source"
      @@ prefix "from"
      @@ ContractAlias.alias_param
        ~name: "src" ~desc: "name of the source contract"
      @@ prefix "to"
      @@ ContractAlias.destination_param
        ~name: "dst" ~desc: "name/literal of the destination contract"
      @@ stop
    end begin fun amount (_, source) (_, destination) cctxt ->
      get_manager cctxt source >>=? fun (_src_name, _src_pkh, src_pk, src_sk) ->
      transfer cctxt.rpc_config cctxt.config.block ~force:!force
        ~source ~src_pk ~src_sk ~destination
        ?arg:!arg ~amount ~fee:!fee () >>=? fun (oph, contracts) ->
      message_injection cctxt ~force:!force ~contracts oph >>= fun () ->
      return ()
    end;

    command ~desc: "Activate a protocol" begin
      prefixes [ "activate" ; "protocol" ] @@
      Protocol_hash.param ~name:"version" ~desc:"Protocol version (b58check)" @@
      prefixes [ "with" ; "key" ] @@
      Environment.Ed25519.Secret_key.param
        ~name:"password" ~desc:"Dictator's key" @@
        stop
    end begin fun hash seckey cctxt ->
      dictate cctxt.rpc_config cctxt.config.block
        (Activate hash) seckey >>=? fun oph ->
      message_injection cctxt ~force:!force oph >>= fun () ->
      return ()
    end ;

    command ~desc: "Fork a test protocol" begin
      prefixes [ "fork" ; "test" ; "protocol" ] @@
      Protocol_hash.param ~name:"version" ~desc:"Protocol version (b58check)" @@
      prefixes [ "with" ; "key" ] @@
      Environment.Ed25519.Secret_key.param
        ~name:"password" ~desc:"Dictator's key" @@
      stop
    end begin fun hash seckey cctxt ->
      dictate cctxt.rpc_config cctxt.config.block
        (Activate_testnet hash) seckey >>=? fun oph ->
      message_injection cctxt ~force:!force oph >>= fun () ->
      return ()
    end ;

  ]
