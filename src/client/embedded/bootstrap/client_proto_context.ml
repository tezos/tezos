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
module Ed25519 = Environment.Ed25519

let handle_error f () =
    f () >>= Client_proto_rpcs.handle_error

let check_contract neu =
  RawContractAlias.mem neu >>= function
  | true ->
      Cli_entries.error "contract '%s' already exists" neu
  | false ->
      Lwt.return ()

let get_delegate_pkh = function
  | None -> Lwt.return None
  | Some delegate ->
      Lwt.catch
        (fun () ->
           Public_key_hash.find delegate >>= fun r ->
           Lwt.return (Some r))
        (fun _ -> Lwt.return None)

let get_timestamp block () =
  Client_node_rpcs.Blocks.timestamp block >>= fun v ->
  Cli_entries.message "%s" (Time.to_notation v)

let list_contracts block () =
  Client_proto_rpcs.Context.Contract.list block >>=? fun contracts ->
  iter_s (fun h ->
      begin match Contract.is_default h with
        | Some m -> begin
            Public_key_hash.rev_find m >>= function
            | None -> Lwt.return ""
            | Some nm ->
                RawContractAlias.find_opt nm >|= function
                | None -> " (known as " ^ nm ^ ")"
                | Some _ -> " (known as key:" ^ nm ^ ")"
          end
        | None -> begin
            RawContractAlias.rev_find h >|= function
            | None -> ""
            | Some nm ->  " (known as " ^ nm ^ ")"
          end
      end >>= fun nm ->
      let kind = match Contract.is_default h with
        | Some _ -> " (default)"
        | None -> "" in
      Cli_entries.message "%s%s%s" (Contract.to_b48check h) kind nm >>= fun () ->
      return ())
    contracts

let transfer block ?force
    ~source ~src_pk ~src_sk ~destination ?arg ~amount ~fee () =
  let open Cli_entries in
  Client_node_rpcs.Blocks.net block >>= fun net ->
  begin match arg with
    | Some arg ->
        Client_proto_programs.parse_data arg >>= fun arg ->
        Lwt.return (Some arg)
    | None -> Lwt.return None
  end >>= fun parameters ->
  Client_proto_rpcs.Context.Contract.counter block source >>=? fun pcounter ->
  let counter = Int32.succ pcounter in
  message "Acquired the source's sequence counter (%ld -> %ld)."
    pcounter counter >>= fun () ->
  Client_proto_rpcs.Helpers.Forge.Manager.transaction block
    ~net ~source ~sourcePubKey:src_pk ~counter ~amount
    ~destination ?parameters ~fee () >>=? fun bytes ->
  message "Forged the raw transaction frame." >>= fun () ->
  let signed_bytes = Ed25519.append_signature src_sk bytes in
  Client_node_rpcs.inject_operation ?force ~wait:true signed_bytes >>=? fun oph ->
  answer "Operation successfully injected in the node." >>= fun () ->
  answer "Operation hash is '%a'." Operation_hash.pp oph >>= fun () ->
  return ()

let originate_account block ?force
    ~source ~src_pk ~src_sk ~manager_pkh ?delegatable ?spendable ?delegate ~balance ~fee () =
  let open Cli_entries in
  Client_node_rpcs.Blocks.net block >>= fun net ->
  Client_proto_rpcs.Context.Contract.counter block source >>=? fun pcounter ->
  let counter = Int32.succ pcounter in
  message "Acquired the source's sequence counter (%ld -> %ld)."
    pcounter counter >>= fun () ->
  Client_proto_rpcs.Helpers.Forge.Manager.origination block
    ~net ~source ~sourcePubKey:src_pk ~managerPubKey:manager_pkh
    ~counter ~balance ?spendable
    ?delegatable ?delegatePubKey:delegate ~fee () >>=? fun (contract, bytes) ->
  message "Forged the raw origination frame." >>= fun () ->
  let signed_bytes = Ed25519.append_signature src_sk bytes in
  Client_node_rpcs.inject_operation ?force ~wait:true signed_bytes >>=? fun oph ->
  message "Operation successfully injected in the node." >>= fun () ->
  message "Operation hash is '%a'." Operation_hash.pp oph >>= fun () ->
  return contract

let originate_contract
    block ?force
    ~source ~src_pk ~src_sk ~manager_pkh ~balance ?delegatable ?delegatePubKey
    ~(code:Script.code) ~init ~fee () =
  let open Cli_entries in
  Client_proto_programs.parse_data init >>= fun storage ->
  let init = Script.{ storage ; storage_type = code.storage_type } in
  Client_proto_rpcs.Context.Contract.counter block source >>=? fun pcounter ->
  let counter = Int32.succ pcounter in
  message "Acquired the source's sequence counter (%ld -> %ld)."
    pcounter counter >>= fun () ->
  Client_node_rpcs.Blocks.net block >>= fun net ->
  Client_proto_rpcs.Helpers.Forge.Manager.origination block
    ~net ~source ~sourcePubKey:src_pk ~managerPubKey:manager_pkh
    ~counter ~balance ~spendable:!spendable
    ?delegatable ?delegatePubKey
    ~script:(code, init) ~fee () >>=? fun (contract, bytes) ->
  message "Forged the raw origination frame." >>= fun () ->
  let signed_bytes = Ed25519.append_signature src_sk bytes in
  Client_node_rpcs.inject_operation ?force ~wait:true signed_bytes >>=? fun oph ->
  message "Operation successfully injected in the node." >>= fun () ->
  message "Operation hash is '%a'." Operation_hash.pp oph >>= fun () ->
  return contract

let commands () =
  let open Cli_entries in
  register_group "context" "Block contextual commands (see option -block)" ;
  [ command
      ~group: "context"
      ~desc: "access the timestamp of the block"
      (fixed [ "get" ; "timestamp" ])
      (get_timestamp (block ())) ;
    command
      ~group: "context"
      ~desc: "lists all non empty contracts of the block"
      (fixed [ "list" ; "contracts" ])
      (handle_error (list_contracts (block ()))) ;
    command
      ~group: "context"
      ~desc: "get the bootstrap keys and bootstrap contract handle"
      (fixed [ "bootstrap" ])
      (fun () ->
         Client_proto_rpcs.Constants.bootstrap `Genesis >>= fun accounts ->
         let cpt = ref 0 in
         Lwt_list.iter_s
           (fun { Bootstrap.public_key_hash = pkh ;
                  public_key = pk ; secret_key = sk } ->
             incr cpt ;
             let name = Printf.sprintf "bootstrap%d" !cpt in
             Public_key_hash.add name pkh >>= fun () ->
             Public_key.add name pk >>= fun () ->
             Secret_key.add name sk >>= fun () ->
             message "Bootstrap keys added under the name '%s'." name)
           accounts >>= fun () ->
         Lwt.return_unit) ;
    command
      ~group: "context"
      ~desc: "get the balance of a contract"
      (prefixes [ "get" ; "balance" ]
       @@ ContractAlias.destination_param ~name:"src" ~desc:"source contract"
       @@ stop)
      (fun (_, contract) () ->
         Client_proto_rpcs.Context.Contract.balance (block ()) contract
         >>= Client_proto_rpcs.handle_error >>= fun amount ->
         answer "%a %s" Tez.pp amount tez_sym);
    command
      ~group: "context"
      ~desc: "get the manager of a block"
      (prefixes [ "get" ; "manager" ]
       @@ ContractAlias.destination_param ~name:"src" ~desc:"source contract"
       @@ stop)
      (fun (_, contract) () ->
         Client_proto_rpcs.Context.Contract.manager (block ()) contract
         >>= Client_proto_rpcs.handle_error >>= fun manager ->
         Public_key_hash.rev_find manager >>= fun mn ->
         Public_key_hash.to_source manager >>= fun m ->
         message "%s (%s)" m
           (match mn with None -> "unknown" | Some n -> "known as " ^ n));
    command
      ~group: "context"
      ~desc: "open a new account"
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
      (fun neu (_, manager) balance (_, source) ->
         handle_error @@ fun () ->
         check_contract neu >>= fun () ->
         get_delegate_pkh !delegate >>= fun delegate ->
         Client_proto_contracts.get_manager (block ()) source >>=? fun src_pkh ->
         Client_keys.get_key src_pkh >>=? fun (src_name, src_pk, src_sk) ->
         message "Got the source's manager keys (%s)." src_name >>= fun () ->
         originate_account (block ()) ~force:!force
           ~source ~src_pk ~src_sk ~manager_pkh:manager ~balance ~fee:!fee
           ~delegatable:!delegatable ~spendable:!spendable ?delegate:delegate
           () >>=? fun contract ->
         RawContractAlias.add neu contract >>= fun () ->
         return ()) ;
    command
      ~group: "context"
      ~desc: "open a new scripted account"
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
                          combine with -init if the storage type is non void"
       @@ stop)
      (fun neu (_, manager) balance (_, source) code ->
         handle_error @@ fun () ->
         check_contract neu >>= fun () ->
         get_delegate_pkh !delegate >>= fun delegate ->
         Client_proto_contracts.get_manager (block ()) source >>=? fun src_pkh ->
         Client_keys.get_key src_pkh >>=? fun (src_name, src_pk, src_sk) ->
         message "Got the source's manager keys (%s)." src_name >>= fun () ->
         originate_contract (block ()) ~force:!force
           ~source ~src_pk ~src_sk ~manager_pkh:manager ~balance ~fee:!fee
           ~delegatable:!delegatable ?delegatePubKey:delegate ~code ~init:!init ()
         >>=? fun contract ->
         RawContractAlias.add neu contract >>= fun () ->
         return ()) ;
    command
      ~group: "context"
      ~desc: "transfer tokens"
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
      (fun amount (_, source) (_, destination) ->
         handle_error @@ fun () ->
         Client_proto_contracts.get_manager (block ()) source >>=? fun src_pkh ->
         Client_keys.get_key src_pkh >>=? fun (src_name, src_pk, src_sk) ->
         message "Got the source's manager keys (%s)." src_name >>= fun () ->
         transfer (block ()) ~force:!force
           ~source ~src_pk ~src_sk ~destination ?arg:!arg ~amount ~fee:!fee ())
  ]
