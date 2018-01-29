(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Proto_alpha
open Tezos_context
open Tezos_micheline
open Client_proto_context
open Client_proto_contracts
open Client_proto_programs
open Client_keys
open Client_proto_args

let get_pkh cctxt = function
  | None -> return None
  | Some x -> Public_key_hash.find_opt cctxt x

let report_michelson_errors ?(no_print_source=false) ~msg (cctxt : #Client_commands.logger) = function
  | Error errs ->
      cctxt#warning "%a"
        (Michelson_v1_error_reporter.report_errors
           ~details:(not no_print_source)
           ~show_source: (not no_print_source)
           ?parsed:None) errs >>= fun () ->
      cctxt#error "%s" msg >>= fun () ->
      Lwt.return None
  | Ok data ->
      Lwt.return (Some data)


let group =
  { Cli_entries.name = "context" ;
    title = "Block contextual commands (see option -block)" }

let alphanet =
  { Cli_entries.name = "alphanet" ;
    title = "Alphanet only commands" }

let commands () =
  let open Cli_entries in
  [
    command ~group ~desc: "Access the timestamp of the block."
      no_options
      (fixed [ "get" ; "timestamp" ])
      begin fun () (cctxt : Client_commands.full_context) ->
        Client_node_rpcs.Blocks.timestamp
          cctxt cctxt#block >>=? fun v ->
        cctxt#message "%s" (Time.to_notation v) >>= fun () ->
        return ()
      end ;

    command ~group ~desc: "Lists all non empty contracts of the block."
      no_options
      (fixed [ "list" ; "contracts" ])
      begin fun () (cctxt : Client_commands.full_context) ->
        list_contract_labels cctxt cctxt#block >>=? fun contracts ->
        Lwt_list.iter_s
          (fun (alias, hash, kind) -> cctxt#message "%s%s%s" hash kind alias)
          contracts >>= fun () ->
        return ()
      end ;

    command ~group ~desc: "Get the balance of a contract."
      no_options
      (prefixes [ "get" ; "balance" ; "for" ]
       @@ ContractAlias.destination_param ~name:"src" ~desc:"source contract"
       @@ stop)
      begin fun () (_, contract) (cctxt : Client_commands.full_context) ->
        get_balance cctxt cctxt#block contract >>=? fun amount ->
        cctxt#answer "%a %s" Tez.pp amount Client_proto_args.tez_sym >>= fun () ->
        return ()
      end ;

    command ~group ~desc: "Get the storage of a contract."
      no_options
      (prefixes [ "get" ; "storage" ; "for" ]
       @@ ContractAlias.destination_param ~name:"src" ~desc:"source contract"
       @@ stop)
      begin fun () (_, contract) (cctxt : Client_commands.full_context) ->
        get_storage cctxt cctxt#block contract >>=? function
        | None ->
            cctxt#error "This is not a smart contract."
        | Some storage ->
            cctxt#answer "%a" Michelson_v1_printer.print_expr_unwrapped storage >>= fun () ->
            return ()
      end ;

    command ~group ~desc: "Get the manager of a contract."
      no_options
      (prefixes [ "get" ; "manager" ; "for" ]
       @@ ContractAlias.destination_param ~name:"src" ~desc:"source contract"
       @@ stop)
      begin fun () (_, contract) (cctxt : Client_commands.full_context) ->
        Client_proto_contracts.get_manager
          cctxt cctxt#block contract >>=? fun manager ->
        Public_key_hash.rev_find cctxt manager >>=? fun mn ->
        Public_key_hash.to_source cctxt manager >>=? fun m ->
        cctxt#message "%s (%s)" m
          (match mn with None -> "unknown" | Some n -> "known as " ^ n) >>= fun () ->
        return ()
      end ;

    command ~group ~desc: "Get the delegate of a contract."
      no_options
      (prefixes [ "get" ; "delegate" ; "for" ]
       @@ ContractAlias.destination_param ~name:"src" ~desc:"source contract"
       @@ stop)
      begin fun () (_, contract) (cctxt : Client_commands.full_context) ->
        Client_proto_contracts.get_delegate
          cctxt cctxt#block contract >>=? fun delegate ->
        Public_key_hash.rev_find cctxt delegate >>=? fun mn ->
        Public_key_hash.to_source cctxt delegate >>=? fun m ->
        cctxt#message "%s (%s)" m
          (match mn with None -> "unknown" | Some n -> "known as " ^ n) >>= fun () ->
        return ()
      end ;

    command ~group ~desc: "Set the delegate of a contract."
      (args1 fee_arg)
      (prefixes [ "set" ; "delegate" ; "for" ]
       @@ ContractAlias.destination_param ~name:"src" ~desc:"source contract"
       @@ prefix "to"
       @@ Public_key_hash.alias_param
         ~name: "mgr" ~desc: "new delegate of the contract"
       @@ stop)
      begin fun fee (_, contract) (_, delegate) cctxt ->
        source_to_keys cctxt cctxt#block contract >>=? fun (src_pk, manager_sk) ->
        set_delegate ~fee cctxt cctxt#block contract (Some delegate) ~src_pk ~manager_sk >>=? fun oph ->
        operation_submitted_message cctxt oph
      end ;

    command ~group ~desc:"Open a new account."
      (args4 fee_arg delegate_arg delegatable_switch Client_keys.force_switch)
      (prefixes [ "originate" ; "account" ]
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
       @@ stop)
      begin fun (fee, delegate, delegatable, force)
        new_contract (_, manager_pkh) balance (_, source) (cctxt : Client_commands.full_context) ->
        RawContractAlias.of_fresh cctxt force new_contract >>=? fun alias_name ->
        source_to_keys cctxt cctxt#block source >>=? fun (src_pk, src_sk) ->
        get_pkh cctxt delegate >>=? fun delegate ->
        originate_account
          ~fee
          ?delegate
          ~delegatable
          ~manager_pkh
          ~balance
          ~source
          ~src_pk
          ~src_sk
          cctxt#block
          cctxt
          () >>=? fun (oph, contract) ->
        save_contract ~force cctxt alias_name contract >>=? fun () ->
        operation_submitted_message ~contracts:[ contract ] cctxt oph
      end ;

    command ~group ~desc: "Launch a smart contract on the blockchain."
      (args7
         fee_arg delegate_arg Client_keys.force_switch
         delegatable_switch spendable_switch init_arg no_print_source_flag)
      (prefixes [ "originate" ; "contract" ]
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
                             Combine with -init if the storage type is not unit."
       @@ stop)
      begin fun (fee, delegate, force, delegatable, spendable, initial_storage, no_print_source)
        alias_name (_, manager) balance (_, source) program (cctxt : Client_commands.full_context) ->
        RawContractAlias.of_fresh cctxt force alias_name >>=? fun alias_name ->
        Lwt.return (Micheline_parser.no_parsing_error program) >>=? fun { expanded = code } ->
        source_to_keys cctxt cctxt#block source >>=? fun (src_pk, src_sk) ->
        get_pkh cctxt delegate >>=? fun delegate ->
        originate_contract ~fee ~delegate ~delegatable ~spendable ~initial_storage
          ~manager ~balance ~source ~src_pk ~src_sk ~code cctxt >>= fun errors ->
        report_michelson_errors ~no_print_source ~msg:"origination simulation failed" cctxt errors >>= function
        | None -> return ()
        | Some (oph, contract) ->
            save_contract ~force cctxt alias_name contract >>=? fun () ->
            operation_submitted_message cctxt
              ~contracts:[contract] oph
      end ;

    command ~group ~desc: "Transfer tokens / call a smart contract."
      (args3 fee_arg arg_arg no_print_source_flag)
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
      begin fun (fee, arg, no_print_source) amount (_, source) (_, destination) cctxt ->
        source_to_keys cctxt cctxt#block source >>=? fun (src_pk, src_sk) ->
        transfer cctxt ~fee cctxt#block
          ~source ~src_pk ~src_sk ~destination ~arg ~amount () >>=
        report_michelson_errors ~no_print_source ~msg:"transfer simulation failed" cctxt >>= function
        | None -> return ()
        | Some (oph, contracts) ->
            operation_submitted_message cctxt ~contracts oph
      end;

    command ~group:alphanet ~desc: "Open a new FREE account (Alphanet only)."
      (args1 force_switch)
      (prefixes [ "originate" ; "free" ; "account" ]
       @@ RawContractAlias.fresh_alias_param
         ~name: "new" ~desc: "name of the new contract"
       @@ prefix "for"
       @@ Public_key_hash.alias_param
         ~name: "mgr" ~desc: "manager of the new contract"
       @@ stop)
      begin fun force alias_name (_, manager_pkh) cctxt ->
        RawContractAlias.of_fresh cctxt force alias_name >>=? fun alias_name ->
        faucet ~manager_pkh cctxt#block cctxt () >>=? fun (oph, contract) ->
        operation_submitted_message cctxt
          ~contracts:[contract] oph >>=? fun () ->
        save_contract ~force cctxt alias_name contract
      end;

    command ~group:alphanet ~desc: "Activate a protocol (Alphanet dictator only)."
      no_options
      (prefixes [ "activate" ; "protocol" ]
       @@ Protocol_hash.param ~name:"version"
         ~desc:"protocol version (b58check)"
       @@ prefixes [ "with" ; "key" ]
       @@ Environment.Ed25519.Secret_key.param
         ~name:"password" ~desc:"dictator's key"
       @@ stop)
      begin fun () hash seckey cctxt ->
        dictate cctxt cctxt#block
          (Activate hash) seckey >>=? fun oph ->
        operation_submitted_message cctxt oph
      end ;

    command ~group:alphanet ~desc: "Fork a test protocol (Alphanet dictator only)."
      no_options
      (prefixes [ "fork" ; "test" ; "protocol" ]
       @@ Protocol_hash.param ~name:"version"
         ~desc:"protocol version (b58check)"
       @@ prefixes [ "with" ; "key" ]
       @@ Environment.Ed25519.Secret_key.param
         ~name:"password" ~desc:"dictator's key"
       @@ stop)
      begin fun () hash seckey cctxt ->
        dictate cctxt cctxt#block
          (Activate_testnet hash) seckey >>=? fun oph ->
        operation_submitted_message cctxt oph
      end ;

  ]
