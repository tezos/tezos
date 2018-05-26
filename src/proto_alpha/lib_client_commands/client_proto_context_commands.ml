(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Proto_alpha
open Alpha_context
open Tezos_micheline
open Client_proto_context
open Client_proto_contracts
open Client_proto_programs
open Client_keys
open Client_proto_args

let encrypted_switch =
  Clic.switch
    ~long:"encrypted"
    ~doc:("Encrypt the key on-disk") ()

let report_michelson_errors ?(no_print_source=false) ~msg (cctxt : #Client_context.printer) = function
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

let file_parameter =
  Clic.parameter (fun _ p ->
      if not (Sys.file_exists p) then
        failwith "File doesn't exist: '%s'" p
      else
        return p)

let group =
  { Clic.name = "context" ;
    title = "Block contextual commands (see option -block)" }

let alphanet =
  { Clic.name = "alphanet" ;
    title = "Alphanet only commands" }

let commands () =
  let open Clic in
  [
    command ~group ~desc: "Access the timestamp of the block."
      no_options
      (fixed [ "get" ; "timestamp" ])
      begin fun () (cctxt : Proto_alpha.full) ->
        Block_services.timestamp
          cctxt cctxt#block >>=? fun v ->
        cctxt#message "%s" (Time.to_notation v) >>= fun () ->
        return ()
      end ;

    command ~group ~desc: "Lists all non empty contracts of the block."
      no_options
      (fixed [ "list" ; "contracts" ])
      begin fun () (cctxt : Proto_alpha.full) ->
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
      begin fun () (_, contract) (cctxt : Proto_alpha.full) ->
        get_balance cctxt cctxt#block contract >>=? fun amount ->
        cctxt#answer "%a %s" Tez.pp amount Client_proto_args.tez_sym >>= fun () ->
        return ()
      end ;

    command ~group ~desc: "Get the storage of a contract."
      no_options
      (prefixes [ "get" ; "storage" ; "for" ]
       @@ ContractAlias.destination_param ~name:"src" ~desc:"source contract"
       @@ stop)
      begin fun () (_, contract) (cctxt : Proto_alpha.full) ->
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
      begin fun () (_, contract) (cctxt : Proto_alpha.full) ->
        Client_proto_contracts.get_manager
          cctxt cctxt#block contract >>=? fun manager ->
        Public_key_hash.rev_find cctxt manager >>=? fun mn ->
        Public_key_hash.to_source manager >>=? fun m ->
        cctxt#message "%s (%s)" m
          (match mn with None -> "unknown" | Some n -> "known as " ^ n) >>= fun () ->
        return ()
      end ;

    command ~group ~desc: "Get the delegate of a contract."
      no_options
      (prefixes [ "get" ; "delegate" ; "for" ]
       @@ ContractAlias.destination_param ~name:"src" ~desc:"source contract"
       @@ stop)
      begin fun () (_, contract) (cctxt : Proto_alpha.full) ->
        Client_proto_contracts.get_delegate
          cctxt cctxt#block contract >>=? function
        | None ->
            cctxt#message "none" >>= fun () ->
            return ()
        | Some delegate ->
            Public_key_hash.rev_find cctxt delegate >>=? fun mn ->
            Public_key_hash.to_source delegate >>=? fun m ->
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
      begin fun fee (_, contract) (_, delegate) (cctxt : Proto_alpha.full) ->
        source_to_keys cctxt cctxt#block contract >>=? fun (src_pk, manager_sk) ->
        set_delegate ~fee cctxt cctxt#block contract (Some delegate) ~src_pk ~manager_sk >>=? fun oph ->
        operation_submitted_message cctxt oph
      end ;

    command ~group ~desc:"Open a new account."
      (args4 fee_arg delegate_arg delegatable_switch (Client_keys.force_switch ()))
      (prefixes [ "originate" ; "account" ]
       @@ RawContractAlias.fresh_alias_param
         ~name: "new" ~desc: "name of the new contract"
       @@ prefix "for"
       @@ Public_key_hash.source_param
         ~name: "mgr" ~desc: "manager of the new contract"
       @@ prefix "transferring"
       @@ tez_param
         ~name: "qty" ~desc: "amount taken from source"
       @@ prefix "from"
       @@ ContractAlias.destination_param
         ~name:"src" ~desc: "name of the source contract"
       @@ stop)
      begin fun (fee, delegate, delegatable, force)
        new_contract manager_pkh balance (_, source) (cctxt : Proto_alpha.full) ->
        RawContractAlias.of_fresh cctxt force new_contract >>=? fun alias_name ->
        source_to_keys cctxt cctxt#block source >>=? fun (src_pk, src_sk) ->
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
         fee_arg delegate_arg (Client_keys.force_switch ())
         delegatable_switch spendable_switch init_arg no_print_source_flag)
      (prefixes [ "originate" ; "contract" ]
       @@ RawContractAlias.fresh_alias_param
         ~name: "new" ~desc: "name of the new contract"
       @@ prefix "for"
       @@ Public_key_hash.source_param
         ~name: "mgr" ~desc: "manager of the new contract"
       @@ prefix "transferring"
       @@ tez_param
         ~name: "qty" ~desc: "amount taken from source"
       @@ prefix "from"
       @@ ContractAlias.destination_param
         ~name:"src" ~desc: "name of the source contract"
       @@ prefix "running"
       @@ Program.source_param
         ~name:"prg" ~desc: "script of the account\n\
                             Combine with -init if the storage type is not unit."
       @@ stop)
      begin fun (fee, delegate, force, delegatable, spendable, initial_storage, no_print_source)
        alias_name manager balance (_, source) program (cctxt : Proto_alpha.full) ->
        RawContractAlias.of_fresh cctxt force alias_name >>=? fun alias_name ->
        Lwt.return (Micheline_parser.no_parsing_error program) >>=? fun { expanded = code } ->
        source_to_keys cctxt cctxt#block source >>=? fun (src_pk, src_sk) ->
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
       @@ ContractAlias.destination_param
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

    command ~group ~desc: "Reveal the public key of the contract manager."
      (args1 fee_arg)
      (prefixes [ "reveal" ; "key" ; "for" ]
       @@ ContractAlias.alias_param
         ~name: "src" ~desc: "name of the source contract"
       @@ stop)
      begin fun fee (_, source) cctxt ->
        source_to_keys cctxt cctxt#block source >>=? fun (src_pk, src_sk) ->
        reveal cctxt ~fee cctxt#block
          ~source ~src_pk ~src_sk () >>=? fun oph ->
        operation_submitted_message cctxt oph
      end;

    command ~group ~desc: "Register the public key hash as a delegate."
      (args1 fee_arg)
      (prefixes [ "register" ; "key" ]
       @@ Public_key_hash.source_param
         ~name: "mgr" ~desc: "the delegate key"
       @@ prefixes [ "as" ; "delegate" ]
       @@ stop)
      begin fun fee src_pkh cctxt ->
        Client_keys.get_key cctxt src_pkh >>=? fun (_, src_pk, src_sk) ->
        register_as_delegate cctxt
          ~fee cctxt#block ~manager_sk:src_sk src_pk >>=? fun oph ->
        operation_submitted_message cctxt oph
      end;

    command ~group ~desc:"Register and activate a predefined account using the provided activation key."
      (args3
         (Secret_key.force_switch ())
         (Client_proto_args.no_confirmation)
         encrypted_switch)
      (prefixes [ "activate" ; "account" ]
       @@ Secret_key.fresh_alias_param
       @@ prefixes [ "with" ]
       @@ param ~name:"activation_key"
         ~desc:"Activation key (as JSON file) obtained from the Tezos foundation (or the Alphanet faucet)."
         file_parameter
       @@ stop)
      (fun
        (force, no_confirmation, encrypted)
        name activation_key_file cctxt ->
        Secret_key.of_fresh cctxt force name >>=? fun name ->
        Lwt_utils_unix.Json.read_file activation_key_file >>=? fun json ->
        match Data_encoding.Json.destruct
                Client_proto_context.activation_key_encoding
                json with
        | exception (Data_encoding.Json.Cannot_destruct _ as exn) ->
            Format.kasprintf (fun s -> failwith "%s" s)
              "Invalid activation file: %a %a"
              (fun ppf -> Data_encoding.Json.print_error ppf) exn
              Data_encoding.Json.pp json
        | key ->
            let confirmations =
              if no_confirmation then None else Some 0 in
            claim_commitment cctxt cctxt#block
              ~encrypted ?confirmations ~force key name
      );

    command ~group:alphanet ~desc: "Activate a protocol (Alphanet dictator only)."
      no_options
      (prefixes [ "activate" ; "protocol" ]
       @@ Protocol_hash.param ~name:"version"
         ~desc:"protocol version (b58check)"
       @@ prefixes [ "with" ; "key" ]
       @@ Signature.Secret_key.param
         ~name:"password" ~desc:"dictator's key"
       @@ stop)
      begin fun () hash seckey cctxt ->
        dictate cctxt cctxt#block
          (Activate hash) seckey >>=? fun oph ->
        operation_submitted_message cctxt oph
      end ;

    command ~desc:"Wait until an operation is included in a block"
      (let int_param =
         parameter
           (fun _ s ->
              try return (int_of_string s)
              with _ -> failwith "Given an invalid integer literal: '%s'" s) in
       args2
         (default_arg
            ~long:"-confirmations"
            ~placeholder:"num_blocks"
            ~doc:"do not end until after 'N' additional blocks after the operation appears"
            ~default:"0"
            int_param)
         (default_arg
            ~long:"-check-previous"
            ~placeholder:"num_blocks"
            ~doc:"number of previous blocks to check"
            ~default:"10"
            int_param))
      (prefixes [ "wait" ; "for" ]
       @@ param
         ~name:"operation"
         ~desc:"Operation to be included"
         (parameter
            (fun _ x ->
               match Operation_hash.of_b58check_opt x with
               | None -> Error_monad.failwith "Invalid operation hash: '%s'" x
               | Some hash -> return hash))
       @@ prefixes [ "to" ; "be" ; "included" ]
       @@ stop)
      begin fun (confirmations, predecessors) operation_hash (ctxt : Proto_alpha.full) ->
        fail_when (confirmations < 0)
          (failure "confirmations cannot be negative") >>=? fun () ->
        fail_when (predecessors < 0)
          (failure "check-previous cannot be negative") >>=? fun () ->
        wait_for_operation_inclusion ctxt
          ~confirmations ~predecessors operation_hash
      end ;

    command ~group:alphanet ~desc: "Fork a test protocol (Alphanet dictator only)."
      no_options
      (prefixes [ "fork" ; "test" ; "protocol" ]
       @@ Protocol_hash.param ~name:"version"
         ~desc:"protocol version (b58check)"
       @@ prefixes [ "with" ; "key" ]
       @@ Signature.Secret_key.param
         ~name:"password" ~desc:"dictator's key"
       @@ stop)
      begin fun () hash seckey cctxt ->
        dictate cctxt cctxt#block
          (Activate_testchain hash) seckey >>=? fun oph ->
        operation_submitted_message cctxt oph
      end ;

  ]
