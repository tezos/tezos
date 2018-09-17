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
open Tezos_micheline
open Client_proto_context
open Client_proto_contracts
open Client_proto_programs
open Client_keys
open Client_proto_args

let encrypted_switch =
  Clic.switch
    ~long:"encrypted"
    ~doc:"encrypt the key on-disk" ()

let dry_run_switch =
  Clic.switch
    ~long:"dry-run"
    ~short:'D'
    ~doc:"don't inject the operation, just display it" ()

let report_michelson_errors ?(no_print_source=false) ~msg (cctxt : #Client_context.printer) = function
  | Error errs ->
      cctxt#warning "%a"
        (Michelson_v1_error_reporter.report_errors
           ~details:(not no_print_source)
           ~show_source: (not no_print_source)
           ?parsed:None) errs >>= fun () ->
      cctxt#error "%s" msg >>= fun () ->
      Lwt.return_none
  | Ok data ->
      Lwt.return_some data

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

let binary_description =
  { Clic.name = "description" ;
    title = "Binary Description" }

let commands version () =
  let open Clic in
  [
    command ~group ~desc: "Access the timestamp of the block."
      (args1
         (switch ~doc:"output time in seconds" ~short:'s' ~long:"seconds" ()))
      (fixed [ "get" ; "timestamp" ])
      begin fun seconds (cctxt : Proto_001_PtCJ7pwo.full) ->
        Shell_services.Blocks.Header.shell_header
          cctxt ~block:cctxt#block () >>=? fun { timestamp = v } ->
        begin
          if seconds
          then cctxt#message "%Ld" (Time.to_seconds v)
          else cctxt#message "%s" (Time.to_notation v)
        end >>= fun () ->
        return_unit
      end ;

    command ~group ~desc: "Lists all non empty contracts of the block."
      no_options
      (fixed [ "list" ; "contracts" ])
      begin fun () (cctxt : Proto_001_PtCJ7pwo.full) ->
        list_contract_labels cctxt
          ~chain:`Main ~block:cctxt#block >>=? fun contracts ->
        Lwt_list.iter_s
          (fun (alias, hash, kind) -> cctxt#message "%s%s%s" hash kind alias)
          contracts >>= fun () ->
        return_unit
      end ;

    command ~group ~desc: "Get the balance of a contract."
      no_options
      (prefixes [ "get" ; "balance" ; "for" ]
       @@ ContractAlias.destination_param ~name:"src" ~desc:"source contract"
       @@ stop)
      begin fun () (_, contract) (cctxt : Proto_001_PtCJ7pwo.full) ->
        get_balance cctxt
          ~chain:`Main ~block:cctxt#block
          contract >>=? fun amount ->
        cctxt#answer "%a %s" Tez.pp amount Client_proto_args.tez_sym >>= fun () ->
        return_unit
      end ;

    command ~group ~desc: "Get the storage of a contract."
      no_options
      (prefixes [ "get" ; "script" ; "storage" ; "for" ]
       @@ ContractAlias.destination_param ~name:"src" ~desc:"source contract"
       @@ stop)
      begin fun () (_, contract) (cctxt : Proto_001_PtCJ7pwo.full) ->
        get_storage cctxt
          ~chain:`Main ~block:cctxt#block
          contract >>=? function
        | None ->
            cctxt#error "This is not a smart contract."
        | Some storage ->
            cctxt#answer "%a" Michelson_v1_printer.print_expr_unwrapped storage >>= fun () ->
            return_unit
      end ;

    command ~group ~desc: "Get the storage of a contract."
      no_options
      (prefixes [ "get" ; "script" ; "code" ; "for" ]
       @@ ContractAlias.destination_param ~name:"src" ~desc:"source contract"
       @@ stop)
      begin fun () (_, contract) (cctxt : Proto_001_PtCJ7pwo.full) ->
        get_script cctxt
          ~chain:`Main ~block:cctxt#block
          contract >>=? function
        | None ->
            cctxt#error "This is not a smart contract."
        | Some { code ; storage = _ } ->
            match Script_repr.force_decode code with
            | Error errs -> cctxt#error "%a" (Format.pp_print_list ~pp_sep:Format.pp_print_newline Alpha_environment.Error_monad.pp) errs
            | Ok (code, _) ->
                begin cctxt#answer "%a" Michelson_v1_printer.print_expr_unwrapped code >>= fun () ->
                  return_unit
                end
      end ;

    command ~group ~desc: "Get the manager of a contract."
      no_options
      (prefixes [ "get" ; "manager" ; "for" ]
       @@ ContractAlias.destination_param ~name:"src" ~desc:"source contract"
       @@ stop)
      begin fun () (_, contract) (cctxt : Proto_001_PtCJ7pwo.full) ->
        Client_proto_contracts.get_manager cctxt
          ~chain:`Main ~block:cctxt#block
          contract >>=? fun manager ->
        Public_key_hash.rev_find cctxt manager >>=? fun mn ->
        Public_key_hash.to_source manager >>=? fun m ->
        cctxt#message "%s (%s)" m
          (match mn with None -> "unknown" | Some n -> "known as " ^ n) >>= fun () ->
        return_unit
      end ;

    command ~group ~desc: "Get the delegate of a contract."
      no_options
      (prefixes [ "get" ; "delegate" ; "for" ]
       @@ ContractAlias.destination_param ~name:"src" ~desc:"source contract"
       @@ stop)
      begin fun () (_, contract) (cctxt : Proto_001_PtCJ7pwo.full) ->
        Client_proto_contracts.get_delegate cctxt
          ~chain:`Main ~block:cctxt#block
          contract >>=? function
        | None ->
            cctxt#message "none" >>= fun () ->
            return_unit
        | Some delegate ->
            Public_key_hash.rev_find cctxt delegate >>=? fun mn ->
            Public_key_hash.to_source delegate >>=? fun m ->
            cctxt#message "%s (%s)" m
              (match mn with None -> "unknown" | Some n -> "known as " ^ n) >>= fun () ->
            return_unit
      end ;

    command ~group ~desc: "Set the delegate of a contract."
      (args2 fee_arg dry_run_switch)
      (prefixes [ "set" ; "delegate" ; "for" ]
       @@ ContractAlias.destination_param ~name:"src" ~desc:"source contract"
       @@ prefix "to"
       @@ Public_key_hash.alias_param
         ~name: "mgr" ~desc: "new delegate of the contract"
       @@ stop)
      begin fun (fee, dry_run) (_, contract) (_, delegate) (cctxt : Proto_001_PtCJ7pwo.full) ->
        source_to_keys cctxt
          ~chain:`Main ~block:cctxt#block
          contract >>=? fun (src_pk, manager_sk) ->
        set_delegate cctxt
          ~chain:`Main ~block:cctxt#block ?confirmations:cctxt#confirmations
          ~dry_run
          contract (Some delegate) ~fee ~src_pk ~manager_sk >>=? fun _ ->
        return_unit
      end ;

    command ~group ~desc: "Withdraw the delegate from a contract."
      (args2 fee_arg dry_run_switch)
      (prefixes [ "withdraw" ; "delegate" ; "from" ]
       @@ ContractAlias.destination_param ~name:"src" ~desc:"source contract"
       @@ stop)
      begin fun (fee, dry_run) (_, contract) (cctxt : Proto_001_PtCJ7pwo.full) ->
        source_to_keys cctxt
          ~chain:`Main ~block:cctxt#block
          contract >>=? fun (src_pk, manager_sk) ->
        set_delegate cctxt
          ~chain:`Main ~block:cctxt#block ?confirmations:cctxt#confirmations
          ~dry_run
          contract None ~fee ~src_pk ~manager_sk >>=? fun _ ->
        return_unit
      end ;

    command ~group ~desc:"Open a new account."
      (args5 fee_arg dry_run_switch delegate_arg delegatable_switch (Client_keys.force_switch ()))
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
      begin fun (fee, dry_run, delegate, delegatable, force)
        new_contract manager_pkh balance (_, source) (cctxt : Proto_001_PtCJ7pwo.full) ->
        RawContractAlias.of_fresh cctxt force new_contract >>=? fun alias_name ->
        source_to_keys cctxt
          ~chain:`Main ~block:cctxt#block
          source >>=? fun (src_pk, src_sk) ->
        originate_account cctxt
          ~chain:`Main ~block:cctxt#block ?confirmations:cctxt#confirmations
          ~dry_run
          ~fee ?delegate ~delegatable ~manager_pkh ~balance
          ~source ~src_pk ~src_sk () >>=? fun (_res, contract) ->
        if dry_run then
          return_unit
        else
          save_contract ~force cctxt alias_name contract >>=? fun () ->
          return_unit
      end ;

    command ~group ~desc: "Launch a smart contract on the blockchain."
      (args10
         fee_arg
         dry_run_switch gas_limit_arg storage_limit_arg delegate_arg (Client_keys.force_switch ())
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
      begin fun (fee, dry_run, gas_limit, storage_limit, delegate, force, delegatable, spendable, initial_storage, no_print_source)
        alias_name manager balance (_, source) program (cctxt : Proto_001_PtCJ7pwo.full) ->
        RawContractAlias.of_fresh cctxt force alias_name >>=? fun alias_name ->
        Lwt.return (Micheline_parser.no_parsing_error program) >>=? fun { expanded = code } ->
        source_to_keys cctxt
          ~chain:`Main ~block:cctxt#block
          source >>=? fun (src_pk, src_sk) ->
        originate_contract cctxt
          ~chain:`Main ~block:cctxt#block ?confirmations:cctxt#confirmations
          ~dry_run
          ~fee ?gas_limit ?storage_limit ~delegate ~delegatable ~spendable ~initial_storage
          ~manager ~balance ~source ~src_pk ~src_sk ~code () >>= fun errors ->
        report_michelson_errors ~no_print_source ~msg:"origination simulation failed" cctxt errors >>= function
        | None -> return_unit
        | Some (_res, contract) ->
            if dry_run then
              return_unit
            else
              save_contract ~force cctxt alias_name contract >>=? fun () ->
              return_unit
      end ;

    command ~group ~desc: "Transfer tokens / call a smart contract."
      (args6 fee_arg dry_run_switch gas_limit_arg storage_limit_arg arg_arg no_print_source_flag)
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
      begin fun (fee, dry_run, gas_limit, storage_limit, arg, no_print_source) amount (_, source) (_, destination) cctxt ->
        source_to_keys cctxt
          ~chain:`Main ~block:cctxt#block
          source >>=? fun (src_pk, src_sk) ->
        transfer cctxt
          ~chain:`Main ~block:cctxt#block ?confirmations:cctxt#confirmations
          ~dry_run
          ~source ~fee ~src_pk ~src_sk ~destination ?arg ~amount ?gas_limit ?storage_limit () >>=
        report_michelson_errors ~no_print_source ~msg:"transfer simulation failed" cctxt >>= function
        | None -> return_unit
        | Some (_res, _contracts) ->
            return_unit
      end;

    command ~group ~desc: "Reveal the public key of the contract manager."
      (args1 fee_arg)
      (prefixes [ "reveal" ; "key" ; "for" ]
       @@ ContractAlias.alias_param
         ~name: "src" ~desc: "name of the source contract"
       @@ stop)
      begin fun fee (_, source) cctxt ->
        source_to_keys cctxt
          ~chain:`Main ~block:cctxt#block
          source >>=? fun (src_pk, src_sk) ->
        reveal cctxt
          ~chain:`Main ~block:cctxt#block ?confirmations:cctxt#confirmations
          ~source ~fee ~src_pk ~src_sk () >>=? fun _res ->
        return_unit
      end;

    command ~group ~desc: "Register the public key hash as a delegate."
      (args2 fee_arg dry_run_switch)
      (prefixes [ "register" ; "key" ]
       @@ Public_key_hash.source_param
         ~name: "mgr" ~desc: "the delegate key"
       @@ prefixes [ "as" ; "delegate" ]
       @@ stop)
      begin fun (fee, dry_run)  src_pkh cctxt ->
        Client_keys.get_key cctxt src_pkh >>=? fun (_, src_pk, src_sk) ->
        register_as_delegate cctxt
          ~chain:`Main ~block:cctxt#block ?confirmations:cctxt#confirmations
          ~dry_run
          ~fee ~manager_sk:src_sk src_pk >>=? fun _res ->
        return_unit
      end;
  ] @
  (if version = (Some `Mainnet) then [] else [
      command ~group ~desc:"Register and activate an Alphanet/Zeronet faucet account."
        (args2
           (Secret_key.force_switch ())
           encrypted_switch)
        (prefixes [ "activate" ; "account" ]
         @@ Secret_key.fresh_alias_param
         @@ prefixes [ "with" ]
         @@ param ~name:"activation_key"
           ~desc:"Activate an Alphanet/Zeronet faucet account from the downloaded JSON file."
           file_parameter
         @@ stop)
        (fun (force, encrypted) name activation_key_file cctxt ->
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
               activate_account cctxt
                 ~chain:`Main ~block:cctxt#block ?confirmations:cctxt#confirmations
                 ~encrypted ~force key name >>=? fun _res ->
               return_unit
        );
    ]) @
  (if version <> Some `Mainnet then [] else [
      command ~group ~desc:"Activate a fundraiser account."
        (args1 dry_run_switch)
        (prefixes [ "activate" ; "fundraiser" ; "account" ]
         @@ Public_key_hash.alias_param
         @@ prefixes [ "with" ]
         @@ param ~name:"code"
           (Clic.parameter (fun _ctx code ->
                protect (fun () ->
                    return (Blinded_public_key_hash.activation_code_of_hex code))))
           ~desc:"Activation code obtained from the Tezos foundation."
         @@ stop)
        (fun dry_run (name, _pkh) code cctxt ->
           activate_existing_account cctxt ~chain:`Main
             ~block:cctxt#block ?confirmations:cctxt#confirmations
             ~dry_run
             name code >>=? fun _res ->
           return_unit
        );
    ]) @
  [
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
      begin fun (confirmations, predecessors) operation_hash (ctxt : Proto_001_PtCJ7pwo.full) ->
        fail_when (confirmations < 0)
          (failure "confirmations cannot be negative") >>=? fun () ->
        fail_when (predecessors < 0)
          (failure "check-previous cannot be negative") >>=? fun () ->
        Client_confirmations.wait_for_operation_inclusion ctxt
          ~chain:`Main ~confirmations ~predecessors operation_hash >>=? fun _ ->
        return_unit
      end ;

    command ~group:binary_description ~desc:"Describe unsigned block header"
      no_options
      (fixed [ "describe" ; "unsigned" ; "block" ; "header" ])
      begin fun () (cctxt : Proto_001_PtCJ7pwo.full) ->
        cctxt#message "%a"
          Data_encoding.Binary_schema.pp
          (Data_encoding.Binary.describe
             (Alpha_context.Block_header.unsigned_encoding)) >>= fun () ->
        return_unit
      end ;

    command ~group:binary_description ~desc:"Describe unsigned block header"
      no_options
      (fixed [ "describe" ; "unsigned" ; "operation" ])
      begin fun () (cctxt : Proto_001_PtCJ7pwo.full) ->
        cctxt#message "%a"
          Data_encoding.Binary_schema.pp
          (Data_encoding.Binary.describe
             Alpha_context.Operation.unsigned_encoding) >>= fun () ->
        return_unit
      end

  ]
