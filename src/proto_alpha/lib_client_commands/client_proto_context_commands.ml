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
    ~doc:"encrypt the key on-disk" ()

let dry_run_switch =
  Clic.switch
    ~long:"dry-run"
    ~short:'D'
    ~doc:"don't inject the operation, just display it" ()

let verbose_signing_switch =
  Clic.switch
    ~long:"verbose-signing"
    ~doc:"display extra information before signing the operation" ()

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

let data_parameter =
  Clic.parameter (fun _ data ->
      Lwt.return (Micheline_parser.no_parsing_error
                  @@ Michelson_v1_parser.parse_expression data))

let non_negative_param =
  Clic.parameter (fun _ s ->
      match int_of_string_opt s with
      | Some i when i >= 0 -> return i
      | _ -> failwith "Parameter should be a non-negative integer literal")

let block_hash_param =
  Clic.parameter (fun _ s ->
      try return (Block_hash.of_b58check_exn s)
      with _ ->
        failwith "Parameter '%s' is an invalid block hash" s)

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
      begin fun seconds (cctxt : Proto_alpha.full) ->
        Shell_services.Blocks.Header.shell_header
          cctxt ~chain:cctxt#chain ~block:cctxt#block () >>=? fun { timestamp = v ; _ } ->
        begin
          if seconds
          then cctxt#message "%Ld" (Time.Protocol.to_seconds v)
          else cctxt#message "%s" (Time.Protocol.to_notation v)
        end >>= fun () ->
        return_unit
      end ;

    command ~group ~desc: "Lists all non empty contracts of the block."
      no_options
      (fixed [ "list" ; "contracts" ])
      begin fun () (cctxt : Proto_alpha.full) ->
        list_contract_labels cctxt
          ~chain:cctxt#chain ~block:cctxt#block >>=? fun contracts ->
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
      begin fun () (_, contract) (cctxt : Proto_alpha.full) ->
        get_balance cctxt
          ~chain:cctxt#chain ~block:cctxt#block
          contract >>=? fun amount ->
        cctxt#answer "%a %s" Tez.pp amount Client_proto_args.tez_sym >>= fun () ->
        return_unit
      end ;

    command ~group ~desc: "Get the storage of a contract."
      no_options
      (prefixes [ "get" ; "script" ; "storage" ; "for" ]
       @@ ContractAlias.destination_param ~name:"src" ~desc:"source contract"
       @@ stop)
      begin fun () (_, contract) (cctxt : Proto_alpha.full) ->
        get_storage cctxt
          ~chain:cctxt#chain ~block:cctxt#block
          contract >>=? function
        | None ->
            cctxt#error "This is not a smart contract."
        | Some storage ->
            cctxt#answer "%a" Michelson_v1_printer.print_expr_unwrapped storage >>= fun () ->
            return_unit
      end ;

    command ~group ~desc: "Get the value associated to a key in the big map storage of a contract."
      no_options
      (prefixes [ "get" ; "big" ; "map" ; "value" ; "for" ]
       @@ Clic.param ~name:"key" ~desc:"the key to look for"
         data_parameter
       @@ prefixes [ "of" ; "type" ]
       @@ Clic.param ~name:"type" ~desc:"type of the key"
         data_parameter
       @@ prefix "in"
       @@ ContractAlias.destination_param ~name:"src" ~desc:"source contract"
       @@ stop)
      begin fun () key key_type (_, contract) (cctxt : Proto_alpha.full) ->
        get_big_map_value cctxt
          ~chain:cctxt#chain ~block:cctxt#block
          contract (key.expanded, key_type.expanded) >>=? function
        | None ->
            cctxt#error "No value associated to this key."
        | Some value ->
            cctxt#answer "%a" Michelson_v1_printer.print_expr_unwrapped value >>= fun () ->
            return_unit
      end ;

    command ~group ~desc: "Get the storage of a contract."
      no_options
      (prefixes [ "get" ; "script" ; "code" ; "for" ]
       @@ ContractAlias.destination_param ~name:"src" ~desc:"source contract"
       @@ stop)
      begin fun () (_, contract) (cctxt : Proto_alpha.full) ->
        get_script cctxt
          ~chain:cctxt#chain ~block:cctxt#block
          contract >>=? function
        | None ->
            cctxt#error "This is not a smart contract."
        | Some { code ; storage = _ } ->
            match Script_repr.force_decode code with
            | Error errs -> cctxt#error "%a" (Format.pp_print_list ~pp_sep:Format.pp_print_newline Alpha_environment.Error_monad.pp) errs
            | Ok (code, _) ->
                let { Michelson_v1_parser.source ; _ } =
                  Michelson_v1_printer.unparse_toplevel code in
                cctxt#answer "%a" Format.pp_print_text source >>= return
      end ;

    command ~group ~desc: "Get the manager of a contract."
      no_options
      (prefixes [ "get" ; "manager" ; "for" ]
       @@ ContractAlias.destination_param ~name:"src" ~desc:"source contract"
       @@ stop)
      begin fun () (_, contract) (cctxt : Proto_alpha.full) ->
        Client_proto_contracts.get_manager cctxt
          ~chain:cctxt#chain ~block:cctxt#block
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
      begin fun () (_, contract) (cctxt : Proto_alpha.full) ->
        Client_proto_contracts.get_delegate cctxt
          ~chain:cctxt#chain ~block:cctxt#block
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
      (args9
         fee_arg dry_run_switch verbose_signing_switch
         minimal_fees_arg
         minimal_nanotez_per_byte_arg
         minimal_nanotez_per_gas_unit_arg
         force_low_fee_arg
         fee_cap_arg
         burn_cap_arg)
      (prefixes [ "set" ; "delegate" ; "for" ]
       @@ ContractAlias.destination_param ~name:"src" ~desc:"source contract"
       @@ prefix "to"
       @@ Public_key_hash.alias_param
         ~name: "mgr" ~desc: "new delegate of the contract"
       @@ stop)
      begin fun
        (fee, dry_run, verbose_signing, minimal_fees, minimal_nanotez_per_byte,
         minimal_nanotez_per_gas_unit, force_low_fee, fee_cap, burn_cap)
        (_, contract) (_, delegate) (cctxt : Proto_alpha.full) ->
        let fee_parameter = {
          Injection.minimal_fees ;
          minimal_nanotez_per_byte ;
          minimal_nanotez_per_gas_unit ;
          force_low_fee ;
          fee_cap ;
          burn_cap ;
        } in
        source_to_keys cctxt
          ~chain:cctxt#chain ~block:cctxt#block
          contract >>=? fun (src_pk, manager_sk) ->
        set_delegate cctxt
          ~chain:cctxt#chain ~block:cctxt#block ?confirmations:cctxt#confirmations
          ~dry_run ~verbose_signing
          ~fee_parameter
          ?fee
          contract (Some delegate) ~src_pk ~manager_sk >>=? fun _ ->
        return_unit
      end ;

    command ~group ~desc: "Withdraw the delegate from a contract."
      (args9
         fee_arg dry_run_switch verbose_signing_switch
         minimal_fees_arg
         minimal_nanotez_per_byte_arg
         minimal_nanotez_per_gas_unit_arg
         force_low_fee_arg
         fee_cap_arg
         burn_cap_arg)
      (prefixes [ "withdraw" ; "delegate" ; "from" ]
       @@ ContractAlias.destination_param ~name:"src" ~desc:"source contract"
       @@ stop)
      begin fun (fee, dry_run, verbose_signing, minimal_fees, minimal_nanotez_per_byte,
                 minimal_nanotez_per_gas_unit, force_low_fee, fee_cap, burn_cap)
        (_, contract) (cctxt : Proto_alpha.full) ->
        source_to_keys cctxt
          ~chain:cctxt#chain ~block:cctxt#block
          contract >>=? fun (src_pk, manager_sk) ->
        let fee_parameter = {
          Injection.minimal_fees ;
          minimal_nanotez_per_byte ;
          minimal_nanotez_per_gas_unit ;
          force_low_fee ;
          fee_cap ;
          burn_cap ;
        } in
        set_delegate cctxt
          ~chain:cctxt#chain ~block:cctxt#block ?confirmations:cctxt#confirmations
          ~dry_run ~verbose_signing
          ~fee_parameter
          contract None ?fee ~src_pk ~manager_sk >>=? fun _ ->
        return_unit
      end ;

    command ~group ~desc:"Open a new account."
      (args12 fee_arg dry_run_switch verbose_signing_switch
         delegate_arg delegatable_switch (Client_keys.force_switch ())
         minimal_fees_arg
         minimal_nanotez_per_byte_arg
         minimal_nanotez_per_gas_unit_arg
         force_low_fee_arg
         fee_cap_arg
         burn_cap_arg)
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
      begin fun (fee, dry_run, verbose_signing, delegate, delegatable, force,
                 minimal_fees, minimal_nanotez_per_byte,
                 minimal_nanotez_per_gas_unit, force_low_fee, fee_cap, burn_cap)
        new_contract manager_pkh balance (_, source) (cctxt : Proto_alpha.full) ->
        RawContractAlias.of_fresh cctxt force new_contract >>=? fun alias_name ->
        source_to_keys cctxt
          ~chain:cctxt#chain ~block:cctxt#block
          source >>=? fun (src_pk, src_sk) ->
        let fee_parameter = {
          Injection.minimal_fees ;
          minimal_nanotez_per_byte ;
          minimal_nanotez_per_gas_unit ;
          force_low_fee ;
          fee_cap ;
          burn_cap ;
        } in
        originate_account cctxt
          ~chain:cctxt#chain ~block:cctxt#block ?confirmations:cctxt#confirmations
          ~dry_run ~verbose_signing
          ?fee ?delegate ~delegatable ~manager_pkh ~balance
          ~fee_parameter
          ~source ~src_pk ~src_sk () >>=? fun (_res, contract) ->
        if dry_run then
          return_unit
        else
          save_contract ~force cctxt alias_name contract >>=? fun () ->
          return_unit
      end ;

    command ~group ~desc: "Launch a smart contract on the blockchain."
      (args17
         fee_arg
         dry_run_switch verbose_signing_switch
         gas_limit_arg storage_limit_arg delegate_arg (Client_keys.force_switch ())
         delegatable_switch spendable_switch init_arg no_print_source_flag
         minimal_fees_arg
         minimal_nanotez_per_byte_arg
         minimal_nanotez_per_gas_unit_arg
         force_low_fee_arg
         fee_cap_arg
         burn_cap_arg)
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
      begin fun (fee, dry_run, verbose_signing, gas_limit, storage_limit,
                 delegate, force, delegatable, spendable, initial_storage,
                 no_print_source, minimal_fees, minimal_nanotez_per_byte,
                 minimal_nanotez_per_gas_unit, force_low_fee, fee_cap, burn_cap)
        alias_name manager balance (_, source) program (cctxt : Proto_alpha.full) ->
        RawContractAlias.of_fresh cctxt force alias_name >>=? fun alias_name ->
        Lwt.return (Micheline_parser.no_parsing_error program) >>=? fun { expanded = code ; _ } ->
        source_to_keys cctxt
          ~chain:cctxt#chain ~block:cctxt#block
          source >>=? fun (src_pk, src_sk) ->
        let fee_parameter = {
          Injection.minimal_fees ;
          minimal_nanotez_per_byte ;
          minimal_nanotez_per_gas_unit ;
          force_low_fee ;
          fee_cap ;
          burn_cap ;
        } in
        originate_contract cctxt
          ~chain:cctxt#chain ~block:cctxt#block ?confirmations:cctxt#confirmations
          ~dry_run ~verbose_signing
          ?fee ?gas_limit ?storage_limit ~delegate ~delegatable ~spendable ~initial_storage
          ~manager ~balance ~source ~src_pk ~src_sk ~code
          ~fee_parameter
          () >>= fun errors ->
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
      (args14 fee_arg dry_run_switch verbose_signing_switch
         gas_limit_arg storage_limit_arg counter_arg arg_arg no_print_source_flag
         minimal_fees_arg
         minimal_nanotez_per_byte_arg
         minimal_nanotez_per_gas_unit_arg
         force_low_fee_arg
         fee_cap_arg
         burn_cap_arg)
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
      begin fun (fee, dry_run, verbose_signing, gas_limit, storage_limit,
                 counter, arg, no_print_source, minimal_fees,
                 minimal_nanotez_per_byte, minimal_nanotez_per_gas_unit,
                 force_low_fee, fee_cap, burn_cap)
        amount (_, source) (_, destination) cctxt ->
        source_to_keys cctxt
          ~chain:cctxt#chain ~block:cctxt#block
          source >>=? fun (src_pk, src_sk) ->
        let fee_parameter = {
          Injection.minimal_fees ;
          minimal_nanotez_per_byte ;
          minimal_nanotez_per_gas_unit ;
          force_low_fee ;
          fee_cap ;
          burn_cap ;
        } in
        transfer cctxt
          ~chain:cctxt#chain ~block:cctxt#block ?confirmations:cctxt#confirmations
          ~dry_run ~verbose_signing
          ~fee_parameter
          ~source ?fee ~src_pk ~src_sk ~destination ?arg ~amount ?gas_limit ?storage_limit ?counter () >>=
        report_michelson_errors ~no_print_source ~msg:"transfer simulation failed" cctxt >>= function
        | None -> return_unit
        | Some (_res, _contracts) ->
            return_unit
      end;

    command ~group ~desc: "Reveal the public key of the contract manager."
      (args9 fee_arg
         dry_run_switch verbose_signing_switch
         minimal_fees_arg
         minimal_nanotez_per_byte_arg
         minimal_nanotez_per_gas_unit_arg
         force_low_fee_arg
         fee_cap_arg
         burn_cap_arg)
      (prefixes [ "reveal" ; "key" ; "for" ]
       @@ ContractAlias.alias_param
         ~name: "src" ~desc: "name of the source contract"
       @@ stop)
      begin fun (fee, dry_run, verbose_signing, minimal_fees, minimal_nanotez_per_byte,
                 minimal_nanotez_per_gas_unit, force_low_fee, fee_cap, burn_cap) (_, source) cctxt ->
        source_to_keys cctxt
          ~chain:cctxt#chain ~block:cctxt#block
          source >>=? fun (src_pk, src_sk) ->
        let fee_parameter = {
          Injection.minimal_fees ;
          minimal_nanotez_per_byte ;
          minimal_nanotez_per_gas_unit ;
          force_low_fee ;
          fee_cap ;
          burn_cap ;
        } in
        reveal cctxt ~dry_run ~verbose_signing
          ~chain:cctxt#chain ~block:cctxt#block ?confirmations:cctxt#confirmations
          ~source ?fee ~src_pk ~src_sk
          ~fee_parameter
          () >>=? fun _res ->
        return_unit
      end;

    command ~group ~desc: "Register the public key hash as a delegate."
      (args9 fee_arg dry_run_switch verbose_signing_switch
         minimal_fees_arg
         minimal_nanotez_per_byte_arg
         minimal_nanotez_per_gas_unit_arg
         force_low_fee_arg
         fee_cap_arg
         burn_cap_arg)
      (prefixes [ "register" ; "key" ]
       @@ Public_key_hash.source_param
         ~name: "mgr" ~desc: "the delegate key"
       @@ prefixes [ "as" ; "delegate" ]
       @@ stop)
      begin fun (fee, dry_run, verbose_signing, minimal_fees, minimal_nanotez_per_byte,
                 minimal_nanotez_per_gas_unit, force_low_fee, fee_cap, burn_cap)  src_pkh cctxt ->
        Client_keys.get_key cctxt src_pkh >>=? fun (_, src_pk, src_sk) ->
        let fee_parameter = {
          Injection.minimal_fees ;
          minimal_nanotez_per_byte ;
          minimal_nanotez_per_gas_unit ;
          force_low_fee ;
          fee_cap ;
          burn_cap ;
        } in
        register_as_delegate cctxt
          ~chain:cctxt#chain ~block:cctxt#block ?confirmations:cctxt#confirmations
          ~dry_run ~fee_parameter ~verbose_signing
          ?fee ~manager_sk:src_sk src_pk
        >>= function
        | Ok _ -> return_unit
        | Error (Alpha_environment.Ecoproto_error
                   Proto_alpha.Proto.Delegate_storage.Active_delegate  :: []) ->
            cctxt#message "Delegate already activated." >>= fun () ->
            return_unit
        | Error el -> Lwt.return (Error el)
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
                 ~chain:cctxt#chain ~block:cctxt#block ?confirmations:cctxt#confirmations
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
           activate_existing_account cctxt ~chain:cctxt#chain
             ~block:cctxt#block ?confirmations:cctxt#confirmations
             ~dry_run
             name code >>=? fun _res ->
           return_unit
        );
    ]) @
  [
    command ~desc:"Wait until an operation is included in a block"
      (args3
         (default_arg
            ~long:"confirmations"
            ~placeholder:"num_blocks"
            ~doc:"wait until 'N' additional blocks after the operation \
                  appears in the considered chain"
            ~default:"0"
            non_negative_param)
         (default_arg
            ~long:"check-previous"
            ~placeholder:"num_blocks"
            ~doc:"number of previous blocks to check"
            ~default:"10"
            non_negative_param)
         (arg
            ~long:"branch"
            ~placeholder:"block_hash"
            ~doc:"hash of the oldest block where we should look for the operation"
            block_hash_param))
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
      begin fun (confirmations, predecessors, branch) operation_hash (ctxt : Proto_alpha.full) ->
        Client_confirmations.wait_for_operation_inclusion ctxt
          ~chain:ctxt#chain ~confirmations ~predecessors ?branch operation_hash >>=? fun _ ->
        return_unit
      end ;

    command ~desc:"Get receipt for past operation"
      (args1
         (default_arg
            ~long:"check-previous"
            ~placeholder:"num_blocks"
            ~doc:"number of previous blocks to check"
            ~default:"10"
            non_negative_param))
      (prefixes [ "get" ; "receipt"; "for" ]
       @@ param
         ~name:"operation"
         ~desc:"Operation to be looked up"
         (parameter
            (fun _ x ->
               match Operation_hash.of_b58check_opt x with
               | None -> Error_monad.failwith "Invalid operation hash: '%s'" x
               | Some hash -> return hash))
       @@ stop)
      begin fun predecessors operation_hash (ctxt : Proto_alpha.full) ->
        display_receipt_for_operation ctxt
          ~chain:ctxt#chain ~predecessors operation_hash >>=? fun _ ->
        return_unit
      end ;

    command ~group:binary_description ~desc:"Describe unsigned block header"
      no_options
      (fixed [ "describe" ; "unsigned" ; "block" ; "header" ])
      begin fun () (cctxt : Proto_alpha.full) ->
        cctxt#message "%a"
          Data_encoding.Binary_schema.pp
          (Data_encoding.Binary.describe
             (Alpha_context.Block_header.unsigned_encoding)) >>= fun () ->
        return_unit
      end ;

    command ~group:binary_description ~desc:"Describe unsigned block header"
      no_options
      (fixed [ "describe" ; "unsigned" ; "operation" ])
      begin fun () (cctxt : Proto_alpha.full) ->
        cctxt#message "%a"
          Data_encoding.Binary_schema.pp
          (Data_encoding.Binary.describe
             Alpha_context.Operation.unsigned_encoding) >>= fun () ->
        return_unit
      end ;

    command ~group ~desc: "Submit protocol proposals"
      (args3
         dry_run_switch
         verbose_signing_switch
         (switch
            ~doc:"Do not fail when the checks that try to prevent the user \
                  from shooting themselves in the foot do."
            ~long:"force" ()))
      (prefixes [ "submit" ; "proposals" ; "for" ]
       @@ ContractAlias.destination_param
         ~name: "delegate"
         ~desc: "the delegate who makes the proposal"
       @@ seq_of_param
         (param
            ~name:"proposal"
            ~desc:"the protocol hash proposal to be submitted"
            (parameter
               (fun _ x ->
                  match Protocol_hash.of_b58check_opt x with
                  | None -> Error_monad.failwith "Invalid proposal hash: '%s'" x
                  | Some hash -> return hash))))
      begin fun (dry_run, verbose_signing, force)
        (_name, source) proposals (cctxt : Proto_alpha.full) ->
        get_period_info ~chain:cctxt#chain ~block:cctxt#block cctxt >>=? fun info ->
        begin match info.current_period_kind with
          | Proposal -> return_unit
          | _ -> cctxt#error "Not in a proposal period"
        end >>=? fun () ->
        Shell_services.Protocol.list cctxt >>=? fun known_protos ->
        get_proposals ~chain:cctxt#chain ~block:cctxt#block cctxt >>=? fun known_proposals ->
        Alpha_services.Voting.listings cctxt (cctxt#chain, cctxt#block) >>=? fun listings ->
        Client_proto_context.get_manager
          cctxt ~chain:cctxt#chain ~block:cctxt#block
          source >>=? fun (src_name, src_pkh, _src_pk, src_sk) ->
        (* for a proposal to be valid it must either a protocol that was already
           proposed by somebody else or a protocol known by the node, because
           the user is the first proposer and just injected it with
           tezos-admin-client *)
        let check_proposals proposals : bool tzresult Lwt.t =
          let n = List.length proposals in
          let errors = ref [] in
          let error ppf =
            Format.kasprintf (fun s -> errors := s :: !errors) ppf in
          if n = 0 then error "Empty proposal list." ;
          if n > Constants.fixed.max_proposals_per_delegate then
            error "Too many proposals: %d > %d."
              n Constants.fixed.max_proposals_per_delegate ;
          begin match
              Base.List.find_all_dups ~compare:Protocol_hash.compare proposals with
          | [] -> ()
          | dups ->
              error "There %s: %a."
                (if List.length dups = 1
                 then "is a duplicate proposal"
                 else "are duplicate proposals")
                Format.(
                  pp_print_list
                    ~pp_sep:(fun ppf () -> pp_print_string ppf ", ")
                    Protocol_hash.pp)
                dups
          end ;
          List.iter (fun (p : Protocol_hash.t) ->
              if (List.mem p known_protos) ||
                 (Alpha_environment.Protocol_hash.Map.mem p known_proposals)
              then ()
              else
                error "Protocol %a is not a known proposal." Protocol_hash.pp p
            ) proposals ;
          if not (
              List.exists
                (fun (pkh, _) -> Signature.Public_key_hash.equal pkh src_pkh)
                listings)
          then
            error "Public-key-hash `%a` from account `%s` does not appear to \
                   have voting rights."
              Signature.Public_key_hash.pp src_pkh
              src_name ;
          if !errors <> []
          then
            cctxt#message "There %s with the submission:%t"
              (if List.length !errors = 1 then "is an issue" else "are issues")
              Format.(fun ppf ->
                  pp_print_cut ppf () ;
                  pp_open_vbox ppf 0 ;
                  List.iter (fun msg ->
                      pp_open_hovbox ppf 2 ;
                      pp_print_string ppf "* ";
                      pp_print_text ppf msg ;
                      pp_close_box ppf () ; 
                      pp_print_cut ppf ())
                    !errors ;
                  pp_close_box ppf ())
            >>= fun () ->
            return_false
          else
            return_true
        in
        check_proposals proposals >>=? fun all_valid ->
        begin if all_valid then
            cctxt#message "All proposals are valid."
          else if force then
            cctxt#message
              "Some proposals are not valid, but `--force` was used."
          else
            cctxt#error "Submission failed because of invalid proposals."
        end >>= fun () ->
        submit_proposals ~dry_run ~verbose_signing
          cctxt ~chain:cctxt#chain ~block:cctxt#block ~src_sk src_pkh
          proposals >>= function
        | Ok _res -> return_unit
        | Error errs ->
            begin match errs with
              | Unregistred_error
                  (`O [ "kind", `String "generic" ;
                        "error", `String msg ]) :: [] ->
                  cctxt#message "Error:@[<hov>@.%a@]"
                    Format.pp_print_text
                    (String.split_on_char ' ' msg
                     |> List.filter (function "" | "\n" -> false | _ -> true)
                     |> String.concat " "
                     |> String.map (function '\n' | '\t' -> ' ' | c -> c))
              | el ->
                  cctxt#message "Error:@ %a" pp_print_error el
            end
            >>= fun () ->
            failwith "Failed to submit proposals"
      end ;

    command ~group ~desc: "Submit a ballot"
      (args1 dry_run_switch)
      (prefixes [ "submit" ; "ballot" ; "for" ]
       @@ ContractAlias.destination_param
         ~name: "delegate"
         ~desc: "the delegate who votes"
       @@ param
         ~name:"proposal"
         ~desc:"the protocol hash proposal to vote for"
         (parameter
            (fun _ x ->
               match Protocol_hash.of_b58check_opt x with
               | None -> failwith "Invalid proposal hash: '%s'" x
               | Some hash -> return hash))
       @@ param
         ~name:"ballot"
         ~desc:"the ballot value (yea/yay, nay, or pass)"
         (parameter
            ~autocomplete: (fun _ -> return [ "yea" ; "nay" ; "pass" ])
            (fun _ s -> (* We should have [Vote.of_string]. *)
               match String.lowercase_ascii s with
               | "yay" | "yea" -> return Vote.Yay
               | "nay" -> return Vote.Nay
               | "pass" -> return Vote.Pass
               | s -> failwith "Invalid ballot: '%s'" s))
       @@ stop)
      begin fun dry_run (_name, source) proposal ballot (cctxt : Proto_alpha.full) ->
        get_period_info ~chain:cctxt#chain ~block:cctxt#block cctxt >>=? fun info ->
        begin match info.current_period_kind with
          | Testing_vote | Promotion_vote -> return_unit
          | _ -> cctxt#error "Not in a Testing_vote or Promotion_vote period"
        end >>=? fun () ->
        Client_proto_context.get_manager
          cctxt ~chain:cctxt#chain ~block:cctxt#block
          source >>=? fun (_src_name, src_pkh, _src_pk, src_sk) ->
        submit_ballot cctxt ~chain:cctxt#chain ~block:cctxt#block ~src_sk src_pkh
          ~dry_run proposal ballot >>=? fun _res ->
        return_unit
      end ;

    command ~group ~desc: "Summarize the current voting period"
      no_options
      (fixed [ "show" ; "voting" ; "period" ])
      begin fun () (cctxt : Proto_alpha.full) ->
        get_period_info ~chain:cctxt#chain ~block:cctxt#block cctxt >>=? fun info ->
        cctxt#message "Current period: %a\n\
                       Blocks remaining until end of period: %ld"
          Data_encoding.Json.pp
          (Data_encoding.Json.construct
             Proto_alpha.Alpha_context.Voting_period.kind_encoding
             info.current_period_kind)
          info.remaining >>= fun () ->
        Shell_services.Protocol.list cctxt >>=? fun known_protos ->
        get_proposals ~chain:cctxt#chain ~block:cctxt#block cctxt >>=? fun props ->
        let ranks = Alpha_environment.Protocol_hash.Map.bindings props |>
                    List.sort (fun (_,v1) (_,v2) -> Int32.(compare v2 v1)) in
        let print_proposal = function
          | None -> assert false (* not called during proposal phase *)
          | Some proposal -> cctxt#message "Current proposal: %a"
                               Protocol_hash.pp proposal
        in
        match info.current_period_kind with
        | Proposal ->
            cctxt#answer "Current proposals:%t"
              Format.(fun ppf ->
                  pp_print_cut ppf () ;
                  pp_open_vbox ppf 0 ;
                  List.iter
                    (fun (p, w) ->
                       fprintf ppf "* %a %ld (%sknown by the node)@."
                         Protocol_hash.pp p w
                         (if (List.mem p known_protos) then "" else "not "))
                    ranks ;
                  pp_close_box ppf () )
            >>= fun () -> return_unit
        | Testing_vote | Promotion_vote ->
            print_proposal info.current_proposal >>= fun () ->
            get_ballots_info ~chain:cctxt#chain ~block:cctxt#block cctxt >>=? fun ballots_info ->
            cctxt#answer "Ballots: %a@,\
                          Current participation %.2f%%, necessary quorum %.2f%%@,\
                          Current in favor %ld, needed supermajority %ld"
              Data_encoding.Json.pp (Data_encoding.Json.construct
                                       Vote.ballots_encoding ballots_info.ballots)
              (Int32.to_float ballots_info.participation /. 100.)
              (Int32.to_float ballots_info.current_quorum /. 100.)
              ballots_info.ballots.yay
              ballots_info.supermajority
            >>= fun () -> return_unit
        | Testing -> print_proposal info.current_proposal >>= fun () ->
            return_unit
      end ;

  ]
