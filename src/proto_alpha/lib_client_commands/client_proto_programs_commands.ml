(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Proto_alpha

let group =
  { Clic.name = "programs" ;
    title = "Commands for managing the library of known programs" }

open Tezos_micheline
open Client_proto_programs
open Client_proto_args

let commands () =
  let open Clic in
  let show_types_switch =
    switch
      ~long:"details"
      ~short:'v'
      ~doc:"show the types of each instruction"
      () in
  let emacs_mode_switch =
    switch
      ~long:"emacs"
      ?short:None
      ~doc:"output in `michelson-mode.el` compatible format"
      () in
  let trace_stack_switch =
    switch
      ~long:"trace-stack"
      ~doc:"show the stack after each step"
      () in
  let amount_arg =
    Client_proto_args.tez_arg
      ~parameter:"amount"
      ~doc:"amount of the transfer in \xEA\x9C\xA9"
      ~default:"0.05" in
  let custom_gas_flag =
    arg
      ~long:"gas"
      ~short:'G'
      ~doc:"Initial quantity of gas for typechecking and execution"
      ~placeholder:"gas"
      (parameter (fun _ctx str ->
           try
             let v = Z.of_string str in
             assert Compare.Z.(v >= Z.zero) ;
             return v
           with _ -> failwith "invalid gas limit (must be a positive number)")) in
  let resolve_max_gas cctxt block = function
    | None ->
        Alpha_services.Constants.hard_gas_limits cctxt (`Main, block) >>=? fun (_, gas) ->
        return gas
    | Some gas -> return gas in
  let data_parameter =
    Clic.parameter (fun _ data ->
        Lwt.return (Micheline_parser.no_parsing_error
                    @@ Michelson_v1_parser.parse_expression data)) in
  let hash_parameter =
    Clic.parameter
      (fun _cctxt hash -> return @@ MBytes.of_string hash) in
  let signature_parameter =
    Clic.parameter
      (fun _cctxt s ->
         match Signature.of_b58check_opt s with
         | Some s -> return s
         | None -> failwith "Not given a valid signature") in
  [

    command ~group ~desc: "Lists all programs in the library."
      no_options
      (fixed [ "list" ; "known" ; "programs" ])
      (fun () (cctxt : Proto_alpha.full) ->
         Program.load cctxt >>=? fun list ->
         Lwt_list.iter_s (fun (n, _) -> cctxt#message "%s" n) list >>= fun () ->
         return ()) ;

    command ~group ~desc: "Add a program to the library."
      (args1 (Program.force_switch ()))
      (prefixes [ "remember" ; "program" ]
       @@ Program.fresh_alias_param
       @@ Program.source_param
       @@ stop)
      (fun force name hash cctxt ->
         Program.of_fresh cctxt force name >>=? fun name ->
         Program.add ~force cctxt name hash) ;

    command ~group ~desc: "Remove a program from the library."
      no_options
      (prefixes [ "forget" ; "program" ]
       @@ Program.alias_param
       @@ stop)
      (fun () (name, _) cctxt -> Program.del cctxt name) ;

    command ~group ~desc: "Display a program from the library."
      no_options
      (prefixes [ "show" ; "known" ; "program" ]
       @@ Program.alias_param
       @@ stop)
      (fun () (_, program) (cctxt : Proto_alpha.full) ->
         Program.to_source program >>=? fun source ->
         cctxt#message "%s\n" source >>= fun () ->
         return ()) ;

    command ~group ~desc: "Ask the node to run a program."
      (args3 trace_stack_switch amount_arg no_print_source_flag)
      (prefixes [ "run" ; "program" ]
       @@ Program.source_param
       @@ prefixes [ "on" ; "storage" ]
       @@ Clic.param ~name:"storage" ~desc:"the storage data"
         data_parameter
       @@ prefixes [ "and" ; "input" ]
       @@ Clic.param ~name:"storage" ~desc:"the input data"
         data_parameter
       @@ stop)
      (fun (trace_exec, amount, no_print_source) program storage input cctxt ->
         Lwt.return @@ Micheline_parser.no_parsing_error program >>=? fun program ->
         let show_source = not no_print_source in
         (if trace_exec then
            trace cctxt cctxt#block ~amount ~program ~storage ~input () >>= fun res ->
            print_trace_result cctxt ~show_source ~parsed:program res
          else
            run cctxt cctxt#block ~amount ~program ~storage ~input () >>= fun res ->
            print_run_result cctxt ~show_source ~parsed:program res)) ;
    command ~group ~desc: "Ask the node to typecheck a program."
      (args4 show_types_switch emacs_mode_switch no_print_source_flag custom_gas_flag)
      (prefixes [ "typecheck" ; "program" ]
       @@ Program.source_param
       @@ stop)
      (fun (show_types, emacs_mode, no_print_source, original_gas) program cctxt ->
         match program with
         | program, [] ->
             resolve_max_gas cctxt cctxt#block original_gas >>=? fun original_gas ->
             typecheck_program cctxt cctxt#block ~gas:original_gas program >>= fun res ->
             print_typecheck_result
               ~emacs:emacs_mode
               ~show_types
               ~print_source_on_error:(not no_print_source)
               program
               res
               cctxt
         | res_with_errors when emacs_mode ->
             cctxt#message
               "(@[<v 0>(types . ())@ (errors . %a)@])"
               Michelson_v1_emacs.report_errors res_with_errors >>= fun () ->
             return ()
         | (parsed, errors) ->
             cctxt#message "%a"
               (fun ppf () ->
                  Michelson_v1_error_reporter.report_errors
                    ~details:(not no_print_source) ~parsed
                    ~show_source:(not no_print_source)
                    ppf errors) () >>= fun () ->
             cctxt#error "syntax error in program"
      ) ;

    command ~group ~desc: "Ask the node to typecheck a data expression."
      (args2 no_print_source_flag custom_gas_flag)
      (prefixes [ "typecheck" ; "data" ]
       @@ Clic.param ~name:"data" ~desc:"the data to typecheck"
         data_parameter
       @@ prefixes [ "against" ; "type" ]
       @@ Clic.param ~name:"type" ~desc:"the expected type"
         data_parameter
       @@ stop)
      (fun (no_print_source, custom_gas) data ty cctxt ->
         resolve_max_gas cctxt cctxt#block custom_gas >>=? fun original_gas ->
         Client_proto_programs.typecheck_data cctxt cctxt#block
           ~gas:original_gas ~data ~ty () >>= function
         | Ok gas ->
             cctxt#message "@[<v 0>Well typed@,Gas remaining: %a@]"
               Proto_alpha.Alpha_context.Gas.pp gas >>= fun () ->
             return ()
         | Error errs ->
             cctxt#warning "%a"
               (Michelson_v1_error_reporter.report_errors
                  ~details:false
                  ~show_source:(not no_print_source)
                  ?parsed:None) errs >>= fun () ->
             cctxt#error "ill-typed data") ;

    command ~group
      ~desc: "Ask the node to hash a data expression.\n\
              The returned hash is the same as what Michelson \
              instruction `H` would have produced."
      (args1 custom_gas_flag)
      (prefixes [ "hash" ; "data" ]
       @@ Clic.param ~name:"data" ~desc:"the data to hash"
         data_parameter
       @@ prefixes [ "of" ; "type" ]
       @@ Clic.param ~name:"type" ~desc:"type of the data"
         data_parameter
       @@ stop)
      (fun custom_gas data typ cctxt ->
         resolve_max_gas cctxt cctxt#block custom_gas >>=? fun original_gas ->
         Alpha_services.Helpers.hash_data cctxt (`Main, cctxt#block)
           (data.expanded, typ.expanded, Some original_gas) >>= function
         | Ok (hash, remaining_gas) ->
             cctxt#message "%S@,Gas remaining: %a" hash
               Proto_alpha.Alpha_context.Gas.pp remaining_gas >>= fun () ->
             return ()
         | Error errs ->
             cctxt#warning "%a"
               (Michelson_v1_error_reporter.report_errors
                  ~details:false
                  ~show_source:false
                  ?parsed:None)
               errs  >>= fun () ->
             cctxt#error "ill-formed data") ;

    command ~group
      ~desc: "Ask the node to hash a data expression.\n\
              Uses the same algorithm as Michelson instruction `H` to \
              produce the hash, signs it using a given secret key, and \
              displays it using the format expected by Michelson \
              instruction `CHECK_SIGNATURE`."
      (args1 custom_gas_flag)
      (prefixes [ "hash" ; "and" ; "sign" ; "data" ]
       @@ Clic.param ~name:"data" ~desc:"the data to hash"
         data_parameter
       @@ prefixes [ "of" ; "type" ]
       @@ Clic.param ~name:"type" ~desc:"type of the data"
         data_parameter
       @@ prefixes [ "for" ]
       @@ Client_keys.Secret_key.source_param
       @@ stop)
      (fun gas data typ sk cctxt ->
         resolve_max_gas cctxt cctxt#block gas >>=? fun gas ->
         Client_proto_programs.hash_and_sign cctxt cctxt#block
           ~gas data typ sk >>= begin function
           | Ok (hash, signature, current_gas) ->
               cctxt#message "@[<v 0>Hash: %S@,Signature: %S@,Remaining gas: %a@]"
                 hash signature
                 Proto_alpha.Alpha_context.Gas.pp current_gas
           | Error errs ->
               cctxt#warning "%a"
                 (Michelson_v1_error_reporter.report_errors
                    ~details:false
                    ~show_source:false
                    ?parsed:None)
                 errs >>= fun () ->
               cctxt#error "ill-formed data"
         end >>= return) ;

    command ~group
      ~desc: "Ask the node to check the signature of a hashed expression."
      (args1 (switch ~doc:"Use only exit codes" ~short:'q' ~long:"quiet" ()))
      (prefixes [ "check" ; "that" ]
       @@ Clic.param ~name:"hash" ~desc:"the hashed data"
         hash_parameter
       @@ prefixes [ "was" ; "signed" ; "by" ]
       @@ Client_keys.Public_key.alias_param
         ~name:"key"
       @@ prefixes [ "to" ; "produce" ]
       @@ Clic.param ~name:"signature" ~desc:"the signature to check"
         signature_parameter
       @@ stop)
      (fun quiet hashed (_, key_locator) signature (cctxt : #Proto_alpha.full) ->
         Client_keys.check key_locator signature hashed >>=? fun res ->
         begin
           if quiet
           then if res
             then return ()
             else failwith "Not signed"
           else begin if res
             then cctxt#message "Signed with key"
             else cctxt#message "Not signed with key"
           end >>= return
         end
      ) ;

  ]
