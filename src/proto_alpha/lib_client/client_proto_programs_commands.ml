(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

let group =
  { Cli_entries.name = "programs" ;
    title = "Commands for managing the library of known programs" }

open Tezos_micheline
open Client_proto_programs
open Client_proto_args

let commands () =
  let open Cli_entries in
  let show_types_switch =
    switch
      ~parameter:"-details"
      ~doc:"show the types of each instruction" in
  let emacs_mode_switch =
    switch
      ~parameter:"-emacs"
      ~doc:"output in `michelson-mode.el` compatible format" in
  let trace_stack_switch =
    switch
      ~parameter:"-trace-stack"
      ~doc:"show the stack after each step" in
  let amount_arg =
    Client_proto_args.tez_arg
      ~parameter:"-amount"
      ~doc:"amount of the transfer in \xEA\x9C\xA9"
      ~default:"0.05" in
  let data_parameter =
    Cli_entries.parameter (fun _ data ->
        Lwt.return (Micheline_parser.no_parsing_error
                    @@ Michelson_v1_parser.parse_expression data)) in
  [

    command ~group ~desc: "Lists all programs in the library."
      no_options
      (fixed [ "list" ; "known" ; "programs" ])
      (fun () (cctxt : Client_commands.full_context) ->
         Program.load cctxt >>=? fun list ->
         Lwt_list.iter_s (fun (n, _) -> cctxt#message "%s" n) list >>= fun () ->
         return ()) ;

    command ~group ~desc: "Add a program to the library."
      (args1 Program.force_switch)
      (prefixes [ "remember" ; "program" ]
       @@ Program.fresh_alias_param
       @@ Program.source_param
       @@ stop)
      (fun force name hash (cctxt : Client_commands.full_context) ->
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
      (fun () (_, program) (cctxt : Client_commands.full_context) ->
         Program.to_source program >>=? fun source ->
         cctxt#message "%s\n" source >>= fun () ->
         return ()) ;

    command ~group ~desc: "Ask the node to run a program."
      (args3 trace_stack_switch amount_arg no_print_source_flag)
      (prefixes [ "run" ; "program" ]
       @@ Program.source_param
       @@ prefixes [ "on" ; "storage" ]
       @@ Cli_entries.param ~name:"storage" ~desc:"the storage data"
         data_parameter
       @@ prefixes [ "and" ; "input" ]
       @@ Cli_entries.param ~name:"storage" ~desc:"the input data"
         data_parameter
       @@ stop)
      (fun (trace_exec, amount, no_print_source) program storage input cctxt ->
         Lwt.return @@ Micheline_parser.no_parsing_error program >>=? fun program ->
         let show_source = not no_print_source in
         (if trace_exec then
            trace ~amount ~program ~storage ~input cctxt#block cctxt >>= fun res ->
            print_trace_result cctxt ~show_source ~parsed:program res
          else
            run ~amount ~program ~storage ~input cctxt#block cctxt >>= fun res ->
            print_run_result cctxt ~show_source ~parsed:program res)) ;

    command ~group ~desc: "Ask the node to typecheck a program."
      (args3 show_types_switch emacs_mode_switch no_print_source_flag)
      (prefixes [ "typecheck" ; "program" ]
       @@ Program.source_param
       @@ stop)
      (fun (show_types, emacs_mode, no_print_source) program cctxt ->
         Lwt.return @@ Micheline_parser.no_parsing_error program >>=? fun program ->
         typecheck_program program cctxt#block cctxt >>= fun res ->
         print_typecheck_result
           ~emacs:emacs_mode
           ~show_types
           ~print_source_on_error:(not no_print_source)
           program
           res
           cctxt) ;

    command ~group ~desc: "Ask the node to typecheck a data expression."
      (args1 no_print_source_flag)
      (prefixes [ "typecheck" ; "data" ]
       @@ Cli_entries.param ~name:"data" ~desc:"the data to typecheck"
         data_parameter
       @@ prefixes [ "against" ; "type" ]
       @@ Cli_entries.param ~name:"type" ~desc:"the expected type"
         data_parameter
       @@ stop)
      (fun no_print_source data ty cctxt ->
         Client_proto_programs.typecheck_data ~data ~ty cctxt#block cctxt >>= function
         | Ok () ->
             cctxt#message "Well typed" >>= fun () ->
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
      no_options
      (prefixes [ "hash" ; "data" ]
       @@ Cli_entries.param ~name:"data" ~desc:"the data to hash"
         data_parameter
       @@ prefixes [ "of" ; "type" ]
       @@ Cli_entries.param ~name:"type" ~desc:"type of the data"
         data_parameter
       @@ stop)
      (fun () data typ cctxt ->
         Client_proto_rpcs.Helpers.hash_data cctxt
           cctxt#block (data.expanded, typ.expanded) >>= function
         | Ok hash ->
             cctxt#message "%S" hash >>= fun () ->
             return ()
         | Error errs ->
             cctxt#warning "%a" pp_print_error errs  >>= fun () ->
             cctxt#error "ill-formed data") ;

    command ~group
      ~desc: "Ask the node to hash a data expression.\n\
              Uses the same algorithm as Michelson instruction `H` to \
              produce the hash, signs it using a given secret key, and \
              displays it using the format expected by Michelson \
              instruction `CHECK_SIGNATURE`."
      no_options
      (prefixes [ "hash" ; "and" ; "sign" ; "data" ]
       @@ Cli_entries.param ~name:"data" ~desc:"the data to hash"
         data_parameter
       @@ prefixes [ "of" ; "type" ]
       @@ Cli_entries.param ~name:"type" ~desc:"type of the data"
         data_parameter
       @@ prefixes [ "for" ]
       @@ Client_keys.Secret_key.alias_param
       @@ stop)
      (fun () data typ (_, key) cctxt ->
         Client_proto_programs.hash_and_sign data typ key cctxt#block cctxt >>= begin function
           | Ok (hash, signature) ->
               cctxt#message "@[<v 0>Hash: %S@,Signature: %S@]" hash signature
           | Error errs ->
               cctxt#warning "%a" pp_print_error errs >>= fun () ->
               cctxt#error "ill-formed data"
         end >>= return) ;

  ]
