(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Tezos_micheline

open Michelson_v1_printer

module Program = Client_aliases.Alias (struct
    type t = Michelson_v1_parser.parsed Micheline_parser.parsing_result
    let encoding =
      Data_encoding.conv
        (fun ({ Michelson_v1_parser.source }, _) -> source)
        (fun source -> Michelson_v1_parser.parse_toplevel source)
        Data_encoding.string
    let of_source _cctxt source =
      return (Michelson_v1_parser.parse_toplevel source)
    let to_source _ ({ Michelson_v1_parser.source }, _) = return source
    let name = "program"
  end)

let print_errors (cctxt : #Client_commands.logger) errs ~show_source ~parsed =
  cctxt#warning "%a"
    (Michelson_v1_error_reporter.report_errors
       ~details:false
       ~show_source
       ~parsed) errs >>= fun () ->
  cctxt#error "error running program" >>= fun () ->
  return ()

let print_run_result (cctxt : #Client_commands.logger) ~show_source ~parsed = function
  | Ok (storage, output) ->
      cctxt#message "@[<v 0>@[<v 2>storage@,%a@]@,@[<v 2>output@,%a@]@]@."
        print_expr storage
        print_expr output >>= fun () ->
      return ()
  | Error errs ->
      print_errors cctxt errs ~show_source ~parsed

let print_trace_result (cctxt : #Client_commands.logger) ~show_source ~parsed =
  function
  | Ok (storage, output, trace) ->
      cctxt#message
        "@[<v 0>@[<v 2>storage@,%a@]@,\
         @[<v 2>output@,%a@]@,@[<v 2>trace@,%a@]@]@."
        print_expr storage
        print_expr output
        (Format.pp_print_list
           (fun ppf (loc, gas, stack) ->
              Format.fprintf ppf
                "- @[<v 0>location: %d (remaining gas: %d)@,\
                 [ @[<v 0>%a ]@]@]"
                loc gas
                (Format.pp_print_list print_expr)
                stack))
        trace >>= fun () ->
      return ()
  | Error errs ->
      print_errors cctxt errs ~show_source ~parsed

let run
    ?(amount = Tez.fifty_cents)
    ~(program : Michelson_v1_parser.parsed)
    ~(storage : Michelson_v1_parser.parsed)
    ~(input : Michelson_v1_parser.parsed)
    block
    (cctxt : #Client_rpcs.rpc_sig) =
  Client_proto_rpcs.Helpers.run_code cctxt
    block program.expanded (storage.expanded, input.expanded, amount)

let trace
    ?(amount = Tez.fifty_cents)
    ~(program : Michelson_v1_parser.parsed)
    ~(storage : Michelson_v1_parser.parsed)
    ~(input : Michelson_v1_parser.parsed)
    block
    (cctxt : #Client_rpcs.rpc_sig) =
  Client_proto_rpcs.Helpers.trace_code cctxt
    block program.expanded (storage.expanded, input.expanded, amount)

let hash_and_sign (data : Michelson_v1_parser.parsed) key block cctxt =
  Client_proto_rpcs.Helpers.hash_data cctxt block (data.expanded) >>=? fun hash ->
  let signature = Ed25519.sign key (MBytes.of_string hash) in
  return (hash,
          signature |>
          Data_encoding.Binary.to_bytes Ed25519.Signature.encoding |>
          Hex_encode.hex_of_bytes)

let typecheck_data
    ~(data : Michelson_v1_parser.parsed)
    ~(ty : Michelson_v1_parser.parsed)
    block cctxt =
  Client_proto_rpcs.Helpers.typecheck_data cctxt block (data.expanded, ty.expanded)

let typecheck_program (program : Michelson_v1_parser.parsed) block cctxt =
  Client_proto_rpcs.Helpers.typecheck_code cctxt block program.expanded

let print_typecheck_result
    ~emacs ~show_types ~print_source_on_error
    program res (cctxt : #Client_commands.logger) =
  if emacs then
    let type_map, errs = match res with
      | Ok type_map -> type_map, []
      | Error (Environment.Ecoproto_error
                 (Script_ir_translator.Ill_typed_contract (_, type_map ) :: _)
               :: _ as errs) ->
          type_map, errs
      | Error errs ->
          [], errs in
    cctxt#message
      "(@[<v 0>(types . %a)@ (errors . %a)@])"
      Michelson_v1_emacs.print_type_map (program, type_map)
      Michelson_v1_emacs.report_errors (program, errs) >>= fun () ->
    return ()
  else
    match res with
    | Ok type_map ->
        let program = Michelson_v1_printer.inject_types type_map program in
        cctxt#message "Well typed" >>= fun () ->
        if show_types then
          cctxt#message "%a" Micheline_printer.print_expr program >>= fun () ->
          return ()
        else return ()
    | Error errs ->
        cctxt#warning "%a"
          (Michelson_v1_error_reporter.report_errors
             ~details: show_types
             ~show_source:print_source_on_error
             ~parsed:program) errs >>= fun () ->
        cctxt#error "ill-typed program"
