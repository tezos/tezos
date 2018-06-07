(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Proto_alpha
open Alpha_context
open Tezos_micheline

open Michelson_v1_printer

module Program = Client_aliases.Alias (struct
    type t = Michelson_v1_parser.parsed Micheline_parser.parsing_result
    let encoding =
      Data_encoding.conv
        (fun ({ Michelson_v1_parser.source }, _) -> source)
        (fun source -> Michelson_v1_parser.parse_toplevel source)
        Data_encoding.string
    let of_source source =
      return (Michelson_v1_parser.parse_toplevel source)
    let to_source ({ Michelson_v1_parser.source }, _) = return source
    let name = "program"
  end)

let print_errors (cctxt : #Client_context.printer) errs ~show_source ~parsed =
  cctxt#warning "%a"
    (Michelson_v1_error_reporter.report_errors
       ~details:false
       ~show_source
       ~parsed) errs >>= fun () ->
  cctxt#error "error running program" >>= fun () ->
  return ()

let print_big_map_diff ppf = function
  | None -> ()
  | Some diff ->
      Format.fprintf ppf
        "@[<v 2>map diff:@,%a@]@,"
        (Format.pp_print_list
           ~pp_sep:Format.pp_print_space
           (fun ppf (key, value) ->
              Format.fprintf ppf "%s %s%a"
                (match value with
                 | None -> "-"
                 | Some _ -> "+")
                key
                (fun ppf -> function
                   | None -> ()
                   | Some x -> Format.fprintf ppf "-> %a" print_expr x)
                value))
        diff

let print_run_result (cctxt : #Client_context.printer) ~show_source ~parsed = function
  | Ok (storage, operations, maybe_diff) ->
      cctxt#message "@[<v 0>@[<v 2>storage@,%a@]@,@[<v 2>emitted operations@,%a@]@,@[%a@]@]@."
        print_expr storage
        (Format.pp_print_list Operation_result.pp_internal_operation) operations
        print_big_map_diff maybe_diff >>= fun () ->
      return ()
  | Error errs ->
      print_errors cctxt errs ~show_source ~parsed

let print_trace_result (cctxt : #Client_context.printer) ~show_source ~parsed =
  function
  | Ok (storage, operations, trace, maybe_big_map_diff) ->
      cctxt#message
        "@[<v 0>@[<v 2>storage@,%a@]@,\
         @[<v 2>emitted operations@,%a@]@,%a@[<v 2>@[<v 2>trace@,%a@]@]@."
        print_expr storage
        (Format.pp_print_list Operation_result.pp_internal_operation) operations
        print_big_map_diff maybe_big_map_diff
        (Format.pp_print_list
           (fun ppf (loc, gas, stack) ->
              Format.fprintf ppf
                "- @[<v 0>location: %d (remaining gas: %a)@,\
                 [ @[<v 0>%a ]@]@]"
                loc Gas.pp gas
                (Format.pp_print_list print_expr)
                stack))
        trace >>= fun () ->
      return ()
  | Error errs ->
      print_errors cctxt errs ~show_source ~parsed

let get_contract cctxt ?(chain = `Main) block contract =
  match contract with
  | Some contract -> return contract
  | None ->
      (* TODO use local contract by default *)
      Alpha_services.Contract.list cctxt (chain, block) >>|? List.hd

let run
    (cctxt : #Proto_alpha.rpc_context)
    ?(chain = `Main)
    block
    ?contract
    ?(amount = Tez.fifty_cents)
    ~(program : Michelson_v1_parser.parsed)
    ~(storage : Michelson_v1_parser.parsed)
    ~(input : Michelson_v1_parser.parsed)
    () =
  get_contract cctxt ~chain block contract >>=? fun contract ->
  Alpha_services.Helpers.Scripts.run_code cctxt
    (chain, block)
    program.expanded (storage.expanded, input.expanded, amount, contract)

let trace
    (cctxt : #Proto_alpha.rpc_context)
    ?(chain = `Main)
    block
    ?contract
    ?(amount = Tez.fifty_cents)
    ~(program : Michelson_v1_parser.parsed)
    ~(storage : Michelson_v1_parser.parsed)
    ~(input : Michelson_v1_parser.parsed)
    () =
  get_contract cctxt ~chain block contract >>=? fun contract ->
  Alpha_services.Helpers.Scripts.trace_code cctxt
    (chain, block)
    program.expanded (storage.expanded, input.expanded, amount, contract)

let hash_and_sign
    cctxt
    ?(chain = `Main)
    block
    ?gas
    (data : Michelson_v1_parser.parsed)
    (typ : Michelson_v1_parser.parsed)
    sk =
  Alpha_services.Helpers.Scripts.hash_data
    cctxt (chain, block) (data.expanded, typ.expanded, gas) >>=? fun (hash, gas) ->
  Client_keys.sign sk (MBytes.of_string hash) >>=? fun signature ->
  return (hash, Signature.to_b58check signature, gas)

let typecheck_data
    cctxt
    ?(chain = `Main)
    block
    ?gas
    ~(data : Michelson_v1_parser.parsed)
    ~(ty : Michelson_v1_parser.parsed)
    () =
  Alpha_services.Helpers.Scripts.typecheck_data
    cctxt (chain, block)
    (data.expanded, ty.expanded, gas)

let typecheck_program
    cctxt
    ?(chain = `Main)
    block
    ?gas
    (program : Michelson_v1_parser.parsed) =
  Alpha_services.Helpers.Scripts.typecheck_code cctxt (chain, block) (program.expanded, gas)

let print_typecheck_result
    ~emacs ~show_types ~print_source_on_error
    program res (cctxt : #Client_context.printer) =
  if emacs then
    let type_map, errs, _gas = match res with
      | Ok (type_map, gas) -> (type_map, [], Some gas)
      | Error (Alpha_environment.Ecoproto_error
                 (Script_tc_errors.Ill_typed_contract (_, type_map ))
               :: _ as errs) ->
          (type_map, errs, None)
      | Error errs ->
          ([], errs, None) in
    cctxt#message
      "(@[<v 0>(types . %a)@ (errors . %a)@])"
      Michelson_v1_emacs.print_type_map (program, type_map)
      Michelson_v1_emacs.report_errors (program, errs) >>= fun () ->
    return ()
  else
    match res with
    | Ok (type_map, gas) ->
        let program = Michelson_v1_printer.inject_types type_map program in
        cctxt#message "@[<v 0>Well typed@,Gas remaining: %a@]"
          Gas.pp gas >>= fun () ->
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
