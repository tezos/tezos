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

open Michelson_v1_printer

module Program = Client_aliases.Alias (struct
    type t = Michelson_v1_parser.parsed Micheline_parser.parsing_result
    let encoding =
      Data_encoding.conv
        (fun ({ Michelson_v1_parser.source ; _ }, _) -> source)
        (fun source -> Michelson_v1_parser.parse_toplevel source)
        Data_encoding.string
    let of_source source =
      return (Michelson_v1_parser.parse_toplevel source)
    let to_source ({ Michelson_v1_parser.source ; _ }, _) = return source
    let name = "script"
  end)

let print_errors (cctxt : #Client_context.printer) errs ~show_source ~parsed =
  cctxt#warning "%a"
    (Michelson_v1_error_reporter.report_errors
       ~details:false
       ~show_source
       ~parsed) errs >>= fun () ->
  cctxt#error "error running script" >>= fun () ->
  return_unit

let print_big_map_diff ppf = function
  | None -> ()
  | Some diff ->
      Format.fprintf ppf
        "@[<v 2>map diff:@,%a@]@,"
        (Format.pp_print_list
           ~pp_sep:Format.pp_print_space
           (fun ppf Contract.{ diff_key ; diff_value ; _ } ->
              Format.fprintf ppf "%s %a%a"
                (match diff_value with
                 | None -> "-"
                 | Some _ -> "+")
                print_expr diff_key
                (fun ppf -> function
                   | None -> ()
                   | Some x -> Format.fprintf ppf "-> %a" print_expr x)
                diff_value))
        diff

let print_run_result (cctxt : #Client_context.printer) ~show_source ~parsed = function
  | Ok (storage, operations, maybe_diff) ->
      cctxt#message "@[<v 0>@[<v 2>storage@,%a@]@,@[<v 2>emitted operations@,%a@]@,@[%a@]@]@."
        print_expr storage
        (Format.pp_print_list Operation_result.pp_internal_operation) operations
        print_big_map_diff maybe_diff >>= fun () ->
      return_unit
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
        print_execution_trace trace >>= fun () ->
      return_unit
  | Error errs ->
      print_errors cctxt errs ~show_source ~parsed

let run
    (cctxt : #Proto_alpha.rpc_context)
    ~(chain : Chain_services.chain)
    ~block
    ?(amount = Tez.fifty_cents)
    ~(program : Michelson_v1_parser.parsed)
    ~(storage : Michelson_v1_parser.parsed)
    ~(input : Michelson_v1_parser.parsed)
    () =
  Alpha_services.Helpers.Scripts.run_code cctxt
    (chain, block)
    program.expanded (storage.expanded, input.expanded, amount)

let trace
    (cctxt : #Proto_alpha.rpc_context)
    ~(chain : Chain_services.chain)
    ~block
    ?(amount = Tez.fifty_cents)
    ~(program : Michelson_v1_parser.parsed)
    ~(storage : Michelson_v1_parser.parsed)
    ~(input : Michelson_v1_parser.parsed)
    () =
  Alpha_services.Helpers.Scripts.trace_code cctxt
    (chain, block)
    program.expanded (storage.expanded, input.expanded, amount)

let typecheck_data
    cctxt
    ~(chain : Chain_services.chain)
    ~block
    ?gas
    ~(data : Michelson_v1_parser.parsed)
    ~(ty : Michelson_v1_parser.parsed)
    () =
  Alpha_services.Helpers.Scripts.typecheck_data
    cctxt (chain, block)
    (data.expanded, ty.expanded, gas)

let typecheck_program
    cctxt
    ~(chain : Chain_services.chain)
    ~block
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
    return_unit
  else
    match res with
    | Ok (type_map, gas) ->
        let program = Michelson_v1_printer.inject_types type_map program in
        cctxt#message "@[<v 0>Well typed@,Gas remaining: %a@]"
          Gas.pp gas >>= fun () ->
        if show_types then
          cctxt#message "%a" Micheline_printer.print_expr program >>= fun () ->
          return_unit
        else return_unit
    | Error errs ->
        cctxt#warning "%a"
          (Michelson_v1_error_reporter.report_errors
             ~details: show_types
             ~show_source:print_source_on_error
             ~parsed:program) errs >>= fun () ->
        cctxt#error "ill-typed script"
