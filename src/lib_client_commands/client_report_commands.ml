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

(* Commands used to introspect the node's state *)

let print_invalid_blocks ppf (b: Shell_services.Chain.invalid_block) =
  Format.fprintf ppf
    "@[<v 2>Hash: %a\
     @ Level: %ld\
     @ %a@]"
    Block_hash.pp b.hash
    b.level
    pp_print_error b.errors

let commands () =
  let open Clic in
  let group = { name = "report" ;
                title = "Commands to report the node's status" } in
  let output_arg =
    default_arg
      ~doc:"write to a file"
      ~long:"output"
      ~short:'o'
      ~placeholder:"path"
      ~default: "-"
      (parameter (fun _ -> function
           | "-" -> return Format.std_formatter
           | file ->
               let ppf = Format.formatter_of_out_channel (open_out file) in
               ignore Clic.(setup_formatter ppf Plain Full) ;
               return ppf)) in
  [
    command ~group
      ~desc: "The last heads that have been considered by the node."
      (args1 output_arg)
      (fixed [ "list" ; "heads" ])
      (fun ppf cctxt ->
         Shell_services.Blocks.list cctxt () >>=? fun heads ->
         Format.fprintf ppf "@[<v>%a@]@."
           (Format.pp_print_list Block_hash.pp)
           (List.concat heads) ;
         return_unit) ;
    command ~group ~desc: "The blocks that have been marked invalid by the node."
      (args1 output_arg)
      (fixed [ "list" ; "rejected" ; "blocks" ])
      (fun ppf cctxt ->
         Shell_services.Invalid_blocks.list cctxt () >>=? function
         | [] ->
             Format.fprintf ppf "No invalid blocks.@." ;
             return_unit
         | _ :: _ as invalid ->
             Format.fprintf ppf "@[<v>%a@]@."
               (Format.pp_print_list print_invalid_blocks)
               invalid ;
             return_unit) ;
  ]
