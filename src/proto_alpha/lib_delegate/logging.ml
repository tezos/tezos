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

let timestamp_tag = Tag.def ~doc:"Timestamp when event occurred" "timestamp" Time.System.pp_hum
let valid_ops = Tag.def ~doc:"Valid Operations" "valid_ops" Format.pp_print_int
let op_count = Tag.def ~doc:"Number of operations" "op_count" Format.pp_print_int
let refused_ops = Tag.def ~doc:"Refused Operations" "refused_ops" Format.pp_print_int
let bake_priority_tag = Tag.def ~doc:"Baking priority" "bake_priority" Format.pp_print_int
let fitness_tag = Tag.def ~doc:"Fitness" "fitness" Fitness.pp
let current_slots_tag = Tag.def ~doc:"Number of baking slots that can be baked at this time" "current_slots" Format.pp_print_int
let future_slots_tag = Tag.def ~doc:"Number of baking slots in the foreseeable future but not yet bakeable" "future_slots" Format.pp_print_int
let timespan_tag = Tag.def ~doc:"Timespan in seconds" "timespan" Ptime.Span.pp
let filename_tag = Tag.def ~doc:"Filename" "filename" Format.pp_print_text

let signed_header_tag = Tag.def ~doc:"Signed header" "signed_header" MBytes.pp_hex
let signed_operation_tag = Tag.def ~doc:"Signed operation" "signed_operation" MBytes.pp_hex
let operations_tag = Tag.def ~doc:"Block Operations" "operations"
    (Format.pp_print_list
       ~pp_sep:(fun ppf () -> Format.fprintf ppf "+")
       (fun ppf operations -> Format.fprintf ppf "%d" (List.length operations)))

let raw_operations_tag = Tag.def ~doc:"Raw operations" "raw_operations"
    (fun fmt raw_ops ->
       let pp_op fmt op =
         let json = Data_encoding.Json.construct Operation.raw_encoding op in
         Format.fprintf fmt "%a" Data_encoding.Json.pp json
       in
       Format.fprintf fmt "@[<v>%a@]" (Format.pp_print_list ~pp_sep:Format.pp_print_cut pp_op) raw_ops)

let bake_op_count_tag = Tag.def ~doc:"Bake Operation Count" "operation_count" Format.pp_print_int

let endorsement_slot_tag = Tag.def ~doc:"Endorsement Slot" "endorsement_slot" Format.pp_print_int
let endorsement_slots_tag = Tag.def ~doc:"Endorsement Slots" "endorsement_slots" Format.(fun ppf v -> pp_print_int ppf (List.length v))
let denounced_endorsements_slots_tag = Tag.def ~doc:"Endorsement Slots" "denounced_endorsement_slots" Format.(pp_print_list pp_print_int)
let denouncement_source_tag = Tag.def ~doc:"Denounce Source" "source" Format.pp_print_text

let level_tag = Tag.def ~doc:"Level" "level" Raw_level.pp
let nonce_tag = Tag.def ~doc:"Nonce" "nonce" Data_encoding.Json.(fun ppf nonce -> pp ppf (construct Nonce.encoding nonce))
let chain_tag = Tag.def ~doc:"Chain selector" "chain" Format.(fun ppf chain -> pp_print_string ppf @@ Block_services.chain_to_string chain)
let block_tag = Tag.def ~doc:"Block selector" "block" Format.(fun ppf block -> pp_print_string ppf @@ Block_services.to_string block)

let worker_tag = Tag.def ~doc:"Worker in which event occurred" "worker" Format.pp_print_text

let block_header_tag = Tag.def ~doc:"Raw block header" "block_header" (fun ppf _ -> Format.fprintf ppf "[raw block header]")

let conflicting_endorsements_tag = Tag.def ~doc:"Two conflicting endorsements signed by the same key" "conflicting_endorsements" Format.(
    fun ppf (a,b) -> fprintf ppf "%a / %a" Operation_hash.pp (Operation.hash a) Operation_hash.pp (Operation.hash b))
