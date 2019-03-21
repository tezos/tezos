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

open Error_monad

(** Manage a common ["--for-script <FORMAT>"] option to make the
    output of certain commands script-friendly. *)

type output_format
(** A representation of the output format. *)

val clic_arg : unit -> (output_format option, _) Clic.arg
(** Command line argument for {!Clic.command} (and the [Clic.args*]
    functions). Not that this is the only way to obtain a value of type
    [output_format]. On the command line, it appears as [--for-script] with
    values [TSV] or [CSV]. *)

val output :
  ?channel: Lwt_io.output_channel ->
  output_format option ->
  for_human:(unit -> unit tzresult Lwt.t) ->
  for_script:(unit -> string list list) ->
  unit tzresult Lwt.t
(** [output fmt_opt ~for_human ~for_script] behaves in one of two ways.
    If [fmt_opt] is [Some _], then it formats the value returned by
    [for_script ()]. The function's return value is formatted as lines of
    columns of values (list of lists of strings). This is to help scripts to
    decode/interpret/parse the output.
    Otherwise, if [fmt_opt] is [None], it calls [for_human ()] which is
    responsible for the whole formatting.

    The optional argument [channel] is used when automatically formatting the
    value returned by [for_script ()]. It has no effect on [for_human ()]. *)

val output_row :
  ?channel: Lwt_io.output_channel ->
  output_format option ->
  for_human:(unit -> unit tzresult Lwt.t) ->
  for_script:(unit -> string list) ->
  unit tzresult Lwt.t
(** Same as {!output} but for a single row of data. *)

val output_for_human :
  output_format option -> (unit -> unit tzresult Lwt.t) -> unit tzresult Lwt.t
(** [output_for_human fmt_opt for_human] behaves in either of two ways.
    If [fmt_opt] is [None], then it calls [for_human ()].
    Otherwise, it does nothing.

    Use this function to provide output that is of no interest to automatic
    tools. *)
