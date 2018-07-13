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

type 'a t
type 'a query = 'a t

val empty: unit query

type ('a, 'b) field
val field:
  ?descr: string ->
  string -> 'a RPC_arg.t -> 'a -> ('b -> 'a) -> ('b, 'a) field
val opt_field:
  ?descr: string ->
  string -> 'a RPC_arg.t -> ('b -> 'a option) -> ('b, 'a option) field
val flag:
  ?descr: string ->
  string -> ('b -> bool) -> ('b, bool) field
val multi_field:
  ?descr: string ->
  string -> 'a RPC_arg.t -> ('b -> 'a list) -> ('b, 'a list) field

type ('a, 'b, 'c) open_query
val query: 'b -> ('a, 'b, 'b) open_query
val (|+):
  ('a, 'b, 'c -> 'd) open_query ->
  ('a, 'c) field -> ('a, 'b, 'd) open_query
val seal: ('a, 'b, 'a) open_query -> 'a t

type untyped = (string * string) list
exception Invalid of string
val parse: 'a query -> untyped -> 'a
