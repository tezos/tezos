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

type ('prefix, 'params) t
type ('prefix, 'params) path = ('prefix, 'params) t
type 'prefix context = ('prefix, 'prefix) path

val root: unit context
val open_root: 'a context

val add_suffix:
  ('prefix, 'params) path -> string -> ('prefix, 'params) path
val (/):
  ('prefix, 'params) path -> string -> ('prefix, 'params) path

val add_arg:
  ('prefix, 'params) path -> 'a RPC_arg.t -> ('prefix, 'params * 'a) path
val (/:):
  ('prefix, 'params) path -> 'a RPC_arg.t -> ('prefix, 'params * 'a) path

val add_final_args:
  ('prefix, 'params) path -> 'a RPC_arg.t -> ('prefix, 'params * 'a list) path
val (/:*):
  ('prefix, 'params) path -> 'a RPC_arg.t -> ('prefix, 'params * 'a list) path
