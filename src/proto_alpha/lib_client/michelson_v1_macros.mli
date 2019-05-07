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

open Tezos_micheline

type 'l node = ('l, string) Micheline.node

type error += Unexpected_macro_annotation of string
type error += Sequence_expected of string
type error += Invalid_arity of string * int * int

val expand : 'l node -> 'l node tzresult
val expand_rec : 'l node -> 'l node * error list

val expand_caddadr : 'l node -> 'l node option tzresult
val expand_set_caddadr : 'l node -> 'l node option tzresult
val expand_map_caddadr : 'l node -> 'l node option tzresult
val expand_dxiiivp : 'l node -> 'l node option tzresult
val expand_pappaiir : 'l node -> 'l node option tzresult
val expand_duuuuup : 'l node -> 'l node option tzresult
val expand_compare : 'l node -> 'l node option tzresult
val expand_asserts : 'l node -> 'l node option tzresult
val expand_unpappaiir : 'l node -> 'l node option tzresult
val expand_if_some : 'l node -> 'l node option tzresult
val expand_if_right : 'l node -> 'l node option tzresult

val unexpand : 'l node -> 'l node
val unexpand_rec : 'l node -> 'l node

val unexpand_caddadr : 'l node -> 'l node option
val unexpand_set_caddadr : 'l node -> 'l node option
val unexpand_map_caddadr : 'l node -> 'l node option
val unexpand_dxiiivp : 'l node -> 'l node option
val unexpand_pappaiir : 'l node -> 'l node option
val unexpand_duuuuup : 'l node -> 'l node option
val unexpand_compare : 'l node -> 'l node option
val unexpand_asserts : 'l node -> 'l node option
val unexpand_unpappaiir : 'l node -> 'l node option
val unexpand_if_some : 'l node -> 'l node option
val unexpand_if_right : 'l node -> 'l node option
