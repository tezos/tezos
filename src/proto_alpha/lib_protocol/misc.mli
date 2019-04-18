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

(** {2 Helper functions} *)

type 'a lazyt = unit -> 'a
type 'a lazy_list_t = LCons of 'a * ('a lazy_list_t tzresult Lwt.t lazyt)
type 'a lazy_list = 'a lazy_list_t tzresult Lwt.t

(** Include bounds *)
val (-->) : int -> int -> int list
val (--->) : Int32.t -> Int32.t -> Int32.t list

val pp_print_paragraph : Format.formatter -> string -> unit

val take: int -> 'a list -> ('a list * 'a list) option

(** Some (input with [prefix] removed), if string has [prefix], else [None] **)
val remove_prefix: prefix:string -> string -> string option

(** [remove nb list] remove the first [nb] elements from the list [list]. *)
val remove_elem_from_list: int -> 'a list -> 'a list
