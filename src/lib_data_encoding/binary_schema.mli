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

(** This is for use *within* the data encoding library only. *)

type integer_extended = [ Binary_size.integer | `Int32 | `Int64 ]

type field_descr =
  | Named_field of string * Encoding.Kind.t * layout
  | Anonymous_field of Encoding.Kind.t * layout
  | Dynamic_size_field of string option * int * Binary_size.unsigned_integer
  | Optional_field of string

and layout =
  | Zero_width
  | Int of integer_extended
  | Bool
  | RangedInt of int * int
  | RangedFloat of float * float
  | Float
  | Bytes
  | String
  | Enum of Binary_size.integer * string
  | Seq of layout * int option (* For arrays and lists *)
  | Ref of string
  | Padding

and fields = field_descr list

and toplevel_encoding =
  | Obj of { fields : fields }
  | Cases of { kind : Encoding.Kind.t ;
               tag_size : Binary_size.tag_size ;
               cases : (int * string option * fields) list }
  | Int_enum of { size : Binary_size.integer ;
                  cases : (int * string) list }

and description =
  { title : string ;
    description : string option }

type t = {
  toplevel: toplevel_encoding ;
  fields: (description * toplevel_encoding) list ;
}

val pp: Format.formatter -> t -> unit
val encoding: t Encoding.t
