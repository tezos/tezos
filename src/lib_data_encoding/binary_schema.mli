(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

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
  | Seq of layout (* For arrays and lists *)
  | Ref of string

and fields = field_descr list

and toplevel_encoding =
  | Obj of { fields : fields }
  | Cases of { kind : Encoding.Kind.t ;
               tag_size : Binary_size.tag_size ;
               cases : (int * string option * fields) list }
  | Int_enum of { size : Binary_size.integer ;
                  cases : (int * string) list }

and description =
  { name : string ;
    description : string option }

type t = {
  description: description ;
  toplevel: toplevel_encoding ;
  fields: (description * toplevel_encoding) list ;
}

module Printer : sig
  val pp_layout : Format.formatter -> layout -> unit
end

val pp: Format.formatter -> t -> unit
val encoding: t Encoding.t
