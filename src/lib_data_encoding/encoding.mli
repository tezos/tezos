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

(** This is for use *within* the data encoding library only. Instead, you should
    use the corresponding module intended for use: {!Data_encoding.Encoding}. *)

module Kind: sig
  type t = [ `Fixed of int | `Dynamic | `Variable ]
  type length = [ `Fixed of int | `Variable ]
  type enum = [ `Dynamic | `Variable ]
  val combine: string -> t -> t -> t
  val merge : t -> t -> t
  val merge_list: Binary_size.tag_size -> t list -> t
end

type case_tag = Tag of int | Json_only

type 'a desc =
  | Null : unit desc
  | Empty : unit desc
  | Ignore : unit desc
  | Constant : string -> unit desc
  | Bool : bool desc
  | Int8 : int desc
  | Uint8 : int desc
  | Int16 : int desc
  | Uint16 : int desc
  | Int31 : int desc
  | Int32 : Int32.t desc
  | Int64 : Int64.t desc
  | N : Z.t desc
  | Z : Z.t desc
  | RangedInt : { minimum : int ; maximum : int } -> int desc
  | RangedFloat : { minimum : float ; maximum : float } -> float desc
  | Float : float desc
  | Bytes : Kind.length -> MBytes.t desc
  | String : Kind.length -> string desc
  | Padded : 'a t * int -> 'a desc
  | String_enum : ('a, string * int) Hashtbl.t * 'a array -> 'a desc
  | Array : int option * 'a t -> 'a array desc
  | List : int option * 'a t -> 'a list desc
  | Obj : 'a field -> 'a desc
  | Objs : { kind: Kind.t ; left: 'a t ; right: 'b t } -> ('a * 'b) desc
  | Tup : 'a t -> 'a desc
  | Tups : { kind: Kind.t ; left: 'a t ; right: 'b t } -> ('a * 'b) desc
  | Union :
      { kind: Kind.t ;
        tag_size: Binary_size.tag_size ;
        cases: 'a case list ;
      } -> 'a desc
  | Mu :
      { kind: Kind.enum ;
        name: string ;
        title: string option ;
        description: string option ;
        fix: 'a t -> 'a t ;
      } -> 'a desc
  | Conv :
      { proj : ('a -> 'b) ;
        inj : ('b -> 'a) ;
        encoding : 'b t ;
        schema : Json_schema.schema option ;
      } -> 'a desc
  | Describe :
      { id : string ;
        title : string option ;
        description : string option ;
        encoding : 'a t ;
      } -> 'a desc
  | Splitted :
      { encoding : 'a t ;
        json_encoding : 'a Json_encoding.encoding ;
        is_obj : bool ;
        is_tup : bool ;
      } -> 'a desc
  | Dynamic_size :
      { kind : Binary_size.unsigned_integer ;
        encoding : 'a t ;
      } -> 'a desc
  | Check_size : { limit : int ; encoding : 'a t } -> 'a desc
  | Delayed : (unit -> 'a t) -> 'a desc

and _ field =
  | Req : { name: string ;
            encoding: 'a t ;
            title: string option ;
            description: string option ;
          } -> 'a field
  | Opt : { name: string ;
            kind: Kind.enum ;
            encoding: 'a t ;
            title: string option ;
            description: string option ;
          } -> 'a option field
  | Dft : { name: string ;
            encoding: 'a t ;
            default: 'a ;
            title: string option ;
            description: string option ;
          } -> 'a field

and 'a case =
  | Case : { title : string ;
             description : string option ;
             encoding : 'a t ;
             proj : ('t -> 'a option) ;
             inj : ('a -> 't) ;
             tag : case_tag ;
           } -> 't case

and 'a t = {
  encoding: 'a desc ;
  mutable json_encoding: 'a Json_encoding.encoding option ;
}
type 'a encoding = 'a t

val make: ?json_encoding: 'a Json_encoding.encoding -> 'a desc -> 'a t

val null : unit encoding
val empty : unit encoding
val unit : unit encoding
val constant : string -> unit encoding
val int8 : int encoding
val uint8 : int encoding
val int16 : int encoding
val uint16 : int encoding
val int31 : int encoding
val int32 : int32 encoding
val int64 : int64 encoding
val n : Z.t encoding
val z : Z.t encoding
val ranged_int : int -> int -> int encoding
val ranged_float : float -> float -> float encoding
val bool : bool encoding
val string : string encoding
val bytes : MBytes.t encoding
val float : float encoding
val option : 'a encoding -> 'a option encoding
val result : 'a encoding -> 'b encoding -> ('a, 'b) result encoding
val string_enum : (string * 'a) list -> 'a encoding
val is_obj : 'a encoding -> bool
val is_tup : 'a encoding -> bool
module Fixed : sig
  val string : int -> string encoding
  val bytes : int -> MBytes.t encoding
  val add_padding : 'a encoding -> int -> 'a encoding
end
module Variable : sig
  val string : string encoding
  val bytes : MBytes.t encoding
  val array : ?max_length:int -> 'a encoding -> 'a array encoding
  val list : ?max_length:int -> 'a encoding -> 'a list encoding
end
val dynamic_size :
  ?kind:Binary_size.unsigned_integer -> 'a encoding -> 'a encoding
val check_size : int -> 'a encoding -> 'a encoding
val delayed : (unit -> 'a encoding) -> 'a encoding
val req :
  ?title:string -> ?description:string ->
  string -> 't encoding -> 't field
val opt :
  ?title:string -> ?description:string ->
  string -> 't encoding -> 't option field
val varopt :
  ?title:string -> ?description:string ->
  string -> 't encoding -> 't option field
val dft :
  ?title:string -> ?description:string ->
  string -> 't encoding -> 't -> 't field

val obj1 :
  'f1 field -> 'f1 encoding
val obj2 :
  'f1 field -> 'f2 field -> ('f1 * 'f2) encoding
val obj3 :
  'f1 field -> 'f2 field -> 'f3 field -> ('f1 * 'f2 * 'f3) encoding
val obj4 :
  'f1 field -> 'f2 field -> 'f3 field -> 'f4 field ->
  ('f1 * 'f2 * 'f3 * 'f4) encoding
val obj5 :
  'f1 field -> 'f2 field -> 'f3 field -> 'f4 field -> 'f5 field ->
  ('f1 * 'f2 * 'f3 * 'f4 * 'f5) encoding
val obj6 :
  'f1 field -> 'f2 field -> 'f3 field -> 'f4 field -> 'f5 field ->
  'f6 field ->
  ('f1 * 'f2 * 'f3 * 'f4 * 'f5 * 'f6) encoding
val obj7 :
  'f1 field -> 'f2 field -> 'f3 field -> 'f4 field -> 'f5 field ->
  'f6 field -> 'f7 field ->
  ('f1 * 'f2 * 'f3 * 'f4 * 'f5 * 'f6 * 'f7) encoding
val obj8 :
  'f1 field -> 'f2 field -> 'f3 field -> 'f4 field -> 'f5 field ->
  'f6 field -> 'f7 field -> 'f8 field ->
  ('f1 * 'f2 * 'f3 * 'f4 * 'f5 * 'f6 * 'f7 * 'f8) encoding
val obj9 :
  'f1 field -> 'f2 field -> 'f3 field -> 'f4 field -> 'f5 field ->
  'f6 field -> 'f7 field -> 'f8 field -> 'f9 field ->
  ('f1 * 'f2 * 'f3 * 'f4 * 'f5 * 'f6 * 'f7 * 'f8 * 'f9) encoding
val obj10 :
  'f1 field -> 'f2 field -> 'f3 field -> 'f4 field -> 'f5 field ->
  'f6 field -> 'f7 field -> 'f8 field -> 'f9 field -> 'f10 field ->
  ('f1 * 'f2 * 'f3 * 'f4 * 'f5 * 'f6 * 'f7 * 'f8 * 'f9 * 'f10) encoding

val tup1 :
  'f1 encoding ->
  'f1 encoding
val tup2 :
  'f1 encoding -> 'f2 encoding ->
  ('f1 * 'f2) encoding
val tup3 :
  'f1 encoding -> 'f2 encoding -> 'f3 encoding ->
  ('f1 * 'f2 * 'f3) encoding
val tup4 :
  'f1 encoding -> 'f2 encoding -> 'f3 encoding -> 'f4 encoding ->
  ('f1 * 'f2 * 'f3 * 'f4) encoding
val tup5 :
  'f1 encoding -> 'f2 encoding -> 'f3 encoding -> 'f4 encoding ->
  'f5 encoding ->
  ('f1 * 'f2 * 'f3 * 'f4 * 'f5) encoding
val tup6 :
  'f1 encoding -> 'f2 encoding -> 'f3 encoding -> 'f4 encoding ->
  'f5 encoding -> 'f6 encoding ->
  ('f1 * 'f2 * 'f3 * 'f4 * 'f5 * 'f6) encoding
val tup7 :
  'f1 encoding -> 'f2 encoding -> 'f3 encoding -> 'f4 encoding ->
  'f5 encoding -> 'f6 encoding -> 'f7 encoding ->
  ('f1 * 'f2 * 'f3 * 'f4 * 'f5 * 'f6 * 'f7) encoding
val tup8 :
  'f1 encoding -> 'f2 encoding -> 'f3 encoding -> 'f4 encoding ->
  'f5 encoding -> 'f6 encoding -> 'f7 encoding -> 'f8 encoding ->
  ('f1 * 'f2 * 'f3 * 'f4 * 'f5 * 'f6 * 'f7 * 'f8) encoding
val tup9 :
  'f1 encoding -> 'f2 encoding -> 'f3 encoding -> 'f4 encoding ->
  'f5 encoding -> 'f6 encoding -> 'f7 encoding -> 'f8 encoding ->
  'f9 encoding ->
  ('f1 * 'f2 * 'f3 * 'f4 * 'f5 * 'f6 * 'f7 * 'f8 * 'f9) encoding
val tup10 :
  'f1 encoding -> 'f2 encoding -> 'f3 encoding -> 'f4 encoding ->
  'f5 encoding -> 'f6 encoding -> 'f7 encoding -> 'f8 encoding ->
  'f9 encoding -> 'f10 encoding ->
  ('f1 * 'f2 * 'f3 * 'f4 * 'f5 * 'f6 * 'f7 * 'f8 * 'f9 * 'f10) encoding

val merge_objs : 'o1 encoding -> 'o2 encoding -> ('o1 * 'o2) encoding
val merge_tups : 'a1 encoding -> 'a2 encoding -> ('a1 * 'a2) encoding
val array : ?max_length:int -> 'a encoding -> 'a array encoding
val list : ?max_length:int -> 'a encoding -> 'a list encoding

val case :
  title:string ->
  ?description: string ->
  case_tag ->
  'a encoding -> ('t -> 'a option) -> ('a -> 't) -> 't case
val union :
  ?tag_size:[ `Uint8 | `Uint16 ] -> 't case list -> 't encoding

val def :
  string ->
  ?title:string -> ?description:string ->
  'a encoding -> 'a encoding

val conv :
  ('a -> 'b) -> ('b -> 'a) ->
  ?schema:Json_schema.schema ->
  'b encoding -> 'a encoding
val mu :
  string ->
  ?title:string ->
  ?description: string ->
  ('a encoding -> 'a encoding) -> 'a encoding

val classify : 'a encoding -> [ `Fixed of int | `Dynamic | `Variable ]
val classify_desc : 'a desc -> [ `Fixed of int | `Dynamic | `Variable ]
val raw_splitted : json:'a Json_encoding.encoding -> binary:'a encoding -> 'a encoding
