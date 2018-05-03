(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(** Type-safe serialization and deserialization of data structures. *)

(** {1 Data Encoding} *)

(** This module provides type-safe serialization and deserialization of
    data structures. Backends are provided to both binary and JSON.

    This works by writing type descriptors by hand, using the provided
    combinators. These combinators can fine-tune the binary
    representation to be compact and efficient, but also provide
    proper field names and meta information, so the API of Tezos can
    be automatically introspected and documented.

    Here is an example encoding for type [(int * string)].

    [let enc = obj2 (req "code" uint16) (req "message" string)]

    In JSON, this encoding maps values of type [int * string] to JSON
    objects with a field [code] whose value is a number and a field
    [message] whose value is a string.

    In binary, this encoding maps to two raw bytes for the [int]
    followed by the size of the string in bytes, and finally the raw
    contents of the string. This binary format is mostly tagless,
    meaning that serialized data cannot be interpreted without the
    encoding that was used for serialization.

    Regarding binary serialization, encodings are classified as either:
    - fixed size (booleans, integers, numbers)
      data is always the same size for that type ;
    - dynamically sized (arbitrary strings and bytes)
      data is of unknown size and requires an explicit length field ;
    - variable size (special case of strings, bytes, and arrays)
      data makes up the remainder of an object of known size,
      thus its size is given by the context, and does not
      have to be serialized.

    JSON operations are delegated to [ocplib-json-typed]. *)

(* TODO: reorder all the functions so it makes sense (ground, combinator,
 * predicates, etc.) *)
(* TODO: move the doc into the packing module *)

module Kind: sig

  type t = [ `Fixed of int | `Dynamic | `Variable ]

  type length = [ `Fixed of int | `Variable ]

  type enum = [ `Dynamic | `Variable ]

  val combine: string -> t -> t -> t

  val merge : t -> t -> t

  val merge_list: Size.tag_size -> t list -> t

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
  | RangedInt : { minimum : int ; maximum : int } -> int desc
  | RangedFloat : { minimum : float ; maximum : float } -> float desc
  | Float : float desc
  | Bytes : Kind.length -> MBytes.t desc
  | String : Kind.length -> string desc
  | String_enum : ('a, string * int) Hashtbl.t * 'a array -> 'a desc
  | Array : 'a t -> 'a array desc
  | List : 'a t -> 'a list desc
  | Obj : 'a field -> 'a desc
  | Objs : Kind.t * 'a t * 'b t -> ('a * 'b) desc
  | Tup : 'a t -> 'a desc
  | Tups : Kind.t * 'a t * 'b t -> ('a * 'b) desc
  | Union : Kind.t * Size.tag_size * 'a case list -> 'a desc
  | Mu : Kind.enum * string * ('a t -> 'a t) -> 'a desc
  | Conv :
      { proj : ('a -> 'b) ;
        inj : ('b -> 'a) ;
        encoding : 'b t ;
        schema : Json_schema.schema option } -> 'a desc
  | Describe :
      { title : string option ;
        description : string option ;
        encoding : 'a t } -> 'a desc
  | Def : { name : string ;
            encoding : 'a t } -> 'a desc
  | Splitted :
      { encoding : 'a t ;
        json_encoding : 'a Json_encoding.encoding ;
        is_obj : bool ; is_tup : bool } -> 'a desc
  | Dynamic_size : 'a t -> 'a desc
  | Delayed : (unit -> 'a t) -> 'a desc

and _ field =
  | Req : string * 'a t -> 'a field
  | Opt : Kind.enum * string * 'a t -> 'a option field
  | Dft : string * 'a t * 'a -> 'a field

and 'a case =
  | Case : { name : string option ;
             encoding : 'a t ;
             proj : ('t -> 'a option) ;
             inj : ('a -> 't) ;
             tag : case_tag } -> 't case

and 'a t = {
  encoding: 'a desc ;
  mutable json_encoding: 'a Json_encoding.encoding option ;
}
type 'a encoding = 'a t

val make: ?json_encoding: 'a Json_encoding.encoding -> 'a desc -> 'a t


exception No_case_matched
exception Unexpected_tag of int
exception Duplicated_tag of int
exception Invalid_tag of int * [ `Uint8 | `Uint16 ]
exception Unexpected_enum of string * string list
exception Parse_error of string
exception Float_out_of_range of float * float * float
exception Int_out_of_range of int * int * int
exception Invalid_size of int


(** Special value [null] in JSON, nothing in binary. *)
val null : unit encoding

(** Empty object (not included in binary, encoded as empty object in JSON). *)
val empty : unit encoding

(** Unit value, ommitted in binary.
    Serialized as an empty object in JSON, accepts any object when deserializing. *)
val unit : unit encoding

(** Constant string (data is not included in the binary data). *)
val constant : string -> unit encoding

(** Signed 8 bit integer
    (data is encoded as a byte in binary and an integer in JSON). *)
val int8 : int encoding

(** Unsigned 8 bit integer
    (data is encoded as a byte in binary and an integer in JSON). *)
val uint8 : int encoding

(** Signed 16 bit integer
    (data is encoded as a short in binary and an integer in JSON). *)
val int16 : int encoding

(** Unsigned 16 bit integer
    (data is encoded as a short in binary and an integer in JSON). *)
val uint16 : int encoding

(** Signed 31 bit integer, which corresponds to type int on 32-bit OCaml systems
    (data is encoded as a 32 bit int in binary and an integer in JSON). *)
val int31 : int encoding

(** Signed 32 bit integer
    (data is encoded as a 32-bit int in binary and an integer in JSON). *)
val int32 : int32 encoding

(** Signed 64 bit integer
    (data is encodedas a 64-bit int in binary and a decimal string in JSON). *)
val int64 : int64 encoding

(** Integer with bounds in a given range. Both bounds are inclusive.

    Raises [Invalid_argument] if the bounds are beyond the interval
    [-2^30; 2^30-1]. These bounds are chosen to be compatible with all versions
    of OCaml.
*)
val ranged_int : int -> int -> int encoding

(** Float with bounds in a given range. Both bounds are inclusive *)
val ranged_float : float -> float -> float encoding

(** Encoding of a boolean
    (data is encoded as a byte in binary and a boolean in JSON). *)
val bool : bool encoding

(** Encoding of a string
    - default variable in width
    - encoded as a byte sequence in binary
    - encoded as a string in JSON. *)
val string : string encoding

(** Encoding of arbitrary bytes
    (encoded via hex in JSON and directly as a sequence byte in binary). *)
val bytes : MBytes.t encoding

(** Encoding of floating point number
    (encoded as a floating point number in JSON and a double in binary). *)
val float : float encoding

(** Combinator to make an optional value
    (represented as a 1-byte tag followed by the data (or nothing) in binary
     and either the raw value or an empty object in JSON). *)
val option : 'a encoding -> 'a option encoding

(** Combinator to make a {!result} value
    (represented as a 1-byte tag followed by the data of either type in binary,
     and either unwrapped value in JSON (the caller must ensure that both
     encodings do not collide)). *)
val result : 'a encoding -> 'b encoding -> ('a, 'b) result encoding

(** Encode enumeration via association list
    (represented as a string in JSON and binary). *)
val string_enum : (string * 'a) list -> 'a encoding


(** Is the given encoding serialized as a JSON object? *)
val is_obj : 'a encoding -> bool

(** Does the given encoding encode a tuple? *)
val is_tup : 'a encoding -> bool

(** Create encodings that produce data of a fixed length when binary encoded.
    See the preamble for an explanation. *)
module Fixed : sig

  (** Encode a fixed length string *)
  val string : int -> string encoding

  (** Encode a fixed length byte sequence *)
  val bytes : int -> MBytes.t encoding
end

(** Create encodings that produce data of a variable length when binary encoded.
    See the preamble for an explanation. *)
module Variable : sig
  (** Encode a string *)
  val string : string encoding

  (** Encode a byte sequence *)
  val bytes : MBytes.t encoding

  (** Array encoding combinator *)
  val array : 'a encoding -> 'a array encoding

  (** List encoding combinator *)
  val list : 'a encoding -> 'a list encoding
end

(** Mark an encoding as being of dynamic size.
    Forces the size to be stored alongside content when needed.
    Usually used to fix errors from combining two encodings. *)
val dynamic_size : 'a encoding -> 'a encoding

(** Recompute the encoding definition each time it is used.
    Useful for dynamically updating the encoding of values of an extensible
    type via a global reference (e.g. exceptions). *)
val delayed : (unit -> 'a encoding) -> 'a encoding

(** Required field. *)
val req :
  ?title:string -> ?description:string ->
  string -> 't encoding -> 't field

(** Optional field. Omitted entirely in JSON encoding if None.
    Omitted in binary if the only optional field in a [`Variable]
    encoding, otherwise a 1-byte prefix (`0` or `1`) tells if the
    field is present or not. *)
val opt :
  ?title:string -> ?description:string ->
  string -> 't encoding -> 't option field

(** Optional field of variable length.
    Only one can be present in a given object. *)
val varopt :
  ?title:string -> ?description:string ->
  string -> 't encoding -> 't option field

(** Required field with a default value.
    If the default value is passed, the field is omitted in JSON.
    The value is always serialized in binary. *)
val dft :
  ?title:string -> ?description:string ->
  string -> 't encoding -> 't -> 't field

(** {2 Constructors for objects with N fields} *)

(** These are serialized to binary by converting each internal object to binary
    and placing them in the order of the original object.
    These are serialized to JSON as a JSON object with the field names. *)

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

(** {2 Constructors for tuples with N fields} *)

(** These are serialized to binary by converting each internal object to binary
    and placing them in the order of the original object.
    These are serialized to JSON as JSON arrays/lists. *)

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

(** {2 Combinators} *)

(** Create a larger object from the encodings of two smaller ones.
    @raise invalid_arg if both arguments are not objects. *)
val merge_objs : 'o1 encoding -> 'o2 encoding -> ('o1 * 'o2) encoding

(** Create a large tuple encoding from two smaller ones.
    @raise invalid_arg if both values are not tuples. *)
val merge_tups : 'a1 encoding -> 'a2 encoding -> ('a1 * 'a2) encoding

(** Array combinator. *)
val array : 'a encoding -> 'a array encoding

(** List combinator. *)
val list : 'a encoding -> 'a list encoding

(** Encodes a variant constructor. Takes the encoding for the specific
    parameters, a recognizer function that will extract the parameters
    in case the expected case of the variant is being serialized, and
    a constructor function for deserialization.

    The tag must be less than the tag size of the union in which you use the case.
    An optional tag gives a name to a case and should be used to maintain
    compatibility.

    An optional name for the case can be provided,
    which is used in the binary documentation. *)
val case :
  ?name:string ->
  case_tag ->
  'a encoding -> ('t -> 'a option) -> ('a -> 't) -> 't case

(** Create a single encoding from a series of cases.

    In JSON, all cases are tried one after the other. The caller must
    check for collisions.

    In binary, a prefix tag is added to discriminate quickly between
    cases. The default is `Uint8 and you must use a `Uint16 if you are
    going to have more than 256 cases.

    This function will raise an exception if it is given the empty list
    or if there are more cases than can fit in the tag size. *)
val union :
  ?tag_size:[ `Uint8 | `Uint16 ] -> 't case list -> 't encoding

(** Add documentation to an encoding. *)
val describe :
  ?title:string -> ?description:string ->
  't encoding ->'t encoding

(** Give a name to an encoding. *)
val def : string -> 'a encoding -> 'a encoding

(** Provide a transformer from one encoding to a different one.

    Used to simplify nested encodings or to change the generic tuples
    built by {obj1}, {tup1} and the like into proper records.

    A schema may optionally be provided as documentation of the new encoding. *)
val conv :
  ('a -> 'b) -> ('b -> 'a) ->
  ?schema:Json_schema.schema ->
  'b encoding -> 'a encoding

(** Combinator for recursive encodings. *)
val mu : string -> ('a encoding -> 'a encoding) -> 'a encoding

(** Classify an encoding wrt. its binary serialization as explained in the preamble. *)
val classify : 'a encoding -> [ `Fixed of int | `Dynamic | `Variable ]

(** Define different encodings for JSON and binary serialization. *)
val raw_splitted : json:'a Json_encoding.encoding -> binary:'a encoding -> 'a encoding

