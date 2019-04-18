(************************************************************************)
(*  ocplib-json-typed                                                   *)
(*                                                                      *)
(*    Copyright 2014 OCamlPro                                           *)
(*                                                                      *)
(*  This file is distributed under the terms of the GNU Lesser General  *)
(*  Public License as published by the Free Software Foundation; either *)
(*  version 2.1 of the License, or (at your option) any later version,  *)
(*  with the OCaml static compilation exception.                        *)
(*                                                                      *)
(*  ocplib-json-typed is distributed in the hope that it will be useful,*)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of      *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the       *)
(*  GNU General Public License for more details.                        *)
(*                                                                      *)
(************************************************************************)

(** Representations of JSON documents *)

(** {2 Abstraction over JSON representations} *)

(** The internal format used by the library. A common format to view
    JSON structures from different representations. It only shows the
    head of structures, hiding the contents of fields, so that the
    conversion from another format or a stream can be done lazily. *)
type 'a view =
  [ `O of (string * 'a) list
    (** An associative table (object). *)
  | `A of 'a list
    (** An (integer indexed) array. *)
  | `Bool of bool
    (** A JS boolean [true] or [false]. *)
  | `Float of float
    (** A floating point number (double precision). *)
  | `String of string
    (** An UTF-8 encoded string. *)
  | `Null
    (** The [null] constant. *) ]

(** Each representation must provide a unique identifier, obtained via
    the {!repr_uid} function. This identifier is used when converting
    between representations, to optimize out a copy when converting
    from a representation to itself. Beware that this optimization
    relies only on this [uid] token. Converting between values of the
    same type using two different representation modules with
    different [uid]s will perform a copy. A practical way to ensure
    that the optimization is made is to write your representations as
    toplevel modules, and not inside functors. *)
type 'a repr_uid

(** See {!type:repr_uid}. *)
val repr_uid : unit -> 'a repr_uid

(** A view over a given implementation. *)
module type Repr = sig

  (** The implementation type. *)
  type value

  (** View a value in the common format. *)
  val view : value -> value view

  (** Builds a value from a view *)
  val repr : value view -> value

  (** See {!type:repr_uid}. *)
  val repr_uid : value repr_uid

end

(** Convert a JSON value from one representation to another. *)
val convert :
  (module Repr with type value = 'tf) ->
  (module Repr with type value = 'tt) ->
  'tf -> 'tt

(** Generic pretty-printer. If [compact] is set (by default), then the
    output is not really pretty (no space is output). Ascii-compatible
    string encoding is expected, as printing only escapes double
    quotes and control characters. Use [pp_string] for more advanced
    escaping. This function does not claim to be the best JSON pretty
    printer, it is mostly a small utility. *)
val pp :
  ?compact: bool -> ?pp_string: (Format.formatter -> string -> unit) ->
  (module Repr with type value = 'tf) ->
  Format.formatter -> 'tf -> unit


(** {2 Third party in-memory JSON document representations} *)

(** A JSON value compatible with {!Ezjsonm.value}. *)
type ezjsonm =
  [ `O of (string * ezjsonm) list
  (** An associative table (object). *)
  | `A of ezjsonm list
  (** An (integer indexed) array. *)
  | `Bool of bool
  (** A JS boolean [true] or [false]. *)
  | `Float of float
  (** A floating point number (double precision). *)
  | `String of string
  (** An UTF-8 encoded string. *)
  | `Null
  (** The [null] constant. *) ]

(** A view over the {!type:ezjsonm} representation.*)
module Ezjsonm : Repr with type value = ezjsonm

(** A JSON value compatible with {!Yojson.Safe.json}. *)
type yojson =
  [ `Bool of bool
  (** A JS boolean [true] of [false]. *)
  | `Assoc of (string * yojson) list
  (** JSON object. *)
  | `Float of float
  (** A floating point number (double precision). *)
  | `Int of int
  (** A number without decimal point or exponent. *)
  | `Intlit of string
  (** A number without decimal point or exponent, preserved as string. *)
  | `List of yojson list
  (** A JS array. *)
  | `Null
  (** The [null] constant. *)
  | `String of string
  (** An UTF-8 encoded string. *)
  | `Tuple of yojson list
  (** A tuple (non-standard). Syntax: ("abc", 123). *)
  | `Variant of string * yojson option
    (** A variant (non-standard). Syntax: <"Foo"> or <"Bar": 123>. *) ]

(** A view over the {!yojson} representation.*)
module Yojson : Repr with type value = yojson

(** {2 Representation-agnostic JSON format} *)

(** A meta-representation for JSON values that can unify values of
    different representations by boxing them with their corresponding
    {!Repr} modules. *)
type any = private Value_with_repr: (module Repr with type value = 'a) * 'a -> any

(** Converts a boxed value from its intrinsic representation to the
    one of the given {!Repr} module. Optimized if the internal
    representation of the value actually is the requested one. *)
val any_to_repr : (module Repr with type value = 'a) -> any -> 'a

(** Boxes a value with a compatible {!Repr} module. *)
val repr_to_any : (module Repr with type value = 'a) -> 'a -> any

(** Pretty-printer for values of type {!any}. See {!pp} for details. *)
val pp_any :
  ?compact: bool -> ?pp_string: (Format.formatter -> string -> unit) -> unit ->
  Format.formatter -> any -> unit

(** {2 Predefined converters for ezjsonm } *)
(** see {!type:ezjsonm} *)

(** Conversion helper. *)
val from_yojson : [< yojson ] -> [> ezjsonm ]

(** Conversion helper. *)
val to_yojson : [< ezjsonm] -> [> yojson ]

(** Converts a boxed value from its representation to {!ezjsonm}. *)
val from_any : any -> [> ezjsonm ]

(** Boxes as {!ezjsonm} value. *)
val to_any : [< ezjsonm] -> any
