(** Native browser representation of JSON documents *)

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

(** An abstract type for native browser objects. *)
type value

(** A view over the browser representation.*)
module Repr : Json_repr.Repr with type value = value

(** Pre-instanciated {!Json_encoding.Make}. *)
module Json_encoding : module type of Json_encoding.Make (Repr)

(** Pre-instanciated {!Json_encoding.Make}. *)
module Json_query : module type of Json_query.Make (Repr)

(** Parse a JSON string using the native browser parser. *)
val parse : string -> value

(** Produce a JSON string using the native browser printer.

    If indent is not present, everything is printed on a single line.
    Otherwise, it is the number (up to 10) of spaces inserted at
    beginning of lines for each indentation level. *)
val stringify : ?indent: int -> value -> string

(** Same as {!parse} with native browser strings. *)
val parse_js_string : Js.js_string Js.t -> value

(** Same as {!stringify} with native browser strings. *)
val js_stringify : ?indent: int -> value -> Js.js_string Js.t
