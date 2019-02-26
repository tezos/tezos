(** Abstract representation of JSON schemas as of version
    [http://json-schema.org/draft-04/schema#]. *)

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

(** {2 Abstract representation of schemas} *) (******************************)

(** A JSON schema root. *)
type schema

(** A node in the schema, embeds all type-agnostic specs. *)
and element =
  { title : string option ;
    (** An optional short description. *)
    description : string option ;
    (** An optional long description. *)
    default : Json_repr.any option ;
    (** A default constant to be substituted in case of a missing value. *)
    enum : Json_repr.any list option ;
    (** A valid value must equal one of these constants. *)
    kind : element_kind ;
    (** The type-specific part. *)
    format : string option ;
    (** predefined formats such as [date-time], [email], [ipv4], [ipv6], [uri]. *)
    id : string option
    (** An optional ID. *) }

(** The type-specific part of schema nodes. *)
and element_kind =
  | Object of object_specs
  (** The type of an object. *)
  | Array of element list * array_specs
  (** An fixed-length array with the types of its elements (a tuple). *)
  | Monomorphic_array of element * array_specs
  (** A variable-length array with the type of its children. *)
  | Combine of combinator * element list
  (** A mix of schemas using logical combinators. *)
  | Def_ref of Json_query.path
  (** A ref to an element from its path in the JSON representation. *)
  | Id_ref of string
  (** A ref to an element from its ID. *)
  | Ext_ref of Uri.t
  (** A ref to an external element. *)
  | String of string_specs
  (** A string (with optional characteristics). *)
  | Integer of numeric_specs
  (** An int (with optional characteristics). *)
  | Number of numeric_specs
  (** A float (with optional characteristics). *)
  | Boolean  (** Any boolean. *)
  | Null (** The null value. *)
  | Any (** Any JSON value. *)
  | Dummy
  (** For building cyclic definitions, a definition bound to a dummy
      will be considered absent for {!add_definition} but present
      for {!update}. The idea is to insert a dummy definition, build a
      cyclic structure using it for recursion, and finally update the
      definition with the structure. *)

(** Grammar combinators. *)
and combinator =
  | Any_of (** Logical OR n-ary combinator. *)
  | One_of (** Logical XOR n-ary combinator. *)
  | All_of (** Logical AND n-ary combinator. *)
  | Not (** Logical NOT unary combinator. *)

(** Parameters of the [Array] and [MonomorphicArray] type specifiers. *)
and array_specs =
  { min_items : int ;
    (** The minimum number of elements. *)
    max_items : int option ;
    (** The maximum number of elements. *)
    unique_items : bool ;
    (** Teels if all elements must be different. *)
    additional_items : element option ;
    (** The type of additional items, if allowed. *) }

(** Parameters of the [Integer] and [Number] type specifiers. *)
and numeric_specs =
  { multiple_of : float option ;
    (** An optional divisor of valid values *)
    minimum : (float * [ `Inclusive | `Exclusive ]) option ;
    (** The optional lower bound of the numeric range *)
    maximum : (float * [ `Inclusive | `Exclusive ]) option
    (** The optional upper bound of the numeric range *) }

(** Parameters of the [Object] type specifier. *)
and object_specs =
  { properties : (string * element * bool * Json_repr.any option) list ;
    (** The names and types of properties, with a flag to indicate if
        they are required ([true]) or optional. *)
    pattern_properties : (string * element) list ;
    (** Alternative definition of properties, matching field names
        using regexps instead of constant strings. *)
    additional_properties : element option ;
    (** The type of additional properties, if allowed. *)
    min_properties : int ;
    (** The minimum number of properties. *)
    max_properties : int option ;
    (** The maximum number of properties. *)
    schema_dependencies : (string * element) list ;
    (** Additional schemas the value must verify if a property is
        present (property, additional schema). *)
    property_dependencies : (string * string list) list
    (** Additional properties required whenever some property is
        present (property, additional properties). *) }

(** Parameters of the [String] type specifier. *)
and string_specs =
  { pattern : string option ;
    (** A regexp the string must conform to. *)
    min_length : int ;
    (** The minimum string length. *)
    max_length : int option
    (** The maximum string length. *) }

(** {2 Combinators to build schemas and elements} *) (*************************)

(** Construct a naked element (all optional properties to None). *)
val element : element_kind -> element

(** Construct a schema from its root, without any definition ; the
    element is checked not to contain any [Def] element. *)
val create : element -> schema

(** Extract the root element from an existing schema. *)
val root : schema -> element

(** Update a schema from its root, using the definitions from an
    existing schema ; the element is checked to contain only valid
    [Def] elements ; unused definitions are kept, see {!simplify}. *)
val update : element -> schema -> schema

(** Describes the implemented schema specification as a schema. *)
val self : schema

(** A completely generic schema, without any definition. *)
val any : schema

(** Combines several schemas. *)
val combine : combinator -> schema list -> schema

(** Tells is a schema accepts null. *)
val is_nullable : schema -> bool

(** {2 Named definitions} *) (***********************************************)

(** Merges the definitions of two schemas if possible and returns the
    updated schemas, so that their elements can be mixed without
    introducing dangling references ; if two different definitions are
    bound to the same path, {!Duplicate_definition} will be raised. *)
val merge_definitions : schema * schema -> schema * schema

(** Remove the definitions that are not present in the schema. *)
val simplify : schema -> schema

(** Adds a definition by its path. If the path is absolute (starting
    with a ['/']), it is untouched. Otherwise, it is considered
    relative to ["#/definitions"] as recommended by the standard. May
    raise {!Duplicate_definition} if this path is already used or any
    error raised by {!Json_repr.path_of_json_pointer} with
    [~wildcards:false]. Returns the modified schema and the [Def_ref]
    node that references this definition to be used in the schema. *)
val add_definition : ?definitions_path:string -> string -> element -> schema -> schema * element

(** Finds a definition by its path, may raise [Not_found].
    See {!add_definition} for the name format.*)
val find_definition : ?definitions_path:string -> string -> schema -> element

(** Tells if a path leads to a definition.
    See {!add_definition} for the name format. *)
val definition_exists : ?definitions_path:string -> string -> schema -> bool

(** Build a reference to a definition.
    See {!add_definition} for the name format. *)
val definition_ref : ?definitions_path:string -> string -> element

(** {2 Predefined values} *) (***********************************************)

(** Default Parameters of the [Array] and [MonomorphicArray] type specifiers. *)
val array_specs : array_specs

(** Default parameters of the [Object] type specifier. *)
val object_specs : object_specs

(** Default parameters of the [String] type specifier. *)
val string_specs : string_specs

(** Default parameters of the [Integer] and [Number] type specifiers. *)
val numeric_specs : numeric_specs

(** {2 JSON Serialization} *) (*********************************************)

(** Formats a JSON schema as its JSON representation.

    This function works with JSON data represented in the {!Json_repr.ezjsonm}
    format. See functor {!Make} for using another representation. *)
val to_json : schema -> Json_repr.ezjsonm

(** Parse a JSON structure as a JSON schema, if possible.
    May throw {!Cannot_parse}.

    This function works with JSON data represented in the {!Json_repr.ezjsonm}
    format. See functor {!Make} for using another representation. *)
val of_json : Json_repr.ezjsonm -> schema

(** Formats a JSON schema in human readable format. *)
val pp : Format.formatter -> schema -> unit

(** {2 Errors} *) (**********************************************************)

(** An error happened during parsing.
    May box one of the following exceptions, among others.. *)
exception Cannot_parse of Json_query.path * exn

(** A reference to a non-existent location was detected. *)
exception Dangling_reference of Uri.t

(** A reference litteral could not be understood. *)
exception Bad_reference of string

(** An unexpected kind of JSON value was encountered. *)
exception Unexpected of string * string

(** A non-[Dummy] definition appeared twice on insertion or merge. *)
exception Duplicate_definition of Json_query.path * element * element

(** Produces a human readable version of an error. *)
val print_error
  : ?print_unknown: (Format.formatter -> exn -> unit) ->
  Format.formatter -> exn -> unit

(** {2 Advanced interface for using a custom JSON representation} *) (**********)

module Make (Repr : Json_repr.Repr) : sig

  (** Same as {!to_json} for a custom JSON representation. *)
  val to_json : schema -> Repr.value

  (** Same as {!of_json} for a custom JSON representation. *)
  val of_json : Repr.value -> schema

end
