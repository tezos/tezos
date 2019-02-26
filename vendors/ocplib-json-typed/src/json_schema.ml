(* Abstract representation of JSON schemas. *)

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

(* TODO: validator *)

open Json_query

(* The currently handled version *)
let version = "http://json-schema.org/draft-04/schema#"

(*-- types -----------------------------------------------------------------*)

(* The root of a schema with the named definitions,
   a precomputed ID-element map and a cache for external documents. *)
type schema =
  { root : element ;
    source : Uri.t (* whose fragment should be empty *) ;
    definitions : (path * element) list ;
    ids : (string * element) list ;
    world : schema list }

and element =
  { title : string option ;
    description : string option ;
    default : Json_repr.any option ;
    enum : Json_repr.any list option ;
    kind : element_kind ;
    format : string option ;
    id : string option }

and element_kind =
  | Object of object_specs
  | Array of element list * array_specs
  | Monomorphic_array of element * array_specs
  | Combine of combinator * element list
  | Def_ref of path
  | Id_ref of string
  | Ext_ref of Uri.t
  | String of string_specs
  | Integer of numeric_specs
  | Number of numeric_specs
  | Boolean | Null | Any
  | Dummy

and combinator =
  | Any_of | One_of | All_of | Not

and array_specs =
  { min_items : int ;
    max_items : int option ;
    unique_items : bool ;
    additional_items : element option }

and numeric_specs =
  { multiple_of : float option ;
    minimum : (float * [ `Inclusive | `Exclusive ]) option ;
    maximum : (float * [ `Inclusive | `Exclusive ]) option }

and object_specs =
  { properties : (string * element * bool * Json_repr.any option) list ;
    pattern_properties : (string * element) list ;
    additional_properties : element option ;
    min_properties : int ;
    max_properties : int option ;
    schema_dependencies : (string * element) list ;
    property_dependencies : (string * string list) list }

and string_specs =
  { pattern : string option ;
    min_length : int ;
    max_length : int option }

(* box an element kind without any optional field *)
let element kind =
  { title = None ; description = None ; default = None ; kind ;
    format = None ; enum = None ; id = None }

(*-- equality --------------------------------------------------------------*)

let option_map f = function None -> None | Some v -> Some (f v)

let rec eq_element a b =
  a.title = b.title &&
  a.description = b.description &&
  option_map Json_repr.from_any a.default =
  option_map Json_repr.from_any b.default &&
  option_map (List.map Json_repr.from_any) a.enum =
  option_map (List.map Json_repr.from_any) b.enum &&
  eq_kind a.kind b.kind &&
  a.format = b.format &&
  a.id = b.id

and eq_kind a b = match a, b with
  | Object aa, Object ab -> eq_object_specs aa ab
  | Array (esa, sa), Array (esb, sb) ->
    List.length esa = List.length esb &&
    List.for_all2 eq_element esa esb &&
    eq_array_specs sa sb
  | Monomorphic_array (ea, sa), Monomorphic_array (eb, sb) ->
    eq_element ea eb &&
    eq_array_specs sa sb
  | Combine (ca, esa), Combine (cb, esb) ->
    ca = cb &&
    List.length esa = List.length esb &&
    List.for_all2 eq_element esa esb
  | Def_ref pa, Def_ref pb -> pa = pb
  | Id_ref ra, Id_ref rb -> ra = rb
  | Ext_ref ra, Ext_ref rb -> ra = rb
  | String sa, String sb -> sa = sb
  | Integer na, Integer nb -> na = nb
  | Number na, Number nb -> na = nb
  | Boolean, Boolean -> true
  | Null, Null -> true
  | Any, Any -> true
  | Dummy, Dummy -> true
  | _ -> false

and eq_object_specs a b =
  a.min_properties = b.min_properties &&
  a.max_properties = b.max_properties &&
  List.sort compare a.property_dependencies =
  List.sort compare b.property_dependencies &&
  begin match a.additional_properties, b.additional_properties with
    | Some a, Some b -> eq_element a b
    | None, None -> true
    | _, _ -> false
  end &&
  List.length a.pattern_properties =
  List.length b.pattern_properties &&
  List.for_all2
    (fun (na, ea) (nb, eb) -> na = nb && eq_element ea eb)
    (List.sort (fun (x, _) (y, _) -> compare x y) a.pattern_properties)
    (List.sort (fun (x, _) (y, _) -> compare x y) b.pattern_properties) &&
  List.length a.schema_dependencies =
  List.length b.schema_dependencies &&
  List.for_all2
    (fun (na, ea) (nb, eb) -> na = nb && eq_element ea eb)
    (List.sort (fun (x, _) (y, _) -> compare x y) a.schema_dependencies)
    (List.sort (fun (x, _) (y, _) -> compare x y) b.schema_dependencies) &&
  List.length a.properties =
  List.length b.properties &&
  List.for_all2
    (fun (na, ea, ra, da) (nb, eb, rb, db) ->
       na = nb && eq_element ea eb && ra = rb &&
       option_map Json_repr.from_any da = option_map Json_repr.from_any db)
    (List.sort (fun (x, _, _, _) (y, _, _, _) -> compare x y) a.properties)
    (List.sort (fun (x, _, _, _) (y, _, _, _) -> compare x y) b.properties)

and eq_array_specs a b =
  a.min_items = b.min_items &&
  a.max_items = b.max_items &&
  a.unique_items = b.unique_items &&
  match a.additional_items, b.additional_items with
  | Some a, Some b -> eq_element a b
  | None, None -> true
  | _, _ -> false

(*-- human readable output -------------------------------------------------*)

let pp_string ppf s =
  Json_repr.(pp (module Ezjsonm)) ppf (`String s)
let pp_num ppf m =
  if abs_float m < 1000. then
    Format.fprintf ppf "%g" m
  else
    let pos, m =
      if m < 0. then (false, ~-. m) else (true, m) in
    if List.fold_left (fun acc d ->
        if acc then acc else
          let v = log (m +. d) /. log 2. in
          if abs_float (ceil v -. v) < 0.00001 then begin
            Format.fprintf ppf "%s2^%g" (if pos then "" else "-") v ;
            if (pos && d < 0.) || (not pos && d > 0.) then
              Format.fprintf ppf "+%g" (abs_float d) ;
            if (pos && d > 0.) || (not pos && d < 0.) then
              Format.fprintf ppf "-%g" (abs_float d) ;
            true
          end else false)
        false [ -2. ; -1. ; 0. ; 1. ; 2. ] then () else
      Format.fprintf ppf "%f" m
let pp_numeric_specs ppf { multiple_of ; minimum ; maximum } =
  Format.fprintf ppf "%a%a%a"
    (fun ppf -> function None -> () | Some v -> Format.fprintf ppf "multiple of %g" v)
    multiple_of
    (fun ppf -> function
       | (None, _, _) | (_, None, None) -> ()
       | _ -> Format.fprintf ppf ", ")
    (multiple_of, minimum, maximum)
    (fun ppf -> function
       | None, None -> ()
       | minimum, maximum ->
         Format.fprintf ppf "∈ %a, %a"
           (fun ppf -> function
              | None -> Format.fprintf ppf "]∞"
              | Some (m, `Exclusive) -> Format.fprintf ppf "]%a" pp_num m
              | Some (m, `Inclusive) -> Format.fprintf ppf "[%a" pp_num m)
           minimum
           (fun ppf -> function
              | None -> Format.fprintf ppf "∞["
              | Some (m, `Exclusive) -> Format.fprintf ppf "%a[" pp_num m
              | Some (m, `Inclusive) -> Format.fprintf ppf "%a]" pp_num m)
           maximum)
    (minimum, maximum)
let pp_path ppf = function
  | [ `Field "definitions" ; `Field name ] -> Format.fprintf ppf "%s" name
  | path -> Json_query.(print_path_as_json_path ~wildcards:true) ppf path
let pp_desc element = match element with
  | { title = None ; description = None } -> None
  | { title = Some text ; description = None }
  | { title = None ; description = Some text } ->
    Some begin fun ppf () ->
      Format.fprintf ppf "/* @[<hov 0>%a@] */"
        Format.pp_print_text text
    end
  | { title = Some title ; description = Some description } ->
    Some begin fun ppf () ->
      Format.fprintf ppf "/* @[<v 0>@[<hov 0>%a@]@,@[<hov 0>%a@]@] */"
        Format.pp_print_text title
        Format.pp_print_text description
    end
let rec pp_element ppf element =
  match element.id with
  | Some id ->
    Format.fprintf ppf "#%s" id
  | None ->
    match element.format with
    | Some format ->
      Format.fprintf ppf "%s" format
    | None ->
      match element.enum with
      | Some cases ->
        let pp_sep ppf () =
          Format.fprintf ppf "@ | " in
        Format.fprintf ppf "@[<hv 0>%a@]"
          (Format.pp_print_list ~pp_sep (Json_repr.pp_any ~compact: false ()))
          cases
      | None ->
        match pp_desc element with
        | Some pp_desc ->
          let stripped =
            { element with title = None ; description = None } in
          begin match element.kind with
            | Combine _ ->
              Format.fprintf ppf "%a@,%a"
                pp_desc () pp_element stripped
            | Object specs ->
              Format.fprintf ppf "@[<v 2>{ %a@,%a }@]"
                pp_desc () pp_object_contents specs
            | _ ->
              Format.fprintf ppf "%a@ %a" pp_element stripped pp_desc ()
          end
        | None ->
          begin match element.kind with
            | String { pattern = None ; min_length = 0 ; max_length = None} ->
              Format.fprintf ppf "string"
            | String { pattern = Some pat ; min_length = 0 ; max_length = None} ->
              Format.fprintf ppf "/%s/" pat
            | String { pattern ; min_length ; max_length } ->
              Format.fprintf ppf "%a (%alength%a)"
                (fun ppf -> function
                   | None -> Format.fprintf ppf "string"
                   | Some pat -> Format.fprintf ppf "/%s/" pat)
                pattern
                (fun ppf n -> if n > 0 then Format.fprintf ppf "%d <= " n)
                min_length
                (fun ppf -> function None -> () | Some m -> Format.fprintf ppf "<= %d" m)
                max_length
            | Integer { multiple_of = None ; minimum = None ; maximum = None } ->
              Format.fprintf ppf "integer"
            | Integer specs ->
              Format.fprintf ppf "integer %a" pp_numeric_specs specs
            | Number { multiple_of = None ; minimum = None ; maximum = None } ->
              Format.fprintf ppf "number"
            | Number specs ->
              Format.fprintf ppf "number %a" pp_numeric_specs specs
            | Id_ref id ->
              Format.fprintf ppf "#%s" id
            | Def_ref path ->
              Format.fprintf ppf "$%a" pp_path path
            | Ext_ref uri ->
              Format.fprintf ppf "$%a" Uri.pp_hum uri
            | Boolean ->
              Format.fprintf ppf "boolean"
            | Null ->
              Format.fprintf ppf "null"
            | Any ->
              Format.fprintf ppf "any"
            | Dummy -> assert false
            | Combine (Not, [ elt ]) ->
              Format.fprintf ppf "! %a" pp_element elt
            | Combine (c, elts) ->
              let pp_sep ppf () = match c with
                | Any_of -> Format.fprintf ppf "@ | "
                | One_of -> Format.fprintf ppf "@ || "
                | All_of -> Format.fprintf ppf "@ && "
                | _ -> assert false in
              Format.fprintf ppf "@[<hv 0>%a@]"
                (Format.pp_print_list ~pp_sep pp_element)
                elts
            | Object { properties = [] ;
                       pattern_properties = [] ;
                       additional_properties = None ;
                       min_properties = 0 ;
                       max_properties = Some 0 ;
                       schema_dependencies = [] ;
                       property_dependencies = [] } ->
              Format.fprintf ppf "{}"
            | Object specs ->
              Format.fprintf ppf "@[<v 2>{ %a }@]"
                pp_object_contents specs
            | Array (_, { max_items = Some 0 })
            | Monomorphic_array (_, { max_items = Some 0 }) ->
              Format.fprintf ppf "[]"
            | Array (elements, { additional_items }) ->
              let pp_sep =
                let first = ref true in
                fun ppf () ->
                  if !first then
                    first := false
                  else
                    Format.fprintf ppf ",@ " in
              Format.fprintf ppf "@[<hv 2>[ " ;
              List.iter (fun elt ->
                  Format.fprintf ppf "%a%a"
                    pp_sep ()
                    pp_element elt)
                elements ;
              begin match additional_items with
                | None -> ()
                | Some { kind = Any } ->
                  Format.fprintf ppf "%a,@ ..." pp_sep ()
                | Some elt ->
                  Format.fprintf ppf "%a,@ %a ..."
                    pp_sep ()
                    pp_element elt
              end ;
              Format.fprintf ppf " ]@]"
            | Monomorphic_array (elt, { additional_items = None }) ->
              Format.fprintf ppf "[ %a ... ]"
                pp_element elt
            | Monomorphic_array (elt, { additional_items = Some { kind = Any } }) ->
              Format.fprintf ppf "@[<hv 2>[ %a ...,@ ... ]@]"
                pp_element elt
            | Monomorphic_array (elt, { additional_items = Some add_elt }) ->
              (* TODO: find a good way to print length *)
              Format.fprintf ppf "@[<hv 2>[ %a ...,@ %a ... ]@]"
                pp_element elt pp_element add_elt
          end
and pp_object_contents ppf
    { properties ; pattern_properties ; additional_properties } =
  (* TODO: find a good way to print length / dependencies *)
  let pp_sep =
    let first = ref true in
    fun ppf () ->
      if !first then
        first := false
      else
        Format.fprintf ppf ",@ " in
  List.iter (fun (name, elt, req, _) ->
      Format.fprintf ppf "%a@[<hv 2>%a%s:@ %a@]"
        pp_sep ()
        pp_string name (if req then "" else "?")
        pp_element elt)
    properties ;
  List.iter (fun (name, elt) ->
      Format.fprintf ppf "%a@[<hv 2>/%s/:@ %a@]"
        pp_sep ()
        name
        pp_element elt)
    pattern_properties ;
  begin match additional_properties with
    | None -> ()
    | Some { kind = Any } ->
      Format.fprintf ppf "%a..." pp_sep ()
    | Some elt ->
      Format.fprintf ppf "%a@[<hv 2>*:@ %a@]"
        pp_sep ()
        pp_element elt
  end
let pp ppf schema =
  Format.fprintf ppf "@[<v 0>" ;
  pp_element ppf schema.root ;
  List.iter (fun (path, elt) ->
      match pp_desc elt with
      | None ->
        Format.fprintf ppf "@,@[<hv 2>$%a:@ %a@]"
          pp_path path
          pp_element elt
      | Some pp_desc ->
        let stripped =
          { elt with title = None ; description = None } in
        Format.fprintf ppf "@,@[<v 2>$%a:@,%a@,%a@]"
          pp_path path
          pp_desc ()
          pp_element stripped)
    schema.definitions ;
  List.iter (fun (id, elt) ->
      match pp_desc elt with
      | None ->
        Format.fprintf ppf "@,@[<hv 2>#%s:@ %a@]"
          id
          pp_element { elt with id = None }
      | Some pp_desc ->
        let stripped =
          { elt with title = None ; description = None ; id = None } in
        Format.fprintf ppf "@,@[<v 2>#%s:@,%a@,%a@]"
          id
          pp_desc ()
          pp_element stripped)
    schema.ids ;
  Format.fprintf ppf "@]"

(*-- errors ----------------------------------------------------------------*)

exception Cannot_parse of path * exn
exception Dangling_reference of Uri.t
exception Bad_reference of string
exception Unexpected of string * string
exception Duplicate_definition of path * element * element

let rec print_error ?print_unknown ppf = function
  | Cannot_parse (path, exn) ->
    Format.fprintf ppf
      "@[<v 2>Schema parse error:@,At %a@,%a@]"
      (Json_query.print_path_as_json_path ~wildcards:true) path
      (print_error ?print_unknown) exn
  | Dangling_reference uri ->
    Format.fprintf ppf
      "Dangling reference %s" (Uri.to_string uri)
  | Bad_reference str ->
    Format.fprintf ppf
      "Illegal reference notation %s" str
  | Unexpected (unex, ex) ->
    Format.fprintf ppf
      "Unexpected %s instead of %s" unex ex
  | Duplicate_definition (name, elt, defelt) ->
    Format.fprintf ppf
      "@[<v 2>Duplicate definition %a@,\
       To be inserted:@,\
      \  @[<v 0>%a@]@,\
       Already present:@,\
      \  @[<v 0>%a@]@]"
      (Json_query.print_path_as_json_pointer ~wildcards:false) name
      pp_element elt
      pp_element defelt
  | exn ->
    Json_query.print_error ?print_unknown ppf exn

(*-- internal definition table handling ------------------------------------*)

let find_definition name defs =
  List.assoc name defs

let definition_exists name defs =
  List.mem_assoc name defs

let insert_definition name elt defs =
  let rec insert = function
    | [] ->
      [ (name, elt) ]
    | (defname, _) as def :: rem when defname <> name ->
      def :: insert rem
    | (_, { kind = Dummy }) :: rem ->
      (name, elt) :: rem
    | (_, defelt) :: rem ->
      if not (eq_element elt defelt) then
        raise (Duplicate_definition (name, elt, defelt)) ;
      (name, elt) :: rem in
  insert defs

module Make (Repr : Json_repr.Repr) = struct

  module Query = Json_query.Make (Repr)
  open Query

  (*-- printer ---------------------------------------------------------------*)

  let to_json schema =
    (* functional JSON building combinators *)
    let obj l = Repr.repr (`O l) in
    let set_always f v =
      [ f, Repr.repr v ] in
    let set_if_some f v cb =
      match v with None -> [] | Some v -> [ f, Repr.repr (cb v) ] in
    let set_if_cons f v cb =
      match v with [] -> [] | v -> [ f, Repr.repr (cb v) ] in
    let set_if_neq f v v' cb =
      if v <> v' then [ f, Repr.repr (cb v) ] else [] in
    (* recursive encoder *)
    let rec format_element
        { title ; description ; default ; enum ; kind ; format } =
      set_if_some "title" title (fun s -> `String s) @
      set_if_some "description" description (fun s -> `String s) @
      begin match kind with
        | Object specs ->
          let required = List.fold_left
              (fun r (n, _, p, _) -> if p then Repr.repr (`String n) :: r else r)
              [] specs.properties in
          let properties =
            List.map
              (fun (n, elt, _, _) -> n, obj (format_element elt))
              specs.properties in
          set_always "type" (`String "object") @
          set_always "properties" (`O properties) @
          set_if_cons "required" required (fun l -> `A l) @
          set_if_cons "patternProperties" specs.pattern_properties
            (fun fs -> `O (List.map (fun (n, elt) -> n, obj (format_element elt)) fs)) @
          set_if_neq "additionalProperties" specs.additional_properties (Some (element Any))
            (function
              | None -> `Bool false
              | Some elt -> `O (format_element elt)) @
          set_if_neq "minProperties" specs.min_properties 0
            (fun i -> `Float (float i)) @
          set_if_some "maxProperties" specs.max_properties
            (fun i -> `Float (float i)) @
          set_if_cons "schemaDependencies" specs.schema_dependencies
            (fun fs -> `O (List.map (fun (n, elt) -> n, obj (format_element elt)) fs)) @
          set_if_cons "propertyDependencies" specs.property_dependencies
            (fun fs ->
               let property_dependencies =
                 let strings ls = List.map (fun s -> Repr.repr (`String s)) ls in
                 List.map (fun (n, ls) -> n, Repr.repr (`A (strings ls))) fs in
               `O property_dependencies)
        | Array (elts, specs) ->
          set_always "type" (`String "array") @
          set_always "items" (`A (List.map (fun elt -> obj (format_element elt)) elts)) @
          set_if_neq "minItems" specs.min_items 0 (fun i -> `Float (float i)) @
          set_if_some "maxItems" specs.max_items (fun i -> `Float (float i)) @
          set_if_neq "uniqueItems" specs.unique_items false (fun b -> `Bool b) @
          set_if_neq "additionalItems"
            specs.additional_items (Some (element Any))
            (function
              | None -> `Bool false
              | Some elt -> `O (format_element elt))
        | Monomorphic_array (elt, {min_items ; max_items ; unique_items }) ->
          set_always "type" (`String "array") @
          set_always "items" (`O (format_element elt)) @
          set_if_neq "minItems"
            min_items 0
            (fun i -> `Float (float i)) @
          set_if_some "maxItems"
            max_items
            (fun i -> `Float (float i)) @
          set_if_neq "uniqueItems"
            unique_items false
            (fun b -> `Bool b)
        | Combine (c, elts) ->
          let combinator = function
            | Any_of -> "anyOf"
            | One_of -> "oneOf"
            | All_of -> "allOf"
            | Not -> "not" in
          set_always (combinator c) (`A (List.map (fun elt -> obj (format_element elt)) elts))
        | Def_ref path ->
          set_always "$ref" (`String ("#" ^ (json_pointer_of_path path)))
        | Id_ref name ->
          set_always "$ref" (`String ("#" ^ name))
        | Ext_ref uri ->
          set_always "$ref" (`String (Uri.to_string uri))
        | Integer specs ->
          set_always "type" (`String "integer") @
          set_if_some "multipleOf"
            specs.multiple_of (fun v -> `Float v) @
          (match specs.minimum with
           | None -> []
           | Some (v, `Inclusive) ->
             [ "minimum", Repr.repr (`Float v) ]
           | Some (v, `Exclusive) ->
             [ "minimum", Repr.repr (`Float v) ;
               "exclusiveMinimum", Repr.repr (`Bool true) ] ) @
          (match specs.maximum with
           | None -> []
           | Some (v, `Inclusive) ->
             [ "maximum", Repr.repr (`Float v) ]
           | Some (v, `Exclusive) ->
             [ "maximum", Repr.repr (`Float v) ;
               "exclusiveMaximum", Repr.repr (`Bool true) ] )
        | Number specs ->
          set_always "type" (`String "number") @
          set_if_some "multipleOf" specs.multiple_of (fun v -> `Float v) @
          (match specs.minimum with
           | None -> []
           | Some (v, `Inclusive) ->
             [ "minimum", Repr.repr (`Float v) ]
           | Some (v, `Exclusive) ->
             [ "minimum", Repr.repr (`Float v) ;
               "exclusiveMinimum", Repr.repr (`Bool true) ] ) @
          (match specs.maximum with
           | None -> []
           | Some (v, `Inclusive) ->
             [ "maximum", Repr.repr (`Float v) ]
           | Some (v, `Exclusive) ->
             [ "maximum", Repr.repr (`Float v) ;
               "exclusiveMaximum", Repr.repr (`Bool true) ] )
        | String { pattern ; min_length ; max_length } ->
          set_always "type" (`String "string") @
          set_if_neq "minLength" min_length 0 (fun i -> `Float (float i)) @
          set_if_some "maxLength" max_length (fun i -> `Float (float i)) @
          set_if_some "pattern" pattern (fun s -> `String s)
        | Boolean ->
          set_always "type" (`String "boolean")
        | Null ->
          set_always "type" (`String "null")
        | Dummy ->
          invalid_arg "Json_schema.to_json: remaining dummy element"
        | Any -> [] end @
      set_if_some "default" default (fun j ->
          Repr.view (Json_repr.any_to_repr (module Repr) j)) @
      set_if_some "enum" enum (fun js ->
          `A (List.map (Json_repr.any_to_repr (module Repr)) js)) @
      set_if_some "format" format (fun s -> `String s) in
    List.fold_left
      (fun acc (n, elt) -> insert n (obj (format_element elt)) acc)
      (obj (set_always "$schema" (`String version) @
            format_element schema.root))
      schema.definitions

  let unexpected kind expected =
    let kind =match kind with
      | `O [] -> "empty object"
      | `A [] -> "empty array"
      | `O _ -> "object"
      | `A _ -> "array"
      | `Null -> "null"
      | `String "" -> "empty string"
      | `String _ -> "string"
      | `Float _ -> "number"
      | `Bool _ -> "boolean" in
    Cannot_parse ([], Unexpected (kind, expected))

  (*-- parser ----------------------------------------------------------------*)

  let at_path p = function Cannot_parse (l, err) -> Cannot_parse (p @ l, err) | exn -> exn
  let at_field n = at_path [ `Field n ]
  let at_index i = at_path [ `Index i ]

  let of_json json =
    (* parser combinators *)
    let opt_field obj n = match Repr.view obj with
      | `O ls -> (try Some (List.assoc n ls) with Not_found -> None)
      | _ -> None in
    let opt_field_view obj n = match Repr.view obj with
      | `O ls -> (try Some (Repr.view (List.assoc n ls)) with Not_found -> None)
      | _ -> None in
    let opt_string_field obj n = match opt_field_view obj n with
      | Some (`String s) -> Some s
      | Some k -> raise (at_field n @@ unexpected k "string")
      | None -> None in
    let opt_bool_field def obj n = match opt_field_view obj n with
      | Some (`Bool b) -> b
      | Some k -> raise (at_field n @@ unexpected k "bool")
      | None -> def in
    let opt_int_field obj n = match opt_field_view obj n with
      | Some (`Float f) when (fst (modf f) = 0.
                              && f <= 2. ** 53.
                              && f >= -2. ** 53.) ->
        Some f
      | Some k -> raise (at_field n @@ unexpected k "integer")
      | None -> None in
    let opt_length_field obj n = match opt_field_view obj n with
      | Some (`Float f) when (fst (modf f) = 0.
                              && f <= 2. ** 30.
                              && f >= 0.) ->
        Some (int_of_float f)
      | Some k -> raise (at_field n @@ unexpected k "length")
      | None -> None in
    let opt_float_field obj n = match opt_field_view obj n with
      | Some (`Float f) -> Some f
      | Some k -> raise (at_field n @@ unexpected k "number")
      | None -> None in
    let opt_array_field obj n = match opt_field_view obj n with
      | Some (`A s) -> Some s
      | Some k -> raise (at_field n @@ unexpected k "array")
      | None -> None in
    let opt_uri_field obj n = match opt_string_field obj n with
      | None -> None
      | Some uri ->
        match Uri.canonicalize (Uri.of_string uri) with
        | exception _ -> raise (Cannot_parse ([], Bad_reference (uri ^ " is not a valid URI")))
        | uri -> Some uri in
    (* local resolution of definitions *)
    let schema_source = match opt_uri_field json "id" with
      | Some uri -> Uri.with_fragment uri None
      | None -> Uri.empty in
    let collected_definitions = ref [] in
    let collected_id_defs = ref [] in
    let collected_id_refs = ref [] in
    let rec collect_definition : Uri.t -> element_kind = fun uri ->
      match Uri.host uri, Uri.fragment uri with
      | Some _ (* Actually means: any of host, user or port is defined. *), _ ->
        Ext_ref uri
      | None, None ->
        raise (Cannot_parse ([], Bad_reference (Uri.to_string uri ^ " has no fragment")))
      | None, Some fragment when not (String.contains fragment '/') ->
        collected_id_refs := fragment :: !collected_id_refs ;
        Id_ref fragment
      | None, Some fragment ->
        let path =
          try path_of_json_pointer ~wildcards:false fragment
          with err -> raise (Cannot_parse ([], err)) in
        try
          let raw = query path json in
          if not (definition_exists path !collected_definitions) then begin
            (* dummy insertion so we don't recurse and we support cycles *)
            collected_definitions := insert_definition path (element Dummy) !collected_definitions ;
            let elt = try parse_element schema_source raw
              with err -> raise (at_path path err) in
            (* actual insertion *)
            collected_definitions := insert_definition path elt !collected_definitions
          end ;
          Def_ref path
        with Not_found -> raise (Cannot_parse ([], Dangling_reference uri))
    (* recursive parser *)
    and parse_element : Uri.t -> Repr.value -> element = fun source json ->
      let id = opt_uri_field json "id" in
      let id, source = match id with
        | None -> None, source
        | Some uri ->
          let uri = Uri.canonicalize (Uri.resolve "http" source uri) in
          Uri.fragment uri, Uri.with_fragment uri None in
      (* We don't support inlined schemas, so we just drop elements with
         external sources and replace them with external references. *)
      if source <> schema_source then
        element (Ext_ref (Uri.with_fragment source id))
      else
        let id = match id with
          | None -> None
          | Some id when String.contains id '/' ->
            raise (at_field "id" @@ Cannot_parse ([], Bad_reference (id ^ " is not a valid ID")))
          | Some id -> Some id in
        (* We parse the various element syntaxes and combine them afterwards. *)
        (* 1. An element with a known type field and associated fields. *)
        let as_kind =
          match opt_field_view json "type" with
          | Some (`String name) ->
            Some (element (parse_element_kind source json name))
          | Some (`A [] as k) ->
            raise (at_field "type" @@ unexpected k "type, type array or operator")
          | Some (`A l) ->
            let rec items i acc = function
              | [] ->
                let kind = Combine (Any_of, List.rev acc) in
                Some (element kind)
              | `String name :: tl ->
                let kind = parse_element_kind source json name in
                let case = element kind in
                items (succ i) (case :: acc) tl
              | k :: _ ->
                raise (at_field "type" @@ at_index i @@ unexpected k "type")
            in items 0 [] (List.map Repr.view l)
          | Some k ->
            raise (at_field "type" @@ unexpected k "type, type array or operator")
          | None -> None in
        (* 2. A reference *)
        let as_ref =
          match opt_uri_field json "$ref" with
          | Some uri ->
            let path = collect_definition uri in
            Some (element path)
          | None -> None in
        (* 3. Combined schemas *)
        let as_nary name combinator others =
          let build = function
            | [] -> None (* not found and no auxiliary case *)
            | [ case ] -> Some case  (* one case -> simplify *)
            | cases -> (* several cases build the combination node with empty options *)
              let kind = Combine (combinator, cases) in
              Some (element kind) in
          match opt_field_view json name with
          | Some (`A (_ :: _ as cases)) (* list of schemas *) ->
            let rec items i acc = function
              | elt :: tl ->
                let elt = try parse_element source elt
                  with err -> raise (at_field name @@ at_index i @@ err) in
                items (succ i) (elt :: acc) tl
              | [] ->
                build (others @ List.rev acc)
            in items 0 [] cases
          | None -> build others
          | Some k -> raise (at_field name @@ unexpected k "a list of elements") in
        (* 4. Negated schema *)
        let as_not =
          match opt_field_view json "not" with
          | None -> None
          | Some elt ->
            let elt = try parse_element source (Repr.repr elt)
              with err -> raise (at_field "not" err) in
            let kind = Combine (Not, [ elt ]) in
            Some (element kind) in
        (* parse optional fields *)
        let title = opt_string_field json "title" in
        let description = opt_string_field json "description" in
        let default = match opt_field json "default" with
          | Some v -> Some (Json_repr.repr_to_any (module Repr) v)
          | None -> None in
        let enum =match opt_array_field json "enum" with
          | Some v -> Some (List.map (Json_repr.repr_to_any (module Repr)) v)
          | None -> None in
        let format = opt_string_field json "format" in (* TODO: check format ? *)
        (* combine all specifications under a big conjunction *)
        let as_one_of = as_nary "oneOf" One_of [] in
        let as_any_of = as_nary "anyOf" Any_of [] in
        let all = [ as_kind ; as_ref ; as_not ; as_one_of ; as_any_of ] in
        let cases = List.flatten (List.map (function None -> [] | Some e -> [ e ]) all) in
        let kind = match as_nary "allOf" All_of cases with
          | None -> Any (* no type, ref or logical combination found *)
          | Some { kind } -> kind in
        (* add optional fields *)
        { title ; description ; default ; format ; kind ; enum ; id }
    and parse_element_kind  source json name =
      let integer_specs json =
        let multiple_of = opt_int_field json "multipleOf" in
        let minimum =
          if opt_bool_field false json "exclusiveMinimum" then
            match opt_int_field json "minimum" with
            | None ->
              let err =
                "minimum field required when exclusiveMinimum is true" in
              raise (Failure err)
            | Some v -> Some (v, `Inclusive)
          else
            match opt_int_field json "minimum" with
            | None -> None
            | Some v -> Some (v, `Exclusive) in
        let maximum =
          if opt_bool_field false json "exclusiveMaximum" then
            match opt_int_field json "maximum" with
            | None ->
              let err =
                "maximum field required when exclusiveMaximum is true" in
              raise (Failure err)
            | Some v -> Some (v, `Inclusive)
          else
            match opt_int_field json "maximum" with
            | None -> None
            | Some v -> Some (v, `Exclusive) in
        { multiple_of ; minimum ; maximum} in
      let numeric_specs json =
        let multiple_of = opt_float_field json "multipleOf" in
        let minimum =
          if opt_bool_field false json "exclusiveMinimum" then
            match opt_float_field json "minimum" with
            | None ->
              let err =
                "minimum field required when exclusiveMinimum is true" in
              raise (Failure err)
            | Some v -> Some (v, `Inclusive)
          else
            match opt_float_field json "minimum" with
            | None -> None
            | Some v -> Some (v, `Exclusive) in
        let maximum =
          if opt_bool_field false json "exclusiveMaximum" then
            match opt_float_field json "maximum" with
            | None ->
              let err =
                "maximum field required when exclusiveMaximum is true" in
              raise (Failure err)
            | Some v -> Some (v, `Inclusive)
          else
            match opt_float_field json "maximum" with
            | None -> None
            | Some v -> Some (v, `Exclusive) in
        { multiple_of ; minimum ; maximum} in
      match name with
      | "integer" ->
        Integer (integer_specs json)
      | "number" ->
        Number (numeric_specs json)
      | "boolean" -> Boolean
      | "null" -> Null
      | "string" ->
        let specs =
          let pattern = opt_string_field json "pattern" in
          let min_length = opt_length_field json "minLength" in
          let max_length = opt_length_field json "maxLength" in
          let min_length = match min_length with None -> 0 | Some l -> l in
          { pattern ; min_length ; max_length } in
        String specs
      | "array" ->
        let specs =
          let unique_items = opt_bool_field false json "uniqueItems" in
          let min_items = opt_length_field json "minItems" in
          let max_items = opt_length_field json "maxItems" in
          let min_items = match min_items with None -> 0 | Some l -> l in
          match opt_field_view json "additionalItems" with
          | Some (`Bool true) ->
            { min_items ; max_items ; unique_items ; additional_items = Some (element Any) }
          | None | Some (`Bool false) ->
            { min_items ; max_items ; unique_items ; additional_items = None }
          | Some elt ->
            let elt = try parse_element source (Repr.repr elt)
              with err -> raise (at_field "additionalItems" err) in
            { min_items ; max_items ; unique_items ; additional_items = Some elt } in
        begin match opt_field_view json "items" with
          | Some (`A elts) ->
            let rec elements i acc = function
              | [] ->
                Array (List.rev acc, specs)
              | elt :: tl ->
                let elt = try parse_element source elt
                  with err -> raise (at_field "items" @@ at_index i err) in
                elements (succ i) (elt :: acc) tl
            in elements 0 [] elts
          | Some elt ->
            let elt = try parse_element source (Repr.repr elt)
              with err -> raise (at_field "items" err) in
            Monomorphic_array (elt, specs)
          | None ->
            Monomorphic_array (element Any, specs) end
      | "object" ->
        let required =
          match opt_array_field json "required" with
          | None ->  []
          | Some l ->
            let rec items i acc = function
              | `String s :: tl -> items (succ i) (s :: acc) tl
              | [] -> List.rev acc
              | k :: _ -> raise (at_field "required" @@ at_index i @@ unexpected k "string")
            in items 0 [] (List.map Repr.view l) in
        let properties =
          match opt_field_view json "properties" with
          | Some (`O props) ->
            let rec items acc = function
              | [] -> List.rev acc
              | (n, elt) :: tl ->
                let elt = try parse_element source elt
                  with err -> raise (at_field "properties" @@ at_field n @@ err) in
                let req = List.mem n required in
                items ((n, elt, req, None) :: acc) tl (* XXX: fixme *)
            in items [] props
          | None -> []
          | Some k -> raise (at_field "properties" @@ unexpected k "object") in
        let additional_properties =
          match opt_field_view json "additionalProperties" with
          | Some (`Bool false) -> None
          | None | Some (`Bool true) -> Some (element Any)
          | Some elt ->
            let elt = try parse_element source (Repr.repr elt)
              with err -> raise (at_field "additionalProperties" err) in
            Some elt in
        let property_dependencies =
          match opt_field_view json "propertyDependencies" with
          | None -> []
          | Some (`O l) ->
            let rec sets sacc = function
              | (n, `A l) :: tl ->
                let rec strings j acc = function
                  | [] -> sets ((n, List.rev acc) :: sacc) tl
                  | `String s :: tl -> strings (succ j) (s :: acc) tl
                  | k :: _ ->
                    raise (at_field "propertyDependencies" @@
                           at_field n @@
                           at_index j @@
                           unexpected k "string")
                in strings 0 [] (List.map Repr.view l)
              | (n, k) :: _ ->
                raise (at_field "propertyDependencies" @@
                       at_field n @@
                       unexpected k "string array")
              | [] -> List.rev sacc
            in sets [] (List.map (fun (n, v) -> (n, Repr.view v)) l)
          | Some k ->
            raise (at_field "propertyDependencies" @@
                   unexpected k "object") in
        let parse_element_assoc field =
          match opt_field_view json field with
          | None -> []
          | Some (`O props) ->
            let rec items acc = function
              | [] -> List.rev acc
              | (n, elt) :: tl ->
                let elt = try parse_element source elt
                  with err -> raise (at_field field @@
                                     at_field n err) in
                items ((n, elt) :: acc) tl
            in items [] props
          | Some k -> raise (at_field field @@ unexpected k "object") in
        let pattern_properties = parse_element_assoc "patternProperties" in
        let schema_dependencies = parse_element_assoc "schemaDependencies" in
        let min_properties =
          match opt_length_field json "minProperties" with
          | None -> 0
          | Some l -> l in
        let max_properties = opt_length_field json "maxProperties" in
        Object { properties ; pattern_properties ;
                 additional_properties ;
                 min_properties ; max_properties ;
                 schema_dependencies ; property_dependencies }
      | n -> raise (Cannot_parse ([], Unexpected (n, "a known type"))) in
    (* parse recursively from the root *)
    let root = parse_element Uri.empty json in
    (* force the addition of everything inside /definitions *)
    (match Repr.view (query [ `Field "definitions" ] json) with
     | `O all ->
       let all = List.map (fun (n, _) -> Uri.of_string ("#/definitions/" ^ n)) all in
       List.iter (fun uri -> collect_definition uri |> ignore) all
     | _ -> ()
     | exception  Not_found -> ()) ;
    (* check the domain of IDs *)
    List.iter
      (fun id ->
         if not (List.mem_assoc id !collected_id_defs) then
           raise (Cannot_parse ([], Dangling_reference (Uri.(with_fragment empty (Some id))))))
      !collected_id_refs ;
    let ids = !collected_id_defs in
    let source = schema_source in
    let world = [] in
    let definitions = !collected_definitions in
    { root ; definitions ; source ; ids ; world }

  (*-- creation and update ---------------------------------------------------*)

  (* Checks that all local refs and ids are defined *)
  let check_definitions root definitions =
    let collected_id_defs = ref [] in
    let collected_id_refs = ref [] in
    let rec check ({ kind ; id } as elt) =
      begin match id with
        | None -> ()
        | Some id -> collected_id_defs := (id, elt) :: !collected_id_defs end ;
      begin match kind with
        | Object { properties ; pattern_properties ;
                   additional_properties ; schema_dependencies } ->
          List.iter (fun (_, e, _, _) -> check e) properties ;
          List.iter (fun (_, e) -> check e) pattern_properties ;
          List.iter (fun (_, e) -> check e) schema_dependencies ;
          (match additional_properties with Some e -> check e | None -> ())
        | Array (es, { additional_items }) ->
          List.iter check es ;
          (match additional_items with Some e -> check e | None -> ())
        | Monomorphic_array (e, { additional_items }) ->
          check e ;
          (match additional_items with Some e -> check e | None -> ())
        | Combine (_, es) ->
          List.iter check es
        | Def_ref path ->
          if not (definition_exists path definitions) then
            let path = json_pointer_of_path path in
            raise (Dangling_reference (Uri.(with_fragment empty) (Some path)))
        | Id_ref id ->
          collected_id_refs := id :: !collected_id_refs ;
        | Ext_ref _ | String _ | Integer _ | Number _ | Boolean | Null | Any | Dummy -> ()
      end in
    (* check the root and definitions *)
    check root ;
    List.iter (fun (_, root) -> check root) definitions ;
    (* check the domain of IDs *)
    List.iter
      (fun id ->
         if not (List.mem_assoc id !collected_id_defs) then
           raise (Dangling_reference (Uri.(with_fragment empty (Some id)))))
      !collected_id_refs ;
    !collected_id_defs

  let create root =
    let ids = check_definitions root [] in
    { root ; definitions = [] ; world = [] ; ids ; source = Uri.empty }

  let root { root } =
    root

  let update root sch =
    let ids = check_definitions root sch.definitions in
    { sch with root ; ids }

  let any =
    create (element Any)

  let self =
    { root = element (Ext_ref (Uri.of_string version)) ;
      definitions = [] ; ids = [] ; world = [] ; source = Uri.empty }

  (* remove unused definitions from the schema *)
  let simplify schema =
    let res = ref [] (* collected definitions *) in
    let rec collect { kind } = match kind with
      | Object { properties ; pattern_properties ;
                 additional_properties ; schema_dependencies } ->
        List.iter (fun (_, e, _, _) -> collect e) properties ;
        List.iter (fun (_, e) -> collect e) pattern_properties ;
        List.iter (fun (_, e) -> collect e) schema_dependencies ;
        (match additional_properties with Some e -> collect e | None -> ())
      | Array (es, { additional_items }) ->
        List.iter collect es ;
        (match additional_items with Some e -> collect e | None -> ())
      | Monomorphic_array (e, { additional_items }) ->
        collect e ;
        (match additional_items with Some e -> collect e | None -> ())
      | Combine (_, es) ->
        List.iter collect es
      | Def_ref path ->
        let def = find_definition path schema.definitions in
        res := insert_definition path def !res
      | Ext_ref _ | Id_ref _ | String _ | Integer _ | Number _ | Boolean | Null | Any | Dummy -> ()
    in
    collect schema.root ;
    { schema with definitions = !res }

  let definition_path_of_name ?(definitions_path="/definitions/") name =
    path_of_json_pointer ~wildcards:false @@
    match String.get name 0 with
    | exception _ -> raise (Bad_reference name)
    | '/' -> name
    | _ -> definitions_path ^ name

  let find_definition ?definitions_path name schema =
    let path = definition_path_of_name ?definitions_path name in
    find_definition path schema.definitions

  let definition_ref ?definitions_path name =
    let path = definition_path_of_name ?definitions_path name in
    element (Def_ref path)

  let definition_exists ?definitions_path name schema =
    let path = definition_path_of_name ?definitions_path name in
    definition_exists path schema.definitions

  let add_definition ?definitions_path name elt schema =
    let path = definition_path_of_name ?definitions_path name in
    (* check inside def *)
    let definitions = insert_definition path elt schema.definitions in
    { schema with definitions }, element (Def_ref path)

  let merge_definitions (sa, sb) =
    let rec sorted_merge = function
      | ((na, da) as a) :: ((nb, db) as b) :: tl ->
        if na = nb then
          if da.kind = Dummy || db.kind = Dummy || eq_element da db then
            (na, da) :: sorted_merge tl
          else
            raise (Duplicate_definition (na, da, db))
        else
          a :: sorted_merge (b :: tl)
      | [] | [ _ ] as rem -> rem
    in
    let definitions =
      sorted_merge (List.sort compare (sa.definitions @ sb.definitions)) in
    { sa with definitions }, { sb with definitions }

  let combine op schemas =
    let rec combine sacc eacc = function
      | [] -> update (element (Combine (op, eacc))) sacc
      | s :: ss ->
        let sacc, s = merge_definitions (sacc, s) in
        combine sacc (s.root :: eacc) ss
    in combine any [] schemas

  let is_nullable { ids ; definitions ; root } =
    let rec nullable { kind } = match kind with
      | Null | Any -> true
      | Object _
      | Array _
      | Monomorphic_array _
      | Ext_ref _
      | String _
      | Integer _
      | Number _
      | Boolean -> false
      | Combine (Not, [ elt ]) ->
        not (nullable elt)
      | Combine (All_of, elts) ->
        List.for_all nullable elts
      | Combine ((Any_of | One_of), elts) ->
        List.exists nullable elts
      | Def_ref path ->
        nullable (List.assoc path definitions)
      | Id_ref id ->
        nullable (List.assoc id ids)
      | Combine (Not, _) | Dummy -> assert false in
    nullable root


  (*-- default specs ---------------------------------------------------------*)

  let array_specs =
    { min_items = 0 ;
      max_items = None ;
      unique_items = false ;
      additional_items = None }
  let object_specs =
    { properties = [] ;
      pattern_properties = [] ;
      additional_properties = Some (element Any) ;
      min_properties = 0 ;
      max_properties = None ;
      schema_dependencies = [] ;
      property_dependencies = [] }
  let string_specs =
    { pattern = None ;
      min_length = 0 ;
      max_length = None }
  let numeric_specs =
    { multiple_of = None ;
      minimum = None ;
      maximum = None }
end

include Make (Json_repr.Ezjsonm)
