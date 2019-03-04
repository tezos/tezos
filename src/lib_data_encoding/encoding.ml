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

module Kind = struct

  type t =
    [ `Fixed of int
    | `Dynamic
    | `Variable ]

  type length =
    [ `Fixed of int
    | `Variable ]

  type enum =
    [ `Dynamic
    | `Variable ]

  let combine name : t -> t -> t = fun k1 k2 ->
    match k1, k2 with
    | `Fixed n1, `Fixed n2 -> `Fixed (n1 + n2)
    | `Dynamic, `Dynamic | `Fixed _, `Dynamic
    | `Dynamic, `Fixed _ -> `Dynamic
    | `Variable, `Fixed _
    | (`Dynamic | `Fixed _), `Variable -> `Variable
    | `Variable, `Dynamic ->
        Printf.ksprintf invalid_arg
          "Cannot merge two %s when the left element is of variable length \
           and the right one of dynamic length. \
           You should use the reverse order, or wrap the second one \
           with Data_encoding.dynamic_size."
          name
    | `Variable, `Variable ->
        Printf.ksprintf invalid_arg
          "Cannot merge two %s with variable length. \
           You should wrap one of them with Data_encoding.dynamic_size."
          name

  let merge : t -> t -> t = fun k1 k2 ->
    match k1, k2 with
    | `Fixed n1, `Fixed n2 when n1 = n2 -> `Fixed n1
    | `Fixed _, `Fixed _ -> `Dynamic
    | `Dynamic, `Dynamic | `Fixed _, `Dynamic
    | `Dynamic, `Fixed _ -> `Dynamic
    | `Variable, (`Dynamic | `Fixed _)
    | (`Dynamic | `Fixed _), `Variable
    | `Variable, `Variable -> `Variable

  let merge_list sz : t list -> t = function
    | [] -> assert false (* should be rejected by Data_encoding.union *)
    | k :: ks ->
        match List.fold_left merge k ks with
        | `Fixed n -> `Fixed (n + Binary_size.tag_size sz)
        | k -> k

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

let rec classify : type a. a t -> Kind.t = fun e ->
  classify_desc e.encoding
and classify_desc : type a. a desc -> Kind.t = fun e ->
  match e with
  (* Fixed *)
  | Null -> `Fixed 0
  | Empty -> `Fixed 0
  | Constant _ -> `Fixed 0
  | Bool -> `Fixed Binary_size.bool
  | Int8 -> `Fixed Binary_size.int8
  | Uint8 -> `Fixed Binary_size.uint8
  | Int16 -> `Fixed Binary_size.int16
  | Uint16 -> `Fixed Binary_size.uint16
  | Int31 -> `Fixed Binary_size.int31
  | Int32 -> `Fixed Binary_size.int32
  | Int64 -> `Fixed Binary_size.int64
  | N -> `Dynamic
  | Z -> `Dynamic
  | RangedInt { minimum ; maximum } ->
      `Fixed Binary_size.(integer_to_size @@ range_to_size ~minimum ~maximum)
  | Float -> `Fixed Binary_size.float
  | RangedFloat _ -> `Fixed Binary_size.float
  (* Tagged *)
  | Bytes kind -> (kind :> Kind.t)
  | String kind -> (kind :> Kind.t)
  | Padded ({ encoding ; _ }, n) -> begin
      match classify_desc encoding with
      | `Fixed m -> `Fixed (n+m)
      | _ -> assert false (* by construction (see [Fixed.padded]) *)
    end
  | String_enum (_, cases) ->
      `Fixed Binary_size.(integer_to_size @@ enum_size cases)
  | Obj (Opt { kind ; _ }) -> (kind :> Kind.t)
  | Objs { kind ; _ } -> kind
  | Tups { kind ; _ } -> kind
  | Union { kind ; _ } -> (kind :> Kind.t)
  | Mu { kind ; _ } -> (kind :> Kind.t)
  (* Variable *)
  | Ignore -> `Fixed 0
  | Array _ -> `Variable
  | List _ -> `Variable
  (* Recursive *)
  | Obj (Req { encoding ; _ }) -> classify encoding
  | Obj (Dft { encoding ; _ }) -> classify encoding
  | Tup encoding -> classify encoding
  | Conv { encoding ; _ } -> classify encoding
  | Describe { encoding ; _ } -> classify encoding
  | Splitted { encoding ; _ } -> classify encoding
  | Dynamic_size _ -> `Dynamic
  | Check_size { encoding ; _ } -> classify encoding
  | Delayed f -> classify (f ())

let make ?json_encoding encoding = { encoding ; json_encoding }

module Fixed = struct
  let string n =
    if n <= 0 then
      invalid_arg "Cannot create a string encoding of negative or null fixed length." ;
    make @@ String (`Fixed n)
  let bytes n =
    if n <= 0 then
      invalid_arg "Cannot create a byte encoding of negative or null fixed length." ;
    make @@ Bytes (`Fixed n)
  let add_padding e n =
    if n <= 0 then
      invalid_arg "Cannot create a padding of negative or null fixed length." ;
    match classify e with
    | `Fixed _ ->
        make @@ Padded (e, n)
    | _ -> invalid_arg "Cannot pad non-fixed size encoding"
end

let rec is_zeroable: type t. t encoding -> bool = fun e ->
  (* Whether an encoding can ever produce zero-byte of encoding. It is dnagerous
     to place zero-size elements in a collection (list/array) because
     they are indistinguishable from the abscence of elements. *)
  match e.encoding with
  (* trivially true *)
  | Null -> true (* always true *)
  | Empty -> true (* always true *)
  | Ignore -> true (* always true *)
  | Constant _ -> true (* always true *)
  (* trivially false *)
  | Bool -> false
  | Int8 -> false
  | Uint8 -> false
  | Int16 -> false
  | Uint16 -> false
  | Int31 -> false
  | Int32 -> false
  | Int64 -> false
  | N -> false
  | Z -> false
  | RangedInt _ -> false
  | RangedFloat _ -> false
  | Float -> false
  | Bytes _ -> false
  | String _ -> false
  | Padded _ -> false
  | String_enum _ -> false
  (* true in some cases, but in practice always protected by Dynamic *)
  | Array _ -> true (* 0-element array *)
  | List _ -> true (* 0-element list *)
  (* represented as whatever is inside: truth mostly propagates *)
  | Obj (Req { encoding = e ; _ }) -> is_zeroable e (* represented as-is *)
  | Obj (Opt { kind = `Variable ; _ }) -> true (* optional field ommited *)
  | Obj (Dft { encoding = e ; _ }) -> is_zeroable e (* represented as-is *)
  | Obj _ -> false
  | Objs { left ; right ; _ } -> is_zeroable left && is_zeroable right
  | Tup e -> is_zeroable e
  | Tups { left ; right ; _ } -> is_zeroable left && is_zeroable right
  | Union _ -> false (* includes a tag *)
  (* other recursive cases: truth propagates *)
  | Mu { kind = `Dynamic ; _ } -> false (* size prefix *)
  | Mu { kind = `Variable ; fix ; _ } -> is_zeroable (fix e)
  | Conv { encoding ; _ } -> is_zeroable encoding
  | Describe { encoding ; _ } -> is_zeroable encoding
  | Splitted { encoding ; _ } -> is_zeroable encoding
  | Check_size { encoding ; _ } -> is_zeroable encoding
  (* Unscrutable: true by default *)
  | Delayed f -> is_zeroable (f ())
  (* Protected against zeroable *)
  | Dynamic_size _ -> false (* always some data for size *)

module Variable = struct
  let string = make @@ String `Variable
  let bytes = make @@ Bytes `Variable
  let check_not_variable name e =
    match classify e with
    | `Variable ->
        Printf.ksprintf invalid_arg
          "Cannot insert variable length element in %s. \
           You should wrap the contents using Data_encoding.dynamic_size." name
    | `Dynamic | `Fixed _ -> ()
  let check_not_zeroable name e =
    if is_zeroable e then
      Printf.ksprintf invalid_arg
        "Cannot insert potentially zero-sized element in %s." name
    else
      ()
  let array ?max_length e =
    check_not_variable "an array" e ;
    check_not_zeroable "an array" e ;
    let encoding = make @@ Array (max_length, e) in
    match classify e, max_length with
    | `Fixed n, Some max_length ->
        let limit = n * max_length in
        make @@ Check_size { limit ; encoding }
    | _, _ -> encoding
  let list ?max_length e =
    check_not_variable "a list" e ;
    check_not_zeroable "a list" e ;
    let encoding = make @@ List (max_length, e) in
    match classify e, max_length with
    | `Fixed n, Some max_length ->
        let limit = n * max_length in
        make @@ Check_size { limit ; encoding }
    | _, _ -> encoding
end

let dynamic_size ?(kind = `Uint30) e =
  make @@ Dynamic_size { kind ; encoding = e }

let check_size limit encoding =
  make @@ Check_size { limit ; encoding }

let delayed f =
  make @@ Delayed f

let null = make @@ Null
let empty = make @@ Empty
let unit = make @@ Ignore
let constant s = make @@ Constant s
let bool = make @@ Bool
let int8 = make @@ Int8
let uint8 = make @@ Uint8
let int16 = make @@ Int16
let uint16 = make @@ Uint16
let int31 = make @@ Int31
let int32 = make @@ Int32
let ranged_int minimum maximum =
  let minimum = min minimum maximum
  and maximum = max minimum maximum in
  if minimum < -(1 lsl 30) || (1 lsl 30) - 1 < maximum then
    invalid_arg "Data_encoding.ranged_int" ;
  make @@ RangedInt { minimum ; maximum  }
let ranged_float minimum maximum =
  let minimum = min minimum maximum
  and maximum = max minimum maximum in
  make @@ RangedFloat { minimum ; maximum }
let int64 = make @@ Int64
let n = make @@ N
let z = make @@ Z
let float = make @@ Float

let string = dynamic_size Variable.string
let bytes = dynamic_size Variable.bytes
let array ?max_length e = dynamic_size (Variable.array ?max_length e)
let list ?max_length e = dynamic_size (Variable.list ?max_length e)

let string_enum = function
  | [] -> invalid_arg "data_encoding.string_enum: cannot have zero cases"
  | [ _case ] -> invalid_arg "data_encoding.string_enum: cannot have a single case, use constant instead"
  | _ :: _ as cases ->
      let arr = Array.of_list (List.map snd cases) in
      let tbl = Hashtbl.create (Array.length arr) in
      List.iteri (fun ind (str, a) -> Hashtbl.add tbl a (str, ind)) cases ;
      make @@ String_enum (tbl, arr)

let conv proj inj ?schema encoding =
  make @@ Conv { proj ; inj ; encoding ; schema }

let def id ?title ?description encoding =
  make @@ Describe { id ; title ; description ; encoding }

let req ?title ?description n t =
  Req { name = n ; encoding = t ; title ; description }
let opt ?title ?description n encoding =
  let kind =
    match classify encoding with
    | `Variable -> `Variable
    | `Fixed _ | `Dynamic -> `Dynamic in
  Opt { name = n ; kind ; encoding ; title ; description }
let varopt ?title ?description n encoding =
  Opt { name = n ; kind = `Variable ; encoding ; title ; description }
let dft ?title ?description n t d =
  Dft { name = n ; encoding = t ; default = d ; title ; description }

let raw_splitted ~json ~binary =
  make @@ Splitted { encoding = binary ;
                     json_encoding = json ;
                     is_obj = false ;
                     is_tup = false }

let rec is_obj : type a. a t -> bool = fun e ->
  match e.encoding with
  | Obj _ -> true
  | Objs _ (* by construction *) -> true
  | Conv { encoding = e ; _ } -> is_obj e
  | Dynamic_size { encoding = e ; _ } -> is_obj e
  | Union { cases ; _ } ->
      List.for_all (fun (Case { encoding = e ; _ }) -> is_obj e) cases
  | Empty -> true
  | Ignore -> true
  | Mu { fix ; _ } -> is_obj (fix e)
  | Splitted { is_obj ; _ } -> is_obj
  | Delayed f -> is_obj (f ())
  | Describe { encoding ; _ } -> is_obj encoding
  | _ -> false

let rec is_tup : type a. a t -> bool = fun e ->
  match e.encoding with
  | Tup _ -> true
  | Tups _ (* by construction *) -> true
  | Conv { encoding = e ; _ } -> is_tup e
  | Dynamic_size { encoding = e ; _ } -> is_tup e
  | Union { cases ; _ } ->
      List.for_all (function Case { encoding = e; _ } -> is_tup e) cases
  | Mu { fix ; _ } -> is_tup (fix e)
  | Splitted { is_tup ; _ } -> is_tup
  | Delayed f -> is_tup (f ())
  | Describe { encoding ; _ } -> is_tup encoding
  | _ -> false

let raw_merge_objs left right =
  let kind = Kind.combine "objects" (classify left) (classify right) in
  make @@ Objs { kind ; left ; right }

let obj1 f1 = make @@ Obj f1
let obj2 f2 f1 =
  raw_merge_objs (obj1 f2) (obj1 f1)
let obj3 f3 f2 f1 =
  raw_merge_objs (obj1 f3) (obj2 f2 f1)
let obj4 f4 f3 f2 f1 =
  raw_merge_objs (obj2 f4 f3) (obj2 f2 f1)
let obj5 f5 f4 f3 f2 f1 =
  raw_merge_objs (obj1 f5) (obj4 f4 f3 f2 f1)
let obj6 f6 f5 f4 f3 f2 f1 =
  raw_merge_objs (obj2 f6 f5) (obj4 f4 f3 f2 f1)
let obj7 f7 f6 f5 f4 f3 f2 f1 =
  raw_merge_objs (obj3 f7 f6 f5) (obj4 f4 f3 f2 f1)
let obj8 f8 f7 f6 f5 f4 f3 f2 f1 =
  raw_merge_objs (obj4 f8 f7 f6 f5) (obj4 f4 f3 f2 f1)
let obj9 f9 f8 f7 f6 f5 f4 f3 f2 f1 =
  raw_merge_objs (obj1 f9) (obj8 f8 f7 f6 f5 f4 f3 f2 f1)
let obj10 f10 f9 f8 f7 f6 f5 f4 f3 f2 f1 =
  raw_merge_objs (obj2 f10 f9) (obj8 f8 f7 f6 f5 f4 f3 f2 f1)

let merge_objs o1 o2 =
  if is_obj o1 && is_obj o2 then
    raw_merge_objs o1 o2
  else
    invalid_arg "Json_encoding.merge_objs"

let raw_merge_tups left right =
  let kind = Kind.combine "tuples" (classify left) (classify right) in
  make @@ Tups { kind ; left ; right }

let tup1 e1 = make @@ Tup e1
let tup2 e2 e1 =
  raw_merge_tups (tup1 e2) (tup1 e1)
let tup3 e3 e2 e1 =
  raw_merge_tups (tup1 e3) (tup2 e2 e1)
let tup4 e4 e3 e2 e1 =
  raw_merge_tups (tup2 e4 e3) (tup2 e2 e1)
let tup5 e5 e4 e3 e2 e1 =
  raw_merge_tups (tup1 e5) (tup4 e4 e3 e2 e1)
let tup6 e6 e5 e4 e3 e2 e1 =
  raw_merge_tups (tup2 e6 e5) (tup4 e4 e3 e2 e1)
let tup7 e7 e6 e5 e4 e3 e2 e1 =
  raw_merge_tups (tup3 e7 e6 e5) (tup4 e4 e3 e2 e1)
let tup8 e8 e7 e6 e5 e4 e3 e2 e1 =
  raw_merge_tups (tup4 e8 e7 e6 e5) (tup4 e4 e3 e2 e1)
let tup9 e9 e8 e7 e6 e5 e4 e3 e2 e1 =
  raw_merge_tups (tup1 e9) (tup8 e8 e7 e6 e5 e4 e3 e2 e1)
let tup10 e10 e9 e8 e7 e6 e5 e4 e3 e2 e1 =
  raw_merge_tups (tup2 e10 e9) (tup8 e8 e7 e6 e5 e4 e3 e2 e1)

let merge_tups t1 t2 =
  if is_tup t1 && is_tup t2 then
    raw_merge_tups t1 t2
  else
    invalid_arg "Tezos_serial.Encoding.merge_tups"

let conv3 ty =
  conv
    (fun (c, b, a) -> (c, (b, a)))
    (fun (c, (b, a)) -> (c, b, a))
    ty
let obj3 f3 f2 f1 = conv3 (obj3 f3 f2 f1)
let tup3 f3 f2 f1 = conv3 (tup3 f3 f2 f1)
let conv4 ty =
  conv
    (fun (d, c, b, a) -> ((d, c), (b, a)))
    (fun ((d, c), (b, a)) -> (d, c, b, a))
    ty
let obj4 f4 f3 f2 f1 = conv4 (obj4 f4 f3 f2 f1)
let tup4 f4 f3 f2 f1 = conv4 (tup4 f4 f3 f2 f1)
let conv5 ty =
  conv
    (fun (e, d, c, b, a) -> (e, ((d, c), (b, a))))
    (fun (e, ((d, c), (b, a))) -> (e, d, c, b, a))
    ty
let obj5 f5 f4 f3 f2 f1 = conv5 (obj5 f5 f4 f3 f2 f1)
let tup5 f5 f4 f3 f2 f1 = conv5 (tup5 f5 f4 f3 f2 f1)
let conv6 ty =
  conv
    (fun (f, e, d, c, b, a) -> ((f, e), ((d, c), (b, a))))
    (fun ((f, e), ((d, c), (b, a))) -> (f, e, d, c, b, a))
    ty
let obj6 f6 f5 f4 f3 f2 f1 = conv6 (obj6 f6 f5 f4 f3 f2 f1)
let tup6 f6 f5 f4 f3 f2 f1 = conv6 (tup6 f6 f5 f4 f3 f2 f1)
let conv7 ty =
  conv
    (fun (g, f, e, d, c, b, a) -> ((g, (f, e)), ((d, c), (b, a))))
    (fun ((g, (f, e)), ((d, c), (b, a))) -> (g, f, e, d, c, b, a))
    ty
let obj7 f7 f6 f5 f4 f3 f2 f1 = conv7 (obj7 f7 f6 f5 f4 f3 f2 f1)
let tup7 f7 f6 f5 f4 f3 f2 f1 = conv7 (tup7 f7 f6 f5 f4 f3 f2 f1)
let conv8 ty =
  conv (fun (h, g, f, e, d, c, b, a) ->
      (((h, g), (f, e)), ((d, c), (b, a))))
    (fun (((h, g), (f, e)), ((d, c), (b, a))) ->
       (h, g, f, e, d, c, b, a))
    ty
let obj8 f8 f7 f6 f5 f4 f3 f2 f1 = conv8 (obj8 f8 f7 f6 f5 f4 f3 f2 f1)
let tup8 f8 f7 f6 f5 f4 f3 f2 f1 = conv8 (tup8 f8 f7 f6 f5 f4 f3 f2 f1)
let conv9 ty =
  conv
    (fun (i, h, g, f, e, d, c, b, a) ->
       (i, (((h, g), (f, e)), ((d, c), (b, a)))))
    (fun (i, (((h, g), (f, e)), ((d, c), (b, a)))) ->
       (i, h, g, f, e, d, c, b, a))
    ty
let obj9 f9 f8 f7 f6 f5 f4 f3 f2 f1 =
  conv9 (obj9 f9 f8 f7 f6 f5 f4 f3 f2 f1)
let tup9 f9 f8 f7 f6 f5 f4 f3 f2 f1 =
  conv9 (tup9 f9 f8 f7 f6 f5 f4 f3 f2 f1)
let conv10 ty =
  conv
    (fun (j, i, h, g, f, e, d, c, b, a) ->
       ((j, i), (((h, g), (f, e)), ((d, c), (b, a)))))
    (fun ((j, i), (((h, g), (f, e)), ((d, c), (b, a)))) ->
       (j, i, h, g, f, e, d, c, b, a))
    ty
let obj10 f10 f9 f8 f7 f6 f5 f4 f3 f2 f1 =
  conv10 (obj10 f10 f9 f8 f7 f6 f5 f4 f3 f2 f1)
let tup10 f10 f9 f8 f7 f6 f5 f4 f3 f2 f1 =
  conv10 (tup10 f10 f9 f8 f7 f6 f5 f4 f3 f2 f1)

let check_cases tag_size cases =
  if cases = [] then
    invalid_arg "Data_encoding.union: empty list of cases." ;
  let max_tag =
    match tag_size with
    | `Uint8 -> 256
    | `Uint16 -> 256 * 256 in
  ignore @@
  List.fold_left
    (fun others (Case { tag ; _ }) ->
       match tag with
       | Json_only -> others
       | Tag tag ->
           if List.mem tag others then
             Format.kasprintf invalid_arg
               "The tag %d appears twice in an union."
               tag ;
           if tag < 0 || max_tag <= tag then
             Format.kasprintf invalid_arg "The tag %d is invalid." tag ;
           tag :: others
    )
    [] cases

let union ?(tag_size = `Uint8) cases =
  check_cases tag_size cases ;
  let kinds =
    List.map (fun (Case { encoding ; _ }) -> classify encoding) cases in
  let kind = Kind.merge_list tag_size kinds in
  make @@ Union { kind ; tag_size ; cases }
let case ~title ?description tag encoding proj inj =
  Case { title ; description ; encoding ; proj ; inj ; tag }

let rec is_nullable: type t. t encoding -> bool = fun e ->
  match e.encoding with
  | Null -> true
  | Empty -> false
  | Ignore -> true
  | Constant _ -> false
  | Bool -> false
  | Int8 -> false
  | Uint8 -> false
  | Int16 -> false
  | Uint16 -> false
  | Int31 -> false
  | Int32 -> false
  | Int64 -> false
  | N -> false
  | Z -> false
  | RangedInt _ -> false
  | RangedFloat _ -> false
  | Float -> false
  | Bytes _ -> false
  | String _ -> false
  | Padded (e, _) -> is_nullable e
  | String_enum _ -> false
  | Array _ -> false
  | List _ -> false
  | Obj _ -> false
  | Objs _ -> false
  | Tup _ -> false
  | Tups _ -> false
  | Union { cases ; _ } ->
      List.exists (fun (Case { encoding = e ; _ }) -> is_nullable e) cases
  | Mu { fix ; _ } -> is_nullable (fix e)
  | Conv { encoding = e ; _ } -> is_nullable e
  | Describe { encoding = e ; _ } -> is_nullable e
  | Splitted { json_encoding ; _ } -> Json_encoding.is_nullable json_encoding
  | Dynamic_size { encoding = e ; _ } -> is_nullable e
  | Check_size { encoding = e ; _ } -> is_nullable e
  | Delayed _ -> true

let option ty =
  if is_nullable ty then
    invalid_arg "Data_encoding.option: cannot nest nullable encodings" ;
  (* TODO add a special construct `Option` in the GADT *)
  union
    ~tag_size:`Uint8
    [ case
        (Tag 1) ty
        ~title:"Some"
        (fun x -> x)
        (fun x -> Some x) ;
      case
        (Tag 0) null
        ~title:"None"
        (function None -> Some () | Some _ -> None)
        (fun () -> None) ;
    ]
let mu name ?title ?description fix =
  let kind =
    try
      let precursor =
        make @@ Mu { kind = `Dynamic ; name ; title ; description ; fix } in
      match classify @@ fix precursor with
      | `Fixed _ | `Dynamic -> `Dynamic
      | `Variable -> raise Exit
    with Exit | _ (* TODO variability error *) ->
      let precursor =
        make @@ Mu { kind = `Variable ; name ; title ; description ; fix } in
      ignore (classify @@ fix precursor) ;
      `Variable in
  make @@ Mu { kind ; name ; title ; description ; fix }

let result ok_enc error_enc =
  union
    ~tag_size:`Uint8
    [ case (Tag 1) ok_enc
        ~title:"Ok"
        (function Ok x -> Some x | Error _ -> None)
        (fun x -> Ok x) ;
      case (Tag 0) error_enc
        ~title:"Result"
        (function Ok _ -> None | Error x -> Some x)
        (fun x -> Error x) ;
    ]

