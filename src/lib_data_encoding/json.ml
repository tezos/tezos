(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)


type json =
  [ `O of (string * json) list
  | `Bool of bool
  | `Float of float
  | `A of json list
  | `Null
  | `String of string ]

type schema = Json_schema.schema

type pair_builder = {
  build: 'a 'b. Encoding.Kind.t -> 'a Encoding.t -> 'b Encoding.t -> ('a * 'b) Encoding.t
}

exception Parse_error of string

let wrap_error f =
  fun str ->
    try f str
    with exn -> raise (Json_encoding.Cannot_destruct ([], exn))

let int64_encoding =
  let open Json_encoding in
  union [
    case
      int32
      (fun i ->
         let j = Int64.to_int32 i in
         if Int64.equal (Int64.of_int32 j) i then Some j else None)
      Int64.of_int32 ;
    case
      string
      (fun i -> Some (Int64.to_string i))
      Int64.of_string
  ]

let n_encoding =
  let open Json_encoding in
  def "positive_bignum"
    ~title: "Positive big number"
    ~description: "Decimal representation of a positive big number" @@
  conv
    (fun z ->
       if Z.sign z < 0 then
         raise (Json_encoding.Cannot_destruct ([], Failure "negative natural")) ;
       Z.to_string z)
    (fun s ->
       let n = Z.of_string s in
       if Z.sign n < 0 then
         raise (Json_encoding.Cannot_destruct ([], Failure "negative natural")) ;
       n)
    string

let z_encoding =
  let open Json_encoding in
  def "bignum"
    ~title: "Big number"
    ~description: "Decimal representation of a big number" @@
  conv Z.to_string Z.of_string string

let bytes_jsont =
  let open Json_encoding in
  let schema =
    let open Json_schema in
    create
      { title = None ;
        description = None ;
        default = None;
        enum = None;
        kind = String {
            pattern = Some "^[a-zA-Z0-9]+$";
            min_length = 0;
            max_length = None;
          };
        format = None ;
        id = None } in
  conv ~schema
    MBytes.to_hex
    (wrap_error MBytes.of_hex)
    (conv
       (fun (`Hex h) -> h)
       (fun h -> `Hex h)
       string)

let rec lift_union : type a. a Encoding.t -> a Encoding.t = fun e ->
  let open Encoding in
  match e.encoding with
  | Conv { proj ; inj ; encoding = e ; schema } -> begin
      match lift_union e with
      | { encoding = Union (kind, tag, cases) } ->
          make @@
          Union (kind, tag,
                 List.map
                   (fun (Case { name ; encoding ; proj = proj' ; inj = inj' ; tag }) ->
                      Case { encoding ;
                             name ;
                             proj = (fun x -> proj' (proj x));
                             inj = (fun x -> inj (inj' x)) ;
                             tag })
                   cases)
      | e -> make @@ Conv { proj ; inj ; encoding = e ; schema }
    end
  | Objs (p, e1, e2) ->
      lift_union_in_pair
        { build = fun p e1 e2 -> make @@ Objs (p, e1, e2) }
        p e1 e2
  | Tups (p, e1, e2) ->
      lift_union_in_pair
        { build = fun p e1 e2 -> make @@ Tups (p, e1, e2) }
        p e1 e2
  | _ -> e

and lift_union_in_pair
  : type a b. pair_builder -> Encoding.Kind.t -> a Encoding.t -> b Encoding.t -> (a * b) Encoding.t
  = fun b p e1 e2 ->
    let open Encoding in
    match lift_union e1, lift_union e2 with
    | e1, { encoding = Union (_kind, tag, cases) } ->
        make @@
        Union (`Dynamic (* ignored *), tag,
               List.map
                 (fun (Case { name ; encoding = e2 ; proj ; inj ; tag }) ->
                    Case { encoding = lift_union_in_pair b p e1 e2 ;
                           name ;
                           proj = (fun (x, y) ->
                               match proj y with
                               | None -> None
                               | Some y -> Some (x, y)) ;
                           inj = (fun (x, y) -> (x, inj y)) ;
                           tag })
                 cases)
    | { encoding = Union (_kind, tag, cases) }, e2 ->
        make @@
        Union (`Dynamic (* ignored *), tag,
               List.map
                 (fun (Case { name ; encoding = e1 ; proj ; inj ; tag }) ->
                    Case { encoding = lift_union_in_pair b p e1 e2 ;
                           name ;
                           proj = (fun (x, y) ->
                               match proj x with
                               | None -> None
                               | Some x -> Some (x, y)) ;
                           inj = (fun (x, y) -> (inj x, y)) ;
                           tag })
                 cases)
    | e1, e2 -> b.build p e1 e2

let rec json : type a. a Encoding.desc -> a Json_encoding.encoding =
  let open Encoding in
  let open Json_encoding in
  function
  | Null -> null
  | Empty -> empty
  | Constant s -> constant s
  | Ignore -> unit
  | Int8 -> ranged_int ~minimum:~-(1 lsl 7) ~maximum:((1 lsl 7) - 1) "int8"
  | Uint8 -> ranged_int ~minimum:0 ~maximum:((1 lsl 8) - 1) "uint8"
  | Int16 -> ranged_int ~minimum:~-(1 lsl 15) ~maximum:((1 lsl 15) - 1) "int16"
  | Uint16 -> ranged_int ~minimum:0 ~maximum:((1 lsl 16) - 1) "uint16"
  | RangedInt { minimum ; maximum } -> ranged_int ~minimum ~maximum "rangedInt"
  | Int31 -> int
  | Int32 -> int32
  | Int64 -> int64_encoding
  | N -> n_encoding
  | Z -> z_encoding
  | Bool -> bool
  | Float -> float
  | RangedFloat { minimum; maximum } -> ranged_float ~minimum ~maximum "rangedFloat"
  | String (`Fixed expected) ->
      let check s =
        let found = String.length s in
        if found <> expected then
          raise (Cannot_destruct
                   ([] ,
                    Unexpected (Format.asprintf "string (len %d)" found,
                                Format.asprintf "string (len %d)" expected))) ;
        s in
      conv check check string
  | String _ -> string
  | Bytes (`Fixed expected) ->
      let check s =
        let found = MBytes.length s in
        if found <> expected then
          raise (Cannot_destruct
                   ([] ,
                    Unexpected (Format.asprintf "string (len %d)" found,
                                Format.asprintf "string (len %d)" expected))) ;
        s in
      conv check check bytes_jsont
  | Bytes _ -> bytes_jsont
  | String_enum (tbl, _) -> string_enum (Hashtbl.fold (fun a (str, _) acc -> (str, a) :: acc) tbl [])
  | Array e -> array (get_json e)
  | List e -> list (get_json e)
  | Obj f -> obj1 (field_json f)
  | Objs (_, e1, e2) ->
      merge_objs (get_json e1) (get_json e2)
  | Tup e -> tup1 (get_json e)
  | Tups (_, e1, e2) ->
      merge_tups (get_json e1) (get_json e2)
  | Conv { proj ; inj ; encoding = e ; schema } -> conv ?schema proj inj (get_json e)
  | Describe { id ; title ; description ; encoding = e } ->
      def id ?title ?description (get_json e)
  | Mu (_, name, _, _, self) as ty ->
      mu name (fun json_encoding -> get_json @@ self (make ~json_encoding ty))
  | Union (_tag_size, _, cases) -> union (List.map case_json cases)
  | Splitted { json_encoding } -> json_encoding
  | Dynamic_size { encoding = e } -> get_json e
  | Check_size { encoding } -> get_json encoding
  | Delayed f -> get_json (f ())

and field_json
  : type a. a Encoding.field -> a Json_encoding.field =
  let open Json_encoding in
  function
  | Encoding.Req { name ; encoding = e } -> req name (get_json e)
  | Encoding.Opt { name ; encoding = e } -> opt name (get_json e)
  | Encoding.Dft { name ; encoding = e ; default = d} -> dft name (get_json e) d

and case_json : type a. a Encoding.case -> a Json_encoding.case =
  let open Json_encoding in
  function
  | Encoding.Case { encoding = e ; proj ; inj ; _ } -> case (get_json e) proj inj

and get_json : type a. a Encoding.t -> a Json_encoding.encoding = fun e ->
  match e.json_encoding with
  | None ->
      let json_encoding = json (lift_union e).encoding in
      e.json_encoding <- Some json_encoding ;
      json_encoding
  | Some json_encoding -> json_encoding

let convert = get_json

type path = path_item list
and path_item =
  [ `Field of string
  (** A field in an object. *)
  | `Index of int
  (** An index in an array. *)
  | `Star
  (** Any / every field or index. *)
  | `Next
    (** The next element after an array. *) ]

include Json_encoding

let construct e v = construct (get_json e) v
let destruct e v = destruct (get_json e) v
let schema e = schema (get_json e)

let cannot_destruct fmt =
  Format.kasprintf
    (fun msg -> raise (Cannot_destruct ([], Failure msg)))
    fmt

type t = json

let to_root = function
  | `O ctns -> `O ctns
  | `A ctns -> `A ctns
  | `Null -> `O []
  | oth -> `A [ oth ]

let to_string ?minify j =
  Format.asprintf "%a" Json_repr.(pp ?compact:minify (module Ezjsonm)) j

let pp = Json_repr.(pp (module Ezjsonm))

let from_string s =
  match Ezjsonm.from_string ("[" ^ s ^ "]") with
  | exception Ezjsonm.Parse_error (_, msg) -> Error msg
  | `A [ json ] -> Ok json
  | _ -> Error "Malformed value"

let from_stream (stream: string Lwt_stream.t) =
  let buffer = ref "" in
  Lwt_stream.filter_map
    (fun str ->
       buffer := !buffer ^ str ;
       try
         let json = Ezjsonm.from_string !buffer in
         buffer := "" ;
         Some (Ok json)
       with Ezjsonm.Parse_error _ ->
         None)
    stream

let encoding =
  let binary : Json_repr.ezjsonm Encoding.t =
    Encoding.conv
      (fun json ->
         Json_repr.convert
           (module Json_repr.Ezjsonm)
           (module Json_repr_bson.Repr)
           json |>
         Json_repr_bson.bson_to_bytes |>
         Bytes.to_string)
      (fun s -> try
          Bytes.of_string s |>
          Json_repr_bson.bytes_to_bson ~copy:false |>
          Json_repr.convert
            (module Json_repr_bson.Repr)
            (module Json_repr.Ezjsonm)
        with
        | Json_repr_bson.Bson_decoding_error (msg, _, _) ->
            raise (Parse_error msg))
      Encoding.string in
  let json =
    Json_encoding.any_ezjson_value in
  Encoding.raw_splitted ~binary ~json

let schema_encoding =
  Encoding.conv
    Json_schema.to_json
    Json_schema.of_json
    encoding

