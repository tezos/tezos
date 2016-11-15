(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Utils

type json =
  [ `O of (string * json) list
  | `Bool of bool
  | `Float of float
  | `A of json list
  | `Null
  | `String of string ]

and document =
  [ `O of (string * json) list
  | `A of json list ]

type json_schema = Json_schema.schema

exception No_case_matched
exception Unexpected_tag of int
exception Duplicated_tag of int
exception Invalid_tag of int * [ `Uint8 | `Uint16 ]
exception Unexpected_enum of string * string list

let apply fs v =
  let rec loop = function
    | [] -> raise No_case_matched
    | f :: fs ->
        match f v with
        | Some l -> l
        | None -> loop fs in
  loop fs

module Size = struct
  let bool = 1
  let int8 = 1
  let uint8 = 1
  let int16 = 2
  let uint16 = 2
  let int31 = 4
  let int32 = 4
  let int64 = 8
  let float = 8
end

type tag_size = [ `Uint8 | `Uint16 ]

let tag_size = function
  | `Uint8 -> Size.uint8
  | `Uint16 -> Size.uint16

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
    | `Variable, (`Dynamic | `Fixed _)
    | (`Dynamic | `Fixed _), `Variable -> `Variable
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
        | `Fixed n -> `Fixed (n + tag_size sz)
        | k -> k

end

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
  | Float : float desc
  | Bytes : Kind.length -> MBytes.t desc
  | String : Kind.length -> string desc
  | String_enum : Kind.length * (string * 'a) list -> 'a desc
  | Array : 'a t -> 'a array desc
  | List : 'a t -> 'a list desc
  | Obj : 'a field -> 'a desc
  | Objs : Kind.t * 'a t * 'b t -> ('a * 'b) desc
  | Tup : 'a t -> 'a desc
  | Tups : Kind.t * 'a t * 'b t -> ('a * 'b) desc
  | Union : Kind.t * tag_size * 'a case list -> 'a desc
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
        json_encoding : 'a Json_encoding.encoding } -> 'a desc
  | Dynamic_size : 'a t -> 'a desc

and _ field =
  | Req : string * 'a t -> 'a field
  | Opt : Kind.enum * string * 'a t -> 'a option field
  | Dft : string * 'a t * 'a -> 'a field

and 'a case =
  | Case : { encoding : 'a t ;
             proj : ('t -> 'a option) ;
             inj : ('a -> 't) ;
             tag : int option } -> 't case

and 'a t = {
  encoding: 'a desc ;
  mutable json_encoding: 'a Json_encoding.encoding option ;
}

type 'a encoding = 'a t

let rec classify : type a l. a t -> Kind.t = fun e ->
  let open Kind in
  match e.encoding with
  (* Fixed *)
  | Null -> `Fixed 0
  | Empty -> `Fixed 0
  | Constant _ -> `Fixed 0
  | Bool -> `Fixed Size.bool
  | Int8 -> `Fixed Size.int8
  | Uint8 -> `Fixed Size.uint8
  | Int16 -> `Fixed Size.int16
  | Uint16 -> `Fixed Size.uint16
  | Int31 -> `Fixed Size.int31
  | Int32 -> `Fixed Size.int32
  | Int64 -> `Fixed Size.int64
  | Float -> `Fixed Size.float
  (* Tagged *)
  | Bytes kind -> (kind :> Kind.t)
  | String kind -> (kind :> Kind.t)
  | String_enum (kind, _) -> (kind :> Kind.t)
  | Obj (Opt (kind, _, _)) -> (kind :> Kind.t)
  | Objs (kind, _, _) -> kind
  | Tups (kind, _, _) -> kind
  | Union (kind, _, _) -> (kind :> Kind.t)
  | Mu (kind, _, _) -> (kind :> Kind.t)
  (* Variable *)
  | Ignore -> `Variable
  | Array _ -> `Variable
  | List _ -> `Variable
  (* Recursive *)
  | Obj (Req (_, encoding)) -> classify encoding
  | Obj (Dft (_, encoding, _)) -> classify encoding
  | Tup encoding -> classify encoding
  | Conv { encoding } -> classify encoding
  | Describe { encoding } -> classify encoding
  | Def { encoding } -> classify encoding
  | Splitted { encoding } -> classify encoding
  | Dynamic_size _ -> `Dynamic

let make ?json_encoding encoding = { encoding ; json_encoding }

module Json = struct

  type pair_builder = {
    build: 'a 'b. Kind.t -> 'a t -> 'b t -> ('a * 'b) t
  }

  exception Parse_error of string

  type nonrec json = json

  let to_root = function
    | `O ctns -> `O ctns
    | `A ctns -> `A ctns
    | `Null -> `O []
    | oth -> `A [ oth ]

  let to_string j = Ezjsonm.to_string ~minify:false (to_root j)

  let from_string s =
    try Ok (Ezjsonm.from_string s :> json)
    with Ezjsonm.Parse_error (_, msg) -> Error msg

  let from_stream (stream: string Lwt_stream.t) =
    let buffer = ref "" in
    Lwt_stream.filter_map
      (fun str ->
         buffer := !buffer ^ str ;
         try
           let json = Ezjsonm.from_string !buffer in
           buffer := "" ;
           Some (Ok json)
         with Ezjsonm.Parse_error (_, msg) ->
           if String.length str = 32 * 1024 then None
           else Some (Error msg))
      stream

  let write_file file json =
    let json = to_root json in
    let open Lwt in
    catch
      (fun () ->
         Lwt_io.(with_file ~mode:Output file (fun chan ->
             let str = to_string json in
             write chan str >>= fun _ ->
             return true)))
      (fun _ -> return false)

  let read_file file =
    let open Lwt in
    catch
      (fun () ->
         Lwt_io.(with_file ~mode:Input file (fun chan ->
             read chan >>= fun str ->
             return (Some (Ezjsonm.from_string str :> json)))))
      (fun _ ->
         (* TODO log error or use Error_monad. *)
         return None)

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
           if Compare.Int64.(=) (Int64.of_int32 j) i then Some j else None)
        Int64.of_int32 ;
      case
        string
        (fun i -> Some (Int64.to_string i))
        Int64.of_string
    ]

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
      Hex_encode.hex_of_bytes
      (wrap_error Hex_encode.bytes_of_hex)
      string

  let rec lift_union : type a. a t -> a t = fun e ->
    match e.encoding with
    | Conv { proj ; inj ; encoding = e ; schema } -> begin
        match lift_union e with
        | { encoding = Union (kind, tag, cases) } ->
            make @@
            Union (kind, tag,
                   List.map
                     (fun (Case { encoding ; proj = proj' ; inj = inj' ; tag }) ->
                        Case { encoding ;
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
    : type a a_l b b_l. pair_builder -> Kind.t -> a t -> b t -> (a * b) t
    = fun b p e1 e2 ->
    match lift_union e1, lift_union e2 with
    | e1, { encoding = Union (_kind, tag, cases) } ->
        make @@
        Union (`Dynamic (* ignored *), tag,
               List.map
                 (fun (Case { encoding = e2 ; proj ; inj ; tag }) ->
                    Case { encoding = lift_union_in_pair b p e1 e2 ;
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
                 (fun (Case { encoding = e1 ; proj ; inj ; tag }) ->
                    Case { encoding = lift_union_in_pair b p e1 e2 ;
                           proj = (fun (x, y) ->
                               match proj x with
                               | None -> None
                               | Some x -> Some (x, y)) ;
                           inj = (fun (x, y) -> (inj x, y)) ;
                           tag })
                 cases)
    | e1, e2 -> b.build p e1 e2

  let rec json : type a l. a desc -> a Json_encoding.encoding =
    let open Json_encoding in
    function
    | Null -> null
    | Empty -> empty
    | Constant s -> string_enum [s, ()]
    | Ignore -> unit
    | Int8 -> ranged_int ~minimum:~-(1 lsl 7) ~maximum:((1 lsl 7) - 1) "int8"
    | Uint8 -> ranged_int ~minimum:0 ~maximum:((1 lsl 8) - 1) "uint8"
    | Int16 -> ranged_int ~minimum:~-(1 lsl 15) ~maximum:((1 lsl 15) - 1) "int16"
    | Uint16 -> ranged_int ~minimum:0 ~maximum:((1 lsl 16) - 1) "uint16"
    | Int31 -> int
    | Int32 -> int32
    | Int64 -> int64_encoding
    | Bool -> bool
    | Float -> float
    | String _ -> string (* TODO: check length *)
    | Bytes _ -> bytes_jsont (* TODO check length *)
    | String_enum (_, l) -> string_enum l
    | Array e -> array (get_json e)
    | List e -> list (get_json e)
    | Obj f -> obj1 (field_json f)
    | Objs (_, e1, e2) ->
        merge_objs (get_json e1) (get_json e2)
    | Tup e -> tup1 (get_json e)
    | Tups (_, e1, e2) ->
        merge_tups (get_json e1) (get_json e2)
    | Conv { proj ; inj ; encoding = e ; schema } -> conv ?schema proj inj (get_json e)
    | Describe { title ; description ; encoding = e } ->
        describe ?title ?description (get_json e)
    | Def { name ; encoding = e } -> def name (get_json e)
    | Mu (_, name, self) as ty ->
        mu name (fun json_encoding -> get_json @@ self (make ~json_encoding ty))
    | Union (_tag_size, _, cases) -> union (List.map case_json cases)
    | Splitted { json_encoding } -> json_encoding
    | Dynamic_size e -> get_json e

  and field_json
    : type a l. a field -> a Json_encoding.field =
    let open Json_encoding in
    function
    | Req (name, e) -> req name (get_json e)
    | Opt (_, name, e) -> opt name (get_json e)
    | Dft (name, e, d) -> dft name (get_json e) d

  and case_json : type a l. a case -> a Json_encoding.case =
    let open Json_encoding in
    function
    | Case { encoding = e ; proj ; inj ; _ } -> case (get_json e) proj inj

  and get_json : type a l. a t -> a Json_encoding.encoding = fun e ->
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

end

module Encoding = struct

  module Fixed = struct
    let string n = make @@ String (`Fixed n)
    let bytes n = make @@ Bytes (`Fixed n)
  end

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
    let array e =
      check_not_variable "an array" e ;
      make @@ Array e
    let list e =
      check_not_variable "a list" e ;
      make @@ List e
    let string_enum l = make @@ String_enum (`Variable, l)
  end

  let dynamic_size e =
    make @@ Dynamic_size e

  let null = make @@ Null
  let empty = make @@ Empty
  let constant s = make @@ Constant s
  let bool = make @@ Bool
  let int8 = make @@ Int8
  let uint8 = make @@ Uint8
  let int16 = make @@ Int16
  let uint16 = make @@ Uint16
  let int31 = make @@ Int31
  let int32 = make @@ Int32
  let int64 = make @@ Int64
  let float = make @@ Float

  let string = dynamic_size Variable.string
  let bytes = dynamic_size Variable.bytes
  let array e = dynamic_size (Variable.array e)
  let list e = dynamic_size (Variable.list e)

  let conv (type l) proj inj ?schema encoding =
    make @@ Conv { proj ; inj ; encoding ; schema }

  let string_enum l = dynamic_size (Variable.string_enum l)

  let describe ?title ?description encoding =
    match title, description with
    | None, None -> encoding
    | _, _ -> make @@ Describe { title ; description ; encoding }

  let def name encoding = make @@ Def { name ; encoding }

  let req ?title ?description n t =
    Req (n, describe ?title ?description t)
  let opt ?title ?description n encoding =
    let kind =
      match classify encoding with
      | `Variable -> `Variable
      | `Fixed _ | `Dynamic -> `Dynamic in
    Opt (kind, n, make @@ Describe { title ; description ; encoding })
  let varopt ?title ?description n encoding =
    Opt (`Variable, n, make @@ Describe { title ; description ; encoding })
  let dft ?title ?description n t d =
    Dft (n, describe ?title ?description t, d)

  let raw_splitted ~json ~binary =
    make @@ Splitted { encoding = binary ; json_encoding = json }

  let splitted ~json ~binary =
    let json = Json.convert json in
    raw_splitted ~binary ~json

  let raw_json json =
    let binary =
      conv
        (fun v -> Json_encoding.construct json v |> Json.to_string)
        (fun s ->
           match Json.from_string s with
           | Error msg -> raise (Json.Parse_error msg)
           | Ok v -> Json_encoding.destruct json v)
        string in
    raw_splitted ~binary ~json

  let json = raw_json Json_encoding.any_ezjson_value
  let json_schema = raw_json Json_encoding.any_schema

  let raw_merge_objs e1 e2 =
    let kind = Kind.combine "objects" (classify e1) (classify e2) in
    make @@ Objs (kind, e1, e2)

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
    let rec is_obj : type a l. a t -> bool = fun e ->
      match e.encoding with
      | Obj _ -> true
      | Objs _ (* by construction *) -> true
      | Conv { encoding = e } -> is_obj e
      | Union (_,_,cases) ->
          List.for_all (fun (Case { encoding = e }) -> is_obj e) cases
      | Empty -> true
      | Ignore -> true
      | _ -> false in
    if is_obj o1 && is_obj o2 then
      raw_merge_objs o1 o2
    else
      invalid_arg "Json_encoding.merge_objs"

  let raw_merge_tups e1 e2 =
    let kind = Kind.combine "tuples" (classify e1) (classify e2) in
    make @@ Tups (kind, e1, e2)

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
    let rec is_tup : type a l. a t -> bool = fun e ->
      match e.encoding with
      | Tup _ -> true
      | Tups _ (* by construction *) -> true
      | Conv { encoding = e } -> is_tup e
      | Union (_,_,cases) ->
          List.for_all (function Case { encoding = e} -> is_tup e) cases
      | _ -> false in
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
      (fun others (Case { tag }) ->
         match tag with
         | None -> others
         | Some tag ->
             if List.mem tag others then raise (Duplicated_tag tag) ;
             if tag < 0 || max_tag <= tag then
               raise (Invalid_tag (tag, tag_size)) ;
             tag :: others
      )
      [] cases

  let union ?(tag_size = `Uint8) cases =
    check_cases tag_size cases ;
    let kinds =
      List.map (fun (Case { encoding }) -> classify encoding) cases in
    let kind = Kind.merge_list tag_size kinds in
    make @@ Union (kind, tag_size, cases)
  let case ?tag encoding proj inj = Case { encoding ; proj ; inj ; tag }
  let option ty =
    union
      ~tag_size:`Uint8
      [ case ~tag:1 ty
          (fun x -> x)
          (fun x -> Some x) ;
        case ~tag:0 empty
          (function None -> Some () | Some _ -> None)
          (fun () -> None) ;
      ]
  let mu name self =
    let kind =
      try
        match classify (self (make @@ Mu (`Dynamic, name, self))) with
        | `Fixed _ | `Dynamic -> `Dynamic
        | `Variable -> raise Exit
      with Exit | _ (* TODO variability error *) ->
        ignore @@ classify (self (make @@ Mu (`Variable, name, self))) ;
        `Variable in
    make @@ Mu (kind, name, self)

  let assoc enc =
    let json = Json_encoding.assoc (Json.get_json enc) in
    let binary = list (tup2 string enc) in
    raw_splitted ~json ~binary
end

include Encoding

module Binary = struct

  type 'l writer = {
    write: 'a. 'a t -> 'a -> MBytes.t -> int -> int ;
  }

  type 'l reader = {
    read: 'a. 'a t -> MBytes.t -> int -> int -> (int * 'a) ;
  }

let rec length : type x. x t -> x -> int = fun e ->
    let open Kind in
    match e.encoding with
    (* Fixed *)
    | Null -> fun _ -> 0
    | Empty -> fun _ -> 0
    | Constant _ -> fun _ -> 0
    | Bool -> fun _ -> Size.bool
    | Int8 -> fun _ -> Size.int8
    | Uint8 -> fun _ -> Size.uint8
    | Int16 -> fun _ -> Size.int16
    | Uint16 -> fun _ -> Size.uint16
    | Int31 -> fun _ -> Size.int31
    | Int32 -> fun _ -> Size.int32
    | Int64 -> fun _ -> Size.int64
    | Float -> fun _ -> Size.float
    | Bytes `Fixed n -> fun _ -> n
    | String `Fixed n -> fun _ -> n
    | String_enum (`Fixed n, _) -> fun _ -> n
    | Objs (`Fixed n, _, _) -> fun _ -> n
    | Tups (`Fixed n, _, _) -> fun _ -> n
    | Union (`Fixed n, _, _) -> fun _ -> n
    (* Dynamic *)
    | Objs (`Dynamic, e1, e2) ->
        let length1 = length e1 in
        let length2 = length e2 in
        fun (v1, v2) -> length1 v1 + length2 v2
    | Tups (`Dynamic, e1, e2) ->
        let length1 = length e1 in
        let length2 = length e2 in
        fun (v1, v2) -> length1 v1 + length2 v2
    | Union (`Dynamic, sz, cases) ->
        let case_length = function
          | Case { tag = None } -> None
          | Case { encoding = e ; proj ; tag = Some _ } ->
              let length v = tag_size sz + length e v in
              Some (fun v -> Utils.map_option length (proj v)) in
        apply (Utils.filter_map case_length cases)
    | Mu (`Dynamic, _name, self) ->
        fun v -> length (self e) v
    | Obj (Opt (`Dynamic, _, e)) ->
        let length = length e in
        (function None -> 1 | Some x -> 1 + length x)
    (* Variable *)
    | Ignore -> fun _ -> 0
    | Bytes `Variable -> MBytes.length
    | String `Variable -> String.length
    | String_enum (`Variable, l) -> begin
        fun v ->
          try
            let l = List.map (fun (x,y) -> (y,x)) l in
            String.length (List.assoc v l)
          with Not_found -> raise No_case_matched
      end
    | Array e ->
        let length = length e in
        fun v ->
          Array.fold_left
            (fun acc v -> length v + acc)
            0 v
    | List e ->
        let length = length e in
        fun v ->
          List.fold_left
            (fun acc v -> length v + acc)
            0 v
    | Objs (`Variable, e1, e2) ->
        let length1 = length e1 in
        let length2 = length e2 in
        fun (v1, v2) -> length1 v1 + length2 v2
    | Tups (`Variable, e1, e2) ->
        let length1 = length e1
        and length2 = length e2 in
        fun (v1, v2) -> length1 v1 + length2 v2
    | Obj (Opt (`Variable, _, e)) ->
        let length = length e in
        (function None -> 0 | Some x -> length x)
    | Union (`Variable, sz, cases) ->
        let case_length = function
          | Case { tag = None } -> None
          | Case { encoding = e ; proj ; tag = Some _ } ->
              let length v = tag_size sz + length e v in
              Some (fun v ->
                  match proj v with
                  | None -> None
                  | Some v -> Some (length v)) in
        apply (Utils.filter_map case_length cases)
    | Mu (`Variable, _name, self) ->
        fun v -> length (self e) v
    (* Recursive*)
    | Obj (Req (_, e)) -> length e
    | Obj (Dft (_, e, _)) -> length e
    | Tup e -> length e
    | Conv  { encoding = e ; proj } ->
        let length = length e in
        fun v -> length (proj v)
    | Describe { encoding = e } -> length e
    | Def { encoding = e } -> length e
    | Splitted { encoding = e } -> length e
    | Dynamic_size e ->
        let length = length e in
        fun v -> Size.int32 + length v

  (** Writer *)

  module Writer = struct

    let int8 v buf ofs =
      if (v < - (1 lsl 7) || v >= 1 lsl 7) then
        invalid_arg "Data_encoding.Binary.Writer.int8" ;
      MBytes.set_int8 buf ofs v;
      ofs + Size.int8

    let uint8 v buf ofs =
      if (v < 0 || v >= 1 lsl 8) then
        invalid_arg "Data_encoding.Binary.Writer.uint8" ;
      MBytes.set_int8 buf ofs v;
      ofs + Size.uint8

    let char v buf ofs =
      uint8 (Char.code v) buf ofs

    let bool v buf ofs =
      uint8 (if v then 255 else 0) buf ofs

    let int16 v buf ofs =
      if (v < - (1 lsl 15) || v >= 1 lsl 15) then
        invalid_arg "Data_encoding.Binary.Writer.int16" ;
      MBytes.set_int16 buf ofs v;
      ofs + Size.int16

    let uint16 v buf ofs =
      if (v < 0 || v >= 1 lsl 16) then
        invalid_arg "Data_encoding.Binary.Writer.uint16" ;
      MBytes.set_int16 buf ofs v;
      ofs + Size.uint16

    let int31 v buf ofs =
      MBytes.set_int32 buf ofs (Int32.of_int v);
      ofs + Size.int31

    let int32 v buf ofs =
      MBytes.set_int32 buf ofs v;
      ofs + Size.int32

    let int64 v buf ofs =
      MBytes.set_int64 buf ofs v;
      ofs + Size.int64

    let float v buf ofs =
      MBytes.set_float buf ofs v;
      ofs + Size.float

    let fixed_kind_bytes length s buf ofs =
      MBytes.blit s 0 buf ofs length;
      ofs + length

    let variable_length_bytes s buf ofs =
      let length = MBytes.length s in
      MBytes.blit s 0 buf ofs length ;
      ofs + length

    let fixed_kind_string length s buf ofs =
      if String.length s <> length then invalid_arg "fixed_kind_string";
      MBytes.blit_from_string s 0 buf ofs length;
      ofs + length

    let variable_length_string s buf ofs =
      let length = String.length s in
      MBytes.blit_from_string s 0 buf ofs length ;
      ofs + length

    let objs w1 w2 (v1,v2) buf ofs =
      w1 v1 buf ofs |> w2 v2 buf

    let array w a buf ofs =
      Array.fold_left (fun ofs v -> w v buf ofs) ofs a

    let list w l buf ofs =
      List.fold_left (fun ofs v -> w v buf ofs) ofs l

    let conv proj w v buf ofs =
      w (proj v) buf ofs

    let write_tag = function
      | `Uint8 -> uint8
      | `Uint16 -> uint16

    let union w sz cases =
      let writes_case = function
        | Case { tag = None } ->
            (fun _ -> None)
        | Case { encoding = e ; proj ; tag = Some tag } ->
            let write = w.write e in
            let write v buf ofs =
              write_tag sz tag buf ofs |> write v buf in
            fun v ->
              match proj v with
              | None -> None
              | Some v -> Some (write v) in
      apply (List.map writes_case cases)

  end

  let rec write_rec
    : type a l. a t -> a -> MBytes.t -> int -> int = fun e ->
    let open Kind in
    let open Writer in
    match e.encoding with
    | Null -> (fun () _buf ofs -> ofs)
    | Empty -> (fun () _buf ofs -> ofs)
    | Constant _ -> (fun () _buf ofs -> ofs)
    | Ignore -> (fun () _buf ofs -> ofs)
    | Bool -> bool
    | Int8 -> int8
    | Uint8 -> uint8
    | Int16 -> int16
    | Uint16 -> uint16
    | Int31 -> int31
    | Int32 -> int32
    | Int64 -> int64
    | Float -> float
    | Bytes (`Fixed n) -> fixed_kind_bytes n
    | String (`Fixed n) -> fixed_kind_string n
    | Bytes `Variable -> variable_length_bytes
    | String `Variable -> variable_length_string
    | Array t -> array (write_rec t)
    | List t -> list (write_rec t)
    | String_enum (kind, l) -> begin
        fun v ->
          try
            let l = List.map (fun (x,y) -> (y,x)) l in
            write_rec (make @@ String kind) (List.assoc v l)
          with Not_found -> raise No_case_matched
      end
    | Obj (Req (_, e)) -> write_rec e
    | Obj (Opt (`Dynamic, _, e)) ->
        let write = write_rec e in
        (function None -> int8 0
                | Some x -> fun buf ofs -> int8 1 buf ofs |> write x buf)
    | Obj (Opt (`Variable, _, e)) ->
        let write = write_rec e in
        (function None -> fun _buf ofs -> ofs
                | Some x -> write x)
    | Obj (Dft (_, e, _)) -> write_rec e
    | Objs (_, e1, e2) ->
        objs (write_rec e1) (write_rec e2)
    | Tup e -> write_rec e
    | Tups (_, e1, e2) ->
        objs (write_rec e1) (write_rec e2)
    | Conv { encoding = e; proj } -> conv proj (write_rec e)
    | Describe { encoding = e } -> write_rec e
    | Def { encoding = e } -> write_rec e
    | Splitted { encoding = e } -> write_rec e
    | Union (_, sz, cases) -> union { write = write_rec } sz cases
    | Mu (_, _, self) -> fun v buf ofs -> write_rec (self e) v buf ofs
    | Dynamic_size e ->
        let length = length e
        and write = write_rec e in
        fun v buf ofs ->
          int32 (Int32.of_int @@ length v) buf ofs |> write v buf

  let write t v buf ofs =
    try Some (write_rec t v buf ofs)
    with _ -> None

  let to_bytes t v =
    let length = length t v in
    let bytes = MBytes.create length in
    let ofs = write_rec t v bytes 0 in
    assert(ofs = length);
    bytes

  (** Reader *)

  module Reader = struct

    let int8 buf ofs _len =
      ofs + Size.int8, MBytes.get_int8 buf ofs

    let uint8 buf ofs _len =
      ofs + Size.uint8, MBytes.get_uint8 buf ofs

    let char buf ofs len =
      let ofs, v = int8 buf ofs len in
      ofs, Char.chr v

    let bool buf ofs len =
      let ofs, v = int8 buf ofs len in
      ofs, v <> 0

    let int16 buf ofs _len =
      ofs + Size.int16, MBytes.get_int16 buf ofs

    let uint16 buf ofs _len =
      ofs + Size.uint16, MBytes.get_uint16 buf ofs

    let int31 buf ofs _len =
      ofs + Size.int31, Int32.to_int (MBytes.get_int32 buf ofs)

    let int32 buf ofs _len =
      ofs + Size.int32, MBytes.get_int32 buf ofs

    let int64 buf ofs _len =
      ofs + Size.int64, MBytes.get_int64 buf ofs

    let float buf ofs _len =
      ofs + Size.float, MBytes.get_float buf ofs

    let int_of_int32 i =
      let i' = Int32.to_int i in
      let i'' = Int32.of_int i' in
      if i'' = i then
        i'
      else
        invalid_arg "int_of_int32 overflow"

    let fixed_length_bytes length buf ofs _len =
      let s = MBytes.sub buf ofs length in
      ofs + length, s

    let fixed_length_string length buf ofs _len =
      let s = MBytes.substring buf ofs length in
      ofs + length, s

    let seq r1 r2 buf ofs len =
      let ofs', v1 = r1 buf ofs len in
      let ofs'', v2 = r2 buf ofs' (len - (ofs' - ofs)) in
      ofs'', (v1, v2)

    let varseq r e1 e2 buf ofs len =
      let k1 = classify e1
      and k2 = classify e2 in
      match k1, k2 with
      | (`Dynamic | `Fixed _), `Variable ->
          let ofs', v1 = r.read e1 buf ofs len in
          let ofs'', v2 = r.read e2 buf ofs' (len - (ofs' - ofs)) in
          ofs'', (v1, v2)
      | `Variable, `Fixed n ->
          let ofs', v1 = r.read e1 buf ofs (len - n) in
          let ofs'', v2 = r.read e2 buf ofs' n in
          ofs'', (v1, v2)
      | _ -> assert false (* Should be rejected by Kind.combine *)

    let list read buf ofs len =
      let rec loop acc ofs len =
        if len <= 0
        then ofs, List.rev acc
        else
          let ofs', v = read buf ofs len in
          loop (v :: acc) ofs'  (len - (ofs' - ofs))
      in
      loop [] ofs len

    let array read buf ofs len =
      let ofs, l = list read buf ofs len in
      ofs, Array.of_list l

    let conv inj r buf ofs len =
      let ofs, v = r buf ofs len in
      ofs, inj v

    let read_tag = function
      | `Uint8 -> uint8
      | `Uint16 -> uint16

    let union r sz cases =
      let read_cases =
        Utils.filter_map
          (function
            | (Case { tag = None }) -> None
            | (Case { encoding = e ; inj ; tag = Some tag }) ->
                let read = r.read e in
                Some (tag, fun len buf ofs ->
                    let ofs, v = read len buf ofs in
                    ofs, inj v))
          cases in
      fun buf ofs len ->
        let ofs, tag = read_tag sz buf ofs len in
        try List.assoc tag read_cases buf ofs (len - tag_size sz)
        with Not_found -> raise (Unexpected_tag tag)

  end

  let rec read_rec : type a l. a t-> MBytes.t -> int -> int -> int * a = fun e ->
    let open Kind in
    let open Reader in
    match e.encoding with
    | Null -> (fun _buf ofs _len -> ofs, ())
    | Empty -> (fun _buf ofs _len -> ofs, ())
    | Constant _ -> (fun _buf ofs _len -> ofs, ())
    | Ignore -> (fun _buf ofs len -> ofs + len, ())
    | Bool -> bool
    | Int8 -> int8
    | Uint8 -> uint8
    | Int16 -> int16
    | Uint16 -> uint16
    | Int31 -> int31
    | Int32 -> int32
    | Int64 -> int64
    | Float -> float
    | Bytes (`Fixed n) -> fixed_length_bytes n
    | String (`Fixed n) -> fixed_length_string n
    | Bytes `Variable -> fun buf ofs len -> fixed_length_bytes len buf ofs len
    | String `Variable -> fun buf ofs len -> fixed_length_string len buf ofs len
    | String_enum (kind, l) -> begin
        fun buf ofs len ->
          let ofs, str = read_rec (make @@ (String kind)) buf ofs len in
          try ofs, List.assoc str l
          with Not_found -> raise (Unexpected_enum (str, List.map fst l))
      end
    | Array e -> array (read_rec e)
    | List e -> list (read_rec e)
    | Obj (Req (_, e)) -> read_rec e
    | Obj (Opt (`Dynamic, _, t)) ->
        let read = read_rec t in
        (fun buf ofs len ->
           let ofs, v = int8 buf ofs len in
           if v = 0 then ofs, None
           else let ofs, v = read buf ofs (len - Size.int8) in ofs, Some v)
    | Obj (Opt (`Variable, _, t)) ->
        let read = read_rec t in
        (fun buf ofs len ->
           if len = 0 then ofs, None
           else
             let ofs', v = read buf ofs len in
             assert (ofs' = ofs + len) ;
             ofs + len, Some v)
    | Obj (Dft (_, e, _)) -> read_rec e
    | Objs ((`Fixed _ | `Dynamic), e1, e2) ->
        seq (read_rec e1) (read_rec e2)
    | Objs (`Variable, e1, e2) ->
        varseq { read = fun t -> read_rec t } e1 e2
    | Tup e -> read_rec e
    | Tups ((`Fixed _ | `Dynamic), e1, e2) ->
        seq (read_rec e1) (read_rec e2)
    | Tups (`Variable, e1, e2) ->
        varseq { read = fun t -> read_rec t } e1 e2
    | Conv { inj ; encoding = e } -> conv inj (read_rec e)
    | Describe { encoding = e } -> read_rec e
    | Def { encoding = e } -> read_rec e
    | Splitted { encoding = e } -> read_rec e
    | Union (_, sz, cases) ->
        union { read = fun t -> read_rec t } sz cases
    | Mu (_, _, self) -> fun buf ofs len -> read_rec (self e) buf ofs len
    | Dynamic_size e ->
        let read = read_rec e in
        fun buf ofs len ->
          let ofs, sz = int32 buf ofs len in
          read buf ofs (Int32.to_int sz)

  let read t buf ofs len =
    try Some (read_rec t buf ofs len)
    with _ -> None
  let write = write
  let of_bytes ty buf =
    let len = MBytes.length buf in
    match read ty buf 0 len with
    | None -> None
    | Some (read_len, r) -> if read_len <> len then None else Some r
  let to_bytes = to_bytes

  let length = length
end
