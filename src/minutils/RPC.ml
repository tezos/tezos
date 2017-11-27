(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

module Data = struct
  type 'a t = 'a Data_encoding.t
  type schema = Data_encoding.json_schema
  let unit = Data_encoding.empty
  let schema = Data_encoding.Json.schema
  module StringMap = Map.Make(String)

  let arg_encoding =
    let open Data_encoding in
    conv
      (fun {Resto.Arg.name; descr} -> (name, descr))
      (fun (name, descr) -> {name; descr})
      (obj2 (req "name" string) (opt "descr" string))

  open Resto.Description

  let meth_encoding =
    Data_encoding.string_enum
      [ "GET", `GET ;
        "POST", `POST ;
        "DELETE", `DELETE ;
        "PUT", `PUT ;
        "PATCH", `PATCH ]

  let path_item_encoding =
    let open Data_encoding in
    union [
      case ~tag:0 string
        (function PStatic s -> Some s | _ -> None)
        (fun s -> PStatic s) ;
      case ~tag:1 arg_encoding
        (function PDynamic s -> Some s | _ -> None)
        (fun s -> PDynamic s) ;
    ]

  let query_item_encoding =
    let open Data_encoding in
    conv
      (fun {name ; description} -> (name, description))
      (fun (name, description) -> {name ; description})
      (obj2 (req "name" string) (opt "description" string))

  let service_descr_encoding =
    let open Data_encoding in
    conv
      (fun { meth ; path ; description ; query ; input ; output ; error } ->
         (meth, path, description, query, input, output, error))
      (fun (meth, path, description, query, input, output, error) ->
         { meth ; path ; description ; query ; input ; output ; error })
      (obj7
         (req "meth" meth_encoding)
         (req "path" (list path_item_encoding))
         (opt "description" string)
         (req "query" (list query_item_encoding))
         (opt "input" json_schema)
         (req "output" json_schema)
         (req "erro" json_schema))

  let directory_descr_encoding =
    let open Data_encoding in
    mu "service_tree" @@ fun directory_descr_encoding ->
    let static_subdirectories_descr_encoding =
      union [
        case ~tag:0 (obj1 (req  "suffixes"
                      (list (obj2 (req "name" string)
                               (req "tree" directory_descr_encoding)))))
          (function Suffixes map ->
             Some (StringMap.bindings map) | _ -> None)
          (fun m ->
             let add acc (n,t) =  StringMap.add n t acc in
             Suffixes (List.fold_left add StringMap.empty m)) ;
        case ~tag:1 (obj1 (req "dynamic_dispatch"
                      (obj2
                         (req "arg" arg_encoding)
                         (req "tree" directory_descr_encoding))))
          (function Arg (ty, tree) -> Some (ty, tree) | _ -> None)
          (fun (ty, tree) -> Arg (ty, tree))
      ] in

    let static_directory_descr_encoding =
      conv
        (fun { services ; subdirs } ->
           let find s =
             try Some (Resto.MethMap.find s services) with Not_found -> None in
           (find `GET, find `POST, find `DELETE,
            find `PUT, find `PATCH, subdirs))
        (fun (get, post, delete, put, patch, subdirs) ->
           let add meth s services =
             match s with
             | None -> services
             | Some s -> Resto.MethMap.add meth s services in
           let services =
             Resto.MethMap.empty
             |> add `GET get
             |> add `POST post
             |> add `DELETE delete
             |> add `PUT put
             |> add `PATCH patch in
           { services ; subdirs })
        (obj6
           (opt "get_service" service_descr_encoding)
           (opt "post_service" service_descr_encoding)
           (opt "delete_service" service_descr_encoding)
           (opt "put_service" service_descr_encoding)
           (opt "patch_service" service_descr_encoding)
           (opt "subdirs" static_subdirectories_descr_encoding)) in
    union [
      case ~tag:0 (obj1 (req "static" static_directory_descr_encoding))
        (function Static descr -> Some descr | _ -> None)
        (fun descr -> Static descr) ;
      case ~tag:1 (obj1 (req "dynamic" (option string)))
        (function Dynamic descr -> Some descr | _ -> None)
        (fun descr -> Dynamic descr) ;
    ]

  let description_request_encoding =
    let open Data_encoding in
    conv
      (fun { recurse } -> recurse)
      (function recurse -> { recurse })
      (obj1 (dft "recursive" bool false))

  let description_answer_encoding = directory_descr_encoding

end

include Resto
include RestoDirectory
module Directory = RestoDirectory.MakeDirectory(Data)
module Service = Directory.Service


(* Compatibility layer, to be removed ASAP. *)

type 'a directory = 'a Directory.t
type ('prefix, 'params, 'input, 'output) service =
  ([ `POST ], 'prefix, 'params, unit, 'input, 'output, unit) Service.t

let service ?description ~input ~output path =
  Service.post_service
    ?description
    ~query: Query.empty
    ~input
    ~output
    ~error: Data_encoding.null
    path

type directory_descr = Data_encoding.json_schema Description.directory

let empty = Directory.empty
let register d s f = Directory.register d s (fun p () i -> f p i)

open Directory.Curry
let register0 root s f = register root s (curry Z f)
let register1 root s f = register root s (curry (S Z) f)
let register2 root s f = register root s (curry (S (S Z)) f)
(* let register3 root s f = register root s (curry (S (S (S Z))) f) *)
(* let register4 root s f = register root s (curry (S (S (S (S Z)))) f) *)
(* let register5 root s f = register root s (curry (S (S (S (S (S Z))))) f) *)

let register_dynamic_directory1 =
  Directory.register_dynamic_directory1

let forge_request (type i) (service: (_,_,_,_,i,_,_) Service.t) params body =
  let { Service.meth ; path } =
    Service.forge_request service params () in
  let json =
    match Service.input_encoding service with
    | Service.No_input -> assert false (* TODO *)
    | Service.Input input -> Data_encoding.Json.construct input body in
  meth, path, json
