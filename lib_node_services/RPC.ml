(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

let meth_encoding =
  Data_encoding.string_enum
    [ "GET", `GET ;
      "POST", `POST ;
      "DELETE", `DELETE ;
      "PUT", `PUT ;
      "PATCH", `PATCH ]

module Data = struct
  type 'a t = 'a Data_encoding.t
  type schema = Data_encoding.json_schema
  let unit = Data_encoding.empty
  let untyped = Data_encoding.(obj1 (req "untyped" string))
  let conv f g t = Data_encoding.conv ~schema:(Data_encoding.Json.schema t) f g t
  let schema = Data_encoding.Json.schema

  module StringMap = Resto.StringMap

  let arg_encoding =
    let open Data_encoding in
    conv
      (fun {Resto.Arg.name; descr} -> (name, descr))
      (fun (name, descr) -> {name; descr})
      (obj2 (req "name" string) (opt "descr" string))

  open Resto.Description

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

  let query_kind_encoding =
    let open Data_encoding in
    union [
      case ~tag:0 (obj1 (req "single" arg_encoding))
        (function Single s -> Some s | _ -> None)
        (fun s -> Single s) ;
      case ~tag:1 (obj1 (req "optional" arg_encoding))
        (function Optional s -> Some s | _ -> None)
        (fun s -> Optional s) ;
      case ~tag:2 (obj1 (req "flag" unit))
        (function Flag -> Some () | _ -> None)
        (fun () -> Flag) ;
      case ~tag:3 (obj1 (req "multi" arg_encoding))
        (function Multi s -> Some s | _ -> None)
        (fun s -> Multi s) ;
    ]

  let query_item_encoding =
    let open Data_encoding in
    conv
      (fun { name ; description ; kind } -> (name, description, kind))
      (fun (name, description, kind) -> { name ; description ; kind })
      (obj3
         (req "name" string)
         (opt "description" string)
         (req "kind" query_kind_encoding))

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
         (req "query" (list (dynamic_size query_item_encoding)))
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
module Service = Resto.MakeService(Data)
