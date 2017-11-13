(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Lwt.Infix

module Arg = Resto.Arg
module Path = Resto.Path

(* Services *)

(* HTTP methods as defined in Cohttp.Code *)
type meth = [
  | `GET
  | `POST
  | `HEAD
  | `DELETE
  | `PATCH
  | `PUT
  | `OPTIONS
  | `TRACE
  | `CONNECT
  | `Other of string
]

type ('prefix, 'params, 'input, 'output) service =
  meth * ('prefix, 'params, 'input, 'output) Resto.service

(* The default HTTP method for services *)
let default_meth = `POST

(* Commonly used REST HTTP methods *)
let rest_meths = [`GET; `POST; `HEAD; `DELETE; `PATCH; `PUT; `OPTIONS]

let string_of_method = function
  | `GET -> "GET"
  | `POST -> "POST"
  | `HEAD -> "HEAD"
  | `DELETE -> "DELETE"
  | `PATCH -> "PATCH"
  | `PUT -> "PUT"
  | `OPTIONS -> "OPTIONS"
  | `TRACE -> "TRACE"
  | `CONNECT -> "CONNECT"
  | `Other s -> s

let service ?(meth = default_meth) ?description ~input ~output path =
  (meth,
   Resto.service
     ?description
     ~input:(Data_encoding.Json.convert input)
     ~output:(Data_encoding.Json.convert output)
     path)

(* REST services *)

(* GET service: no input body *)
let get_service ?description ~output path =
  service ~meth:`GET ?description
    ~input:Data_encoding.empty ~output
    path

(* HEAD service: same as GET, but without output body *)
let head_service ?description path =
  service ~meth:`HEAD ?description
    ~input:Data_encoding.empty ~output:Data_encoding.empty
    path

let post_service ?description ~input ~output path =
  service ~meth:`POST ?description ~input ~output path

let put_service ?description ~input ~output path =
  service ~meth:`PUT ?description ~input ~output path

let delete_service ?description ~input ~output path =
  service ~meth:`DELETE ?description ~input ~output path

let prefix p (meth, s) = (meth, RestoDirectory.prefix p s)

let forge_request (meth, service) params input =
  let path, arg = Resto.forge_request service params input in
  meth, path, arg

let read_answer (_meth, service) json =
  Resto.read_answer service json

module Description = struct

  include Resto.Description

  let service ?(meth = default_meth) ?description path =
    (meth, Resto.Description.service ?description path)

end

module Answer = struct

  include RestoDirectory.Answer

  let answer ?(code = 200) json = { code; body = Single json }
  let return ?code json = Lwt.return (answer ?code json)

end


type step = RestoDirectory.step =
  | Static of string
  | Dynamic of Arg.descr

type conflict = RestoDirectory.conflict =
  | CService
  | CDir
  | CBuilder
  | CCustom
  | CTypes of Arg.descr * Arg.descr
  | CType of Arg.descr * string list

exception Conflict = RestoDirectory.Conflict
exception Cannot_parse = RestoDirectory.Cannot_parse

(* Dispatch *)

type 'prefix directory = (meth * 'prefix RestoDirectory.directory) list

let empty = []

let map_dirs f dirs =
  List.map (fun (meth, dir) -> (meth, f ~meth dir)) dirs

let map f dirs =
  map_dirs (fun ~meth:_ dir -> RestoDirectory.map f dir) dirs

let prefix path dirs =
  map_dirs (fun ~meth:_ dir -> RestoDirectory.prefix path dir) dirs

let merge dirs1 dirs2 =
  let compare (meth1, _dir1) (meth2, _dir2) = compare meth1 meth2 in
  let f (meth1, dir1) (_, dir2) = (meth1, RestoDirectory.merge dir1 dir2) in
  Utils.merge_list2 ~compare ~f dirs1 dirs2

(*****************************************************************************
 * Registration
 ****************************************************************************)

(** [replace_assoc ~init ~f k l] searches for value corresponding to [k] in an
    association list, and replaces it with [f value]. If not found, a new pair
    [(k, f init)] is added to the list. *)
(* TODO: move to Utils? *)
let replace_assoc ?(finalize = List.rev) ~init ~f key l =
  let rec aux acc = function
    | [] -> finalize ((key, f init) :: acc)
    | (k, v) :: tl when k = key -> finalize ((key, f v) :: acc) @ tl
    | hd :: tl -> aux (hd :: acc) tl
  in
  aux [] l

(* Register [service] to the directory with corresponding [meth] using [reg] *)
let register dirs (meth, service) handler =
  let init = RestoDirectory.empty in
  let f dir = RestoDirectory.register dir service handler in
  replace_assoc ~init ~f meth dirs

(* Register dynamic directory *)

(* By default, the [builder] function of dynamic directory is registered for
   HTTP methods listed in [rest_meths] *)
let register_dynamic_directory
    ?(meths = rest_meths) ?descr init_dirs path builder =
  let builder' ~meth prefix =
    builder prefix >>= fun dirs ->
    Lwt.return (List.assoc meth dirs)
  in
  let init = RestoDirectory.empty in
  List.fold_left (fun dirs meth ->
      let f dir =
        RestoDirectory.register_dynamic_directory
          ?descr dir path (builder' ~meth)
      in
      replace_assoc ~init ~f meth dirs)
    init_dirs meths

(* Register custom lookup *)

type custom_lookup = RestoDirectory.custom_lookup

let register_custom_lookup ?(meth = default_meth) ?descr dirs s f =
  let init = RestoDirectory.empty in
  let f dir = RestoDirectory.register_custom_lookup ?descr dir s f in
  replace_assoc ~init ~f meth dirs

(* Register description service *)

let register_describe_directory_service dirs (meth, service) =
  let init = RestoDirectory.empty in
  let f dir = RestoDirectory.register_describe_directory_service dir service in
  replace_assoc ~init ~f meth dirs

(*****************************************************************************
 * Lookup
 ****************************************************************************)

let lookup dirs ?(meth = default_meth) args path =
  let dir = List.assoc meth dirs in
  RestoDirectory.lookup dir args path

(*****************************************************************************
 * Currying
 ****************************************************************************)

(* Service registration *)

let register0 root s f =
  register root s RestoDirectory.Internal.(curry Z f)

let register1 root s f =
  register root s RestoDirectory.Internal.(curry (S Z) f)

let register2 root s f =
  register root s RestoDirectory.Internal.(curry (S (S Z)) f)

let register3 root s f =
  register root s RestoDirectory.Internal.(curry (S (S (S Z))) f)

let register4 root s f =
  register root s RestoDirectory.Internal.(curry (S (S (S (S Z)))) f)

let register5 root s f =
  register root s RestoDirectory.Internal.(curry (S (S (S (S (S Z))))) f)

(* Dynamic directory registration *)

let register_dynamic_directory1 ?descr root s f =
  register_dynamic_directory
    ?descr root s RestoDirectory.Internal.(curry (S Z) f)

let register_dynamic_directory2 ?descr root s f =
  register_dynamic_directory
    ?descr root s RestoDirectory.Internal.(curry (S (S Z)) f)

let register_dynamic_directory3 ?descr root s f =
  register_dynamic_directory
    ?descr root s RestoDirectory.Internal.(curry (S (S (S Z))) f)

(* Custom lookup registration *)

let register_custom_lookup1 ?meth ?descr root s f =
  register_custom_lookup ?meth ?descr root s
    RestoDirectory.Internal.(curry (S Z) f)

let register_custom_lookup2 ?meth ?descr root s f =
  register_custom_lookup ?meth ?descr root s
    RestoDirectory.Internal.(curry (S (S Z)) f)

let register_custom_lookup3 ?meth ?descr root s f =
  register_custom_lookup ?meth ?descr root s
    RestoDirectory.Internal.(curry (S (S (S Z))) f)
