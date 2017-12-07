(**************************************************************************)
(*  ocplib-resto                                                          *)
(*  Copyright (C) 2016, OCamlPro.                                         *)
(*                                                                        *)
(*    All rights reserved.  This file is distributed under the terms      *)
(*    of the GNU Lesser General Public License version 2.1, with the      *)
(*    special exception on linking described in the file LICENSE.         *)
(*                                                                        *)
(**************************************************************************)

module Make (Encoding : Resto.ENCODING) = struct

  open Cohttp

  type t = {
    name: Cohttp.Accept.media_range ;
    q: int option ;
    pp: 'a. 'a Encoding.t -> Format.formatter -> string -> unit ;
    construct: 'a. 'a Encoding.t -> 'a -> string ;
    destruct: 'a. 'a Encoding.t -> string -> ('a, string) result ;
  }

  let name_of_media_type = function
    | Accept.AnyMedia            -> "*/*"
    | AnyMediaSubtype type_      -> type_ ^ "/*"
    | MediaType (type_, subtype) -> type_ ^ "/" ^ subtype

  let name { name ; _ } = name_of_media_type name

  let rec has_complete_media = function
    | [] -> false
    | { name = MediaType _ ; _ } :: _ -> true
    | _ :: l -> has_complete_media l

  let rec first_complete_media = function
    | [] -> None
    | { name = MediaType (l,r) ; _ } as m :: _ -> Some ((l, r), m)
    | _ :: l -> first_complete_media l

  let matching_media (type_, subtype) = function
    | Accept.AnyMedia       -> true
    | AnyMediaSubtype type_'       -> type_' = type_
    | MediaType (type_', subtype') -> type_' = type_ && subtype' = subtype

  let rec find_media received = function
    | [] -> None
    | { name ; _ } as media :: _ when matching_media received name ->
        Some media
    | _ :: mts -> find_media received mts

  (* Inspired from ocaml-webmachine *)

  let media_match (_, (range, _)) media =
    match media.name with
    | AnyMedia | AnyMediaSubtype _ -> false
    | MediaType (type_, subtype) ->
        let open Accept in
        match range with
        | AnyMedia                     -> true
        | AnyMediaSubtype type_'       -> type_' = type_
        | MediaType (type_', subtype') -> type_' = type_ && subtype' = subtype

  let resolve_accept_header provided header =
    let ranges = Accept.(media_ranges header |> qsort) in
    let rec loop = function
      | [] -> None
      | r :: rs ->
          try
            let media = List.find (media_match r) provided in
            Some (name_of_media_type media.name, media)
          with Not_found -> loop rs
    in
    loop ranges

  let accept_header ranges =
    let ranges =
      List.map (fun r ->
          let q = match r.q with None -> 1000 | Some i -> i in
          (q, (r.name, []))) ranges in
    (Accept.string_of_media_ranges ranges)

  let acceptable_encoding ranges =
    String.concat ", "
      (List.map (fun f -> name_of_media_type f.name) ranges)

end
