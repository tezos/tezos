(**************************************************************************)
(*  ocplib-resto                                                          *)
(*  Copyright (C) 2016, OCamlPro.                                         *)
(*                                                                        *)
(*    All rights reserved.  This file is distributed under the terms      *)
(*    of the GNU Lesser General Public License version 2.1, with the      *)
(*    special exception on linking described in the file LICENSE.         *)
(*                                                                        *)
(**************************************************************************)

open Services

include Resto_directory.Make(Resto_json.Encoding)

let rec repeat i json =
  if i <= 0 then []
  else json :: repeat (i-1) json

let dir = empty
let dir =
  register1 dir repeat_service
    (fun i () json -> Lwt.return (`Ok (`A (repeat i json))))
let dir =
  register1 dir add_service
    (fun i () j -> Lwt.return (`Ok (i+j)))
let dir =
  register2 dir alternate_add_service
    (fun i j () () -> Lwt.return (`Ok (float_of_int i+.j)))
let dir =
  register dir alternate_add_service'
    (fun (((),i),j) () () -> Lwt.return (`Ok (i+ int_of_float j)))
let dir =
  register dir dummy_service
    (fun ((((((((),_a), _b), _c), _d), _e), _f), _g) () () -> Lwt.return (`Ok ()))

let dir =
  register_dynamic_directory1 dir prefix_dir1
    (fun _ ->
       let prefixed_dir = empty in
       let prefixed_dir =
         register2 prefixed_dir minus_service
           (fun i j () () -> Lwt.return (`Ok (i -. float_of_int j))) in
       Lwt.return prefixed_dir)

let dir =
  register_describe_directory_service
    dir describe_service
