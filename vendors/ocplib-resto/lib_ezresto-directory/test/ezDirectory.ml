(**************************************************************************)
(*  ocplib-resto                                                          *)
(*  Copyright (C) 2016, OCamlPro.                                         *)
(*                                                                        *)
(*    All rights reserved.  This file is distributed under the terms      *)
(*    of the GNU Lesser General Public License version 2.1, with the      *)
(*    special exception on linking described in the file LICENSE.         *)
(*                                                                        *)
(**************************************************************************)

open EzServices
include EzResto_directory

let rec repeat i json =
  if i <= 0 then []
  else json :: repeat (i-1) json

let dir = empty let dir =
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
    (fun (((), i),j) () () -> Lwt.return (`Ok (i+ int_of_float j)))
let dir =
  register_describe_directory_service
    dir describe_service
