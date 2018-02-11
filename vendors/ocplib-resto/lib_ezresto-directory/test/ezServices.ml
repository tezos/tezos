(**************************************************************************)
(*  ocplib-resto                                                          *)
(*  Copyright (C) 2016, OCamlPro.                                         *)
(*                                                                        *)
(*    All rights reserved.  This file is distributed under the terms      *)
(*    of the GNU Lesser General Public License version 2.1, with the      *)
(*    special exception on linking described in the file LICENSE.         *)
(*                                                                        *)
(**************************************************************************)

open EzResto

(** Shared part *)

let repeat_service =
  post_service
    ~query:Query.empty
    ~input:Json_encoding.any_ezjson_value
    ~output:Json_encoding.any_ezjson_value
    ~error:Json_encoding.empty
    Path.(root / "foo" /: Arg.int / "repeat")

let add_service =
  post_service
    ~query:Query.empty
    ~input:Json_encoding.int
    ~output:Json_encoding.int
    ~error:Json_encoding.empty
    Path.(root / "foo" /: Arg.int / "add")

let alternate_add_service =
  get_service
    ~query:Query.empty
    ~output:Json_encoding.float
    ~error:Json_encoding.empty
    Path.(root / "bar" /: Arg.int /: Arg.float / "add")

let alternate_add_service' =
  post_service
    ~query:Query.empty
    ~input:Json_encoding.null
    ~output:Json_encoding.int
    ~error:Json_encoding.empty
    Path.(root / "bar" /: Arg.int /: Arg.float / "add")

let minus_service r =
  post_service
    ~query:Query.empty
    ~input:Json_encoding.null
    ~output:Json_encoding.float
    ~error:Json_encoding.empty
    Path.(r /: Arg.int / "minus")

let describe_service =
  description_service Path.(root / "describe")
