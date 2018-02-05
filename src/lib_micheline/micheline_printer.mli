(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Micheline

val print_string : Format.formatter -> string -> unit

type location = { comment : string option }

type node = (location, string) Micheline.node

val print_expr : Format.formatter -> (location, string) Micheline.node -> unit
val print_expr_unwrapped : Format.formatter -> (location, string) Micheline.node -> unit

val printable :
  ?comment: (int -> string option) ->
  ('p -> string) -> 'p canonical -> (location, string) Micheline.node
