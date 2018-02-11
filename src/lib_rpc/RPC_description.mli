(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Error_monad

include (module type of struct include Resto.Description end)

val describe:
  #RPC_context.simple ->
  ?recurse:bool ->
  string list ->
  Json_schema.schema directory tzresult Lwt.t

