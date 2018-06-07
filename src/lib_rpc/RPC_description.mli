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
  RPC_encoding.schema directory tzresult Lwt.t

