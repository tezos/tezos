(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Alpha_context

val errors:
  'a #RPC_context.simple -> 'a -> Data_encoding.json_schema shell_tzresult Lwt.t

(** Returns all the constants of the protocol *)
val all:
  'a #RPC_context.simple -> 'a -> Constants.t shell_tzresult Lwt.t
