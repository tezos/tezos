(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

val run_https:
  #Client_context.io_wallet ->
  host:string -> port:int -> cert:string -> key:string ->
  ?magic_bytes: int list ->
  require_auth: bool ->
  'a tzresult Lwt.t

val run_http:
  #Client_context.io_wallet ->
  host:string -> port:int ->
  ?magic_bytes: int list ->
  require_auth: bool ->
  'a tzresult Lwt.t
