(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Error_monad

val create :
  ?close_on_exec:bool ->
  ?unlink_on_exit:bool ->
  string -> unit tzresult Lwt.t

val blocking_create :
  ?timeout:float ->
  ?close_on_exec:bool ->
  ?unlink_on_exit:bool ->
  string -> unit tzresult Lwt.t

val is_locked : string -> bool tzresult Lwt.t
val get_pid : string -> int tzresult Lwt.t
