(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type operation = {
  hash: Operation_hash.t ;
  content: Operation.t option ;
}

val monitor:
  Client_rpcs.config ->
  ?contents:bool -> ?check:bool -> unit ->
  operation list tzresult Lwt_stream.t tzresult Lwt.t

type valid_endorsement = {
  hash: Operation_hash.t ;
  source: public_key_hash ;
  block: Block_hash.t ;
  slots: int list ;
}

(*
val filter_valid_endorsement:
  Client_rpcs.config ->
  operation -> valid_endorsement option Lwt.t
*)

val monitor_endorsement:
  Client_rpcs.config ->
  valid_endorsement tzresult Lwt_stream.t tzresult Lwt.t

