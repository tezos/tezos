(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Proto_alpha
open Alpha_context

type operation = {
  hash: Operation_hash.t ;
  content: Operation.packed option ;
}

type valid_endorsement = {
  hash: Operation_hash.t ;
  source: public_key_hash ;
  block: Block_hash.t ;
  slots: int list ;
}

val monitor_endorsement:
  #Proto_alpha.rpc_context ->
  valid_endorsement tzresult Lwt_stream.t tzresult Lwt.t

