(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open State

type t = State.mempool = {
  known_valid: Operation_hash.t list ;
  pending: Operation_hash.Set.t ;
}
type mempool = t

let encoding = State.mempool_encoding
let empty = State.empty_mempool

let set net_state ~head mempool =
  update_chain_store net_state begin fun _chain_store data ->
    if Block_hash.equal head (Block.hash data.current_head) then
      Lwt.return (Some { data with current_mempool = mempool },
                  ())
    else
      Lwt.return (None, ())
  end

let get net_state =
  read_chain_store net_state begin fun _chain_store data ->
    Lwt.return (Block.header data.current_head, data.current_mempool)
  end

