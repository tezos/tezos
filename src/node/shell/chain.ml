(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Logging.Node.State
open State

let mempool_encoding = State.mempool_encoding

let genesis net_state =
  let genesis = Net.genesis net_state in
  Block.read_exn net_state genesis.block

let known_heads net_state =
  read_chain_store net_state begin fun chain_store _data ->
    Store.Chain.Known_heads.elements chain_store
  end >>= fun hashes ->
  Lwt_list.map_p (Block.read_exn net_state) hashes

let head net_state =
  read_chain_store net_state begin fun _chain_store data ->
    Lwt.return data.current_head
  end

let mem net_state hash =
  read_chain_store net_state begin fun chain_store data ->
    if Block_hash.equal (Block.hash data.current_head) hash then
      Lwt.return true
    else
      Store.Chain.In_chain.known (chain_store, hash)
  end

type data = State.chain_data = {
  current_head: Block.t ;
  current_mempool: mempool ;
  live_blocks: Block_hash.Set.t ;
  live_operations: Operation_hash.Set.t ;
}

let data net_state =
  read_chain_store net_state begin fun _chain_store data ->
    Lwt.return data
  end

let locked_set_head chain_store data block =
  let rec pop_blocks ancestor block =
    let hash = Block.hash block in
    if Block_hash.equal hash ancestor then
      Lwt.return_unit
    else
      lwt_debug "pop_block %a" Block_hash.pp_short hash >>= fun () ->
      Store.Chain.In_chain.remove (chain_store, hash) >>= fun () ->
      Block.predecessor block >>= function
      | Some predecessor ->
          pop_blocks ancestor predecessor
      | None -> assert false (* Cannot pop the genesis... *)
  in
  let push_block pred_hash block =
    let hash = Block.hash block in
    lwt_debug "push_block %a" Block_hash.pp_short hash >>= fun () ->
    Store.Chain.In_chain.store (chain_store, pred_hash) hash >>= fun () ->
    Lwt.return hash
  in
  Chain_traversal.new_blocks
    ~from_block:data.current_head ~to_block:block >>= fun (ancestor, path) ->
  let ancestor = Block.hash ancestor in
  pop_blocks ancestor data.current_head >>= fun () ->
  Lwt_list.fold_left_s push_block ancestor path >>= fun _ ->
  Store.Chain.Current_head.store chain_store (Block.hash block) >>= fun () ->
  (* TODO more optimized updated of live_{blocks/operations} when the
     new head is a direct successor of the current head... *)
  Chain_traversal.live_blocks
    block (State.Block.max_operations_ttl block) >>= fun (live_blocks,
                                                          live_operations) ->
  Lwt.return { current_head = block  ;
               current_mempool = State.empty_mempool ;
               live_blocks ;
               live_operations ;
             }

let set_head net_state block =
  update_chain_store net_state begin fun chain_store data ->
    locked_set_head chain_store data block >>= fun new_chain_data ->
    Lwt.return (Some new_chain_data,
                data.current_head)
  end

let test_and_set_head net_state ~old block =
  update_chain_store net_state begin fun chain_store data ->
    if not (Block.equal data.current_head old) then
      Lwt.return (None, false)
    else
      locked_set_head chain_store data block >>= fun new_chain_data ->
      Lwt.return (Some new_chain_data, true)
  end

let init net_state =
  head net_state >>= fun block ->
  set_head net_state block >>= fun _ ->
  Lwt.return_unit

