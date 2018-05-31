(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open State_logging

let mempool_encoding = Mempool.encoding

let genesis chain_state =
  let genesis = State.Chain.genesis chain_state in
  State.Block.read_exn chain_state genesis.block

let known_heads chain_state =
  State.read_chain_data chain_state begin fun chain_store _data ->
    Store.Chain_data.Known_heads.elements chain_store
  end >>= fun hashes ->
  Lwt_list.map_p (State.Block.read_exn chain_state) hashes

let head chain_state =
  State.read_chain_data chain_state begin fun _chain_store data ->
    Lwt.return data.current_head
  end

let mem chain_state hash =
  State.read_chain_data chain_state begin fun chain_store data ->
    if Block_hash.equal (State.Block.hash data.current_head) hash then
      Lwt.return true
    else
      Store.Chain_data.In_main_branch.known (chain_store, hash)
  end

type data = State.chain_data = {
  current_head: State.Block.t ;
  current_mempool: Mempool.t ;
  live_blocks: Block_hash.Set.t ;
  live_operations: Operation_hash.Set.t ;
  test_chain: Chain_id.t option ;
}

let data chain_state =
  State.read_chain_data chain_state begin fun _chain_store data ->
    Lwt.return data
  end

let locator chain_state seed =
  data chain_state >>= fun data ->
  State.compute_locator chain_state data.current_head seed

let locked_set_head chain_store data block =
  let rec pop_blocks ancestor block =
    let hash = State.Block.hash block in
    if Block_hash.equal hash ancestor then
      Lwt.return_unit
    else
      lwt_debug "pop_block %a" Block_hash.pp_short hash >>= fun () ->
      Store.Chain_data.In_main_branch.remove (chain_store, hash) >>= fun () ->
      State.Block.predecessor block >>= function
      | Some predecessor ->
          pop_blocks ancestor predecessor
      | None -> assert false (* Cannot pop the genesis... *)
  in
  let push_block pred_hash block =
    let hash = State.Block.hash block in
    lwt_debug "push_block %a" Block_hash.pp_short hash >>= fun () ->
    Store.Chain_data.In_main_branch.store
      (chain_store, pred_hash) hash >>= fun () ->
    Lwt.return hash
  in
  Chain_traversal.new_blocks
    ~from_block:data.current_head ~to_block:block >>= fun (ancestor, path) ->
  let ancestor = State.Block.hash ancestor in
  pop_blocks ancestor data.current_head >>= fun () ->
  Lwt_list.fold_left_s push_block ancestor path >>= fun _ ->
  Store.Chain_data.Current_head.store chain_store (State.Block.hash block) >>= fun () ->
  (* TODO more optimized updated of live_{blocks/operations} when the
     new head is a direct successor of the current head...
     Make sure to do the live blocks computation in `init_head`
     when this TODO is resolved. *)
  Chain_traversal.live_blocks
    block (State.Block.max_operations_ttl block) >>= fun (live_blocks,
                                                          live_operations) ->
  Lwt.return { current_head = block  ;
               current_mempool = Mempool.empty ;
               live_blocks ;
               live_operations ;
               test_chain = None ;
             }

let set_head chain_state block =
  State.update_chain_data chain_state begin fun chain_store data ->
    locked_set_head chain_store data block >>= fun new_chain_data ->
    Lwt.return (Some new_chain_data,
                data.current_head)
  end

let test_and_set_head chain_state ~old block =
  State.update_chain_data chain_state begin fun chain_store data ->
    if not (State.Block.equal data.current_head old) then
      Lwt.return (None, false)
    else
      locked_set_head chain_store data block >>= fun new_chain_data ->
      Lwt.return (Some new_chain_data, true)
  end

let init_head chain_state =
  head chain_state >>= fun block ->
  set_head chain_state block >>= fun _ ->
  Lwt.return_unit

