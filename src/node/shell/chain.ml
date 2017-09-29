(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)


open Logging.Node.State
open State

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

let find_new net_state hist sz =
  let rec common_ancestor hist =
    match hist with
    | [] -> Lwt.return (Net.genesis net_state).block
    | h :: hist ->
        mem net_state h >>= function
        | false -> common_ancestor hist
        | true -> Lwt.return h in
  let rec path sz acc h =
    if sz <= 0 then Lwt.return (List.rev acc)
    else
      read_chain_store net_state begin fun chain_store _data ->
        Store.Chain.In_chain.read_opt (chain_store, h)
      end >>= function
      | None -> Lwt.return (List.rev acc)
      | Some s -> path (sz-1) (s :: acc) s in
  common_ancestor hist >>= fun ancestor ->
  path sz [] ancestor

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
    data.current_head block >>= fun (ancestor, path) ->
  let ancestor = Block.hash ancestor in
  pop_blocks ancestor data.current_head >>= fun () ->
  Lwt_list.fold_left_s push_block ancestor path >>= fun _ ->
  Store.Chain.Current_head.store chain_store (Block.hash block)

let set_head net_state block =
  update_chain_store net_state begin fun chain_store data ->
    locked_set_head chain_store data block >>= fun () ->
    Lwt.return (Some { current_head = block  ;
                       current_reversed_mempool = [] },
                ())
  end

let test_and_set_head net_state ~old block =
  update_chain_store net_state begin fun chain_store data ->
    if not (Block.equal data.current_head old) then
      Lwt.return (None, false)
    else
      locked_set_head chain_store data block >>= fun () ->
      Lwt.return (Some { current_head = block ;
                         current_reversed_mempool = [] },
                  true)
  end

let set_reversed_mempool net_state current_reversed_mempool =
  update_chain_store net_state begin fun _chain_store data ->
    Lwt.return (Some { data with current_reversed_mempool },
                ())
  end

let mempool net_state =
  read_chain_store net_state begin fun _chain_store data ->
    Lwt.return (List.rev data.current_reversed_mempool)
  end
