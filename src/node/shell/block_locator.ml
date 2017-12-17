(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open State

type t = Block_store_locator.t

(** Non private version of Block_store_locator.t for coercions *)
type locator = Block_header.t * Block_hash.t list

let encoding = Block_store_locator.encoding

type error += Invalid_locator of P2p.Peer_id.t * t

let estimated_length locator =
  let (_head, hist) = (locator : t :> locator) in
  let rec loop acc step cpt = function
    | [] -> acc
    | _ :: hist ->
        if cpt = 0 then
          loop (acc+step) (step*2) 9 hist
        else
          loop (acc+step) step (cpt-1) hist
  in
  loop 1 1 9 hist

let fold ~f acc locator =
  let (head, hist) = (locator : t :> locator) in
  let rec loop step cpt acc = function
    | [] | [_] -> acc
    | block :: (pred :: rem as hist) ->
        let step, cpt =
          if cpt = 0 then
            2 * step, 9
          else
            step, cpt - 1 in
        let acc = f acc ~block ~pred ~step ~strict_step:(rem <> []) in
        loop step cpt acc hist
  in
  loop 1 10 acc (Block_header.hash head :: hist)

type step = {
  block: Block_hash.t ;
  predecessor: Block_hash.t ;
  step: int ;
  strict_step: bool ;
}

let to_steps locator =
  fold
    ~f:begin fun acc ~block ~pred ~step ~strict_step -> {
        block ; predecessor = pred ; step ; strict_step ;
      } :: acc
    end
    [] locator

let block_validity net_state block : Block_store_locator.validity Lwt.t =
  Block.known net_state block >>= function
  | false ->
      if Block_hash.equal block (State.Net.faked_genesis_hash net_state) then
        Lwt.return Block_store_locator.Known_valid
      else
        Lwt.return Block_store_locator.Unknown
  | true ->
      Block.known_invalid net_state block >>= function
      | true ->
          Lwt.return Block_store_locator.Known_invalid
      | false ->
          Lwt.return Block_store_locator.Known_valid

let known_ancestor net_state locator =
  Block_store_locator.unknown_prefix (block_validity net_state) locator
  >>= function
  | None -> Lwt.return_none
  | Some (tail, locator) ->
      if Block_hash.equal tail (State.Net.faked_genesis_hash net_state) then
        Block.read_exn
          net_state (State.Net.genesis net_state).block >>= fun genesis ->
        Lwt.return_some (genesis, locator)
      else
        Block.read_exn net_state tail >>= fun block ->
        Lwt.return_some (block, locator)

let find_new net_state locator sz =
  let rec path sz acc h =
    if sz <= 0 then Lwt.return (List.rev acc)
    else
      read_chain_store net_state begin fun chain_store _data ->
        Store.Chain.In_chain.read_opt (chain_store, h)
      end >>= function
      | None -> Lwt.return (List.rev acc)
      | Some s -> path (sz-1) (s :: acc) s in
  known_ancestor net_state locator >>= function
  | None -> Lwt.return_nil
  | Some (known, _) ->
      Chain.head net_state >>= fun head ->
      Chain_traversal.common_ancestor known head >>= fun ancestor ->
      path sz [] (Block.hash ancestor)

