(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open State

type t = Block_header.t * Block_hash.t list

type error += Invalid_locator of P2p.Peer_id.t * t

let encoding =
  let open Data_encoding in
  (* TODO add a [description] *)
  (obj2
     (req "current_head" (dynamic_size Block_header.encoding))
     (req "history" (dynamic_size (list Block_hash.encoding))))

let compute (b: Block.t) sz =
  let rec loop acc sz step cpt b =
    if sz = 0 then
      Lwt.return (List.rev acc)
    else
      Block.predecessor b >>= function
      | None ->
          Lwt.return (List.rev (Block.hash b :: acc))
      | Some predecessor ->
          if cpt = 0 then
            loop (Block.hash b :: acc) (sz - 1)
              (step * 2) (step * 20 - 1) predecessor
          else if cpt mod step = 0 then
            loop (Block.hash b :: acc) (sz - 1)
              step (cpt - 1) predecessor
          else
            loop acc sz step (cpt - 1) predecessor in
  Block.predecessor b >>= function
  | None -> Lwt.return (State.Block.header b, [])
  | Some p ->
      loop [] sz 1 9 p >>= fun hist ->
      Lwt.return (State.Block.header b, hist)

let estimated_length (_head, hist) =
  let rec loop acc step cpt = function
    | [] -> acc
    | _ :: hist ->
        if cpt = 0 then
          loop (acc+step) (step*2) 9 hist
        else
          loop (acc+step) step (cpt-1) hist
  in
  loop 1 1 9 hist

let fold ~f acc (head, hist) =
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

let rec known_ancestor net_state acc hist =
  match hist with
  | [] -> Lwt.return_none
  | h :: hist ->
      Block.read_opt net_state h >>= function
      | Some block -> Lwt.return (Some (block, List.rev (h :: acc)))
      | None ->
          Block.known_invalid net_state h >>= function
          | true -> Lwt.return_none
          | false -> known_ancestor net_state (h :: acc) hist

let known_ancestor net_state (head, hist) =
  let hash = Block_header.hash head in
  if Block_hash.equal hash (State.Net.faked_genesis_hash net_state) then
    State.Block.read_exn
      net_state (State.Net.genesis net_state).block >>= fun genesis ->
    Lwt.return_some (genesis, (head, []))
  else
    State.Block.read_opt net_state hash >>= function
    | Some ancestor -> Lwt.return_some (ancestor, (head, []))
    | None ->
        known_ancestor net_state [] hist >>= function
        | None -> Lwt.return_none
        | Some (ancestor, prefix) ->
            Lwt.return_some (ancestor, (head, prefix))

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

