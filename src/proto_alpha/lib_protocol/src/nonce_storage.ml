(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type t = Seed_repr.nonce
type nonce = t
let encoding = Seed_repr.nonce_encoding

type error +=
  | Too_late_revelation
  | Too_early_revelation
  | Previously_revealed_nonce
  | Unexpected_nonce

let get_unrevealed c level =
  let cur_level = Level_storage.current c in
  let min_cycle =
    match Cycle_repr.pred cur_level.cycle with
    | None -> Cycle_repr.root
    | Some min_cycle -> min_cycle in
  fail_unless
    Cycle_repr.(min_cycle <= level.Level_repr.cycle)
    Too_late_revelation >>=? fun () ->
  fail_unless
    Raw_level_repr.(level.level < cur_level.level)
    Too_early_revelation >>=? fun () ->
  Storage.Seed.Nonce.get c level >>=? function
  | Revealed _ ->
      fail Previously_revealed_nonce
  | Unrevealed { nonce_hash; delegate_to_reward ; reward_amount  } ->
      return (nonce_hash, delegate_to_reward, reward_amount)

(* let get_unrevealed_hash c level = *)
(* get_unrevealed c level >>=? fun (nonce_hash, _) -> *)
(* return nonce_hash *)

let record_hash c delegate_to_reward reward_amount nonce_hash =
  let level = Level_storage.current c in
  Storage.Seed.Nonce.init c level
    (Unrevealed { nonce_hash; delegate_to_reward ; reward_amount })

let reveal c level nonce =
  get_unrevealed c level >>=? fun (nonce_hash, delegate_to_reward, reward_amount) ->
  fail_unless
    (Seed_repr.check_hash nonce nonce_hash)
    Unexpected_nonce >>=? fun () ->
  Storage.Seed.Nonce.set c level (Revealed nonce) >>=? fun c ->
  return (c, delegate_to_reward, reward_amount)

type status = Storage.Seed.nonce_status =
  | Unrevealed of {
      nonce_hash: Tezos_hash.Nonce_hash.t ;
      delegate_to_reward: Ed25519.Public_key_hash.t ;
      reward_amount: Tez_repr.t ;
    }
  | Revealed of nonce

let get c level = Storage.Seed.Nonce.get c level

let of_bytes = Seed_repr.make_nonce
let hash = Seed_repr.hash
let check_hash = Seed_repr.check_hash
