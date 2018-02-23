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
  | Revealed _ -> fail Previously_revealed_nonce
  | Unrevealed status -> return status

let record_hash c unrevealed =
  let level = Level_storage.current c in
  Storage.Seed.Nonce.init c level (Unrevealed unrevealed)

let reveal c level nonce =
  get_unrevealed c level >>=? fun unrevealed ->
  fail_unless
    (Seed_repr.check_hash nonce unrevealed.nonce_hash)
    Unexpected_nonce >>=? fun () ->
  Storage.Seed.Nonce.set c level (Revealed nonce) >>=? fun c ->
  return c

type unrevealed = Storage.Seed.unrevealed_nonce = {
  nonce_hash: Nonce_hash.t ;
  delegate: Ed25519.Public_key_hash.t ;
  bond: Tez_repr.t ;
  rewards: Tez_repr.t ;
  fees: Tez_repr.t ;
}

type status = Storage.Seed.nonce_status =
  | Unrevealed of unrevealed
  | Revealed of Seed_repr.nonce

let get c level = Storage.Seed.Nonce.get c level

let of_bytes = Seed_repr.make_nonce
let hash = Seed_repr.hash
let check_hash = Seed_repr.check_hash
