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

let () =
  register_error_kind
    `Branch
    ~id:"nonce.too_late_revelation"
    ~title:"Too late nonce revelation"
    ~description:"Nonce revelation happens too late"
    ~pp: (fun ppf () ->
        Format.fprintf ppf "This nonce cannot be revealed anymore.")
    Data_encoding.unit
    (function Too_late_revelation -> Some () | _ -> None)
    (fun () -> Too_late_revelation) ;
  register_error_kind
    `Temporary
    ~id:"nonce.too_early_revelation"
    ~title:"Too early nonce revelation"
    ~description:"Nonce revelation happens before cycle end"
    ~pp: (fun ppf () ->
        Format.fprintf ppf "This nonce should not yet be revealed")
    Data_encoding.unit
    (function Too_early_revelation -> Some () | _ -> None)
    (fun () -> Too_early_revelation) ;
  register_error_kind
    `Branch
    ~id:"nonce.previously_revealed"
    ~title:"Previously revealed nonce"
    ~description:"Duplicated revelation for a nonce."
    ~pp: (fun ppf () ->
        Format.fprintf ppf "This nonce was previously revealed")
    Data_encoding.unit
    (function Previously_revealed_nonce -> Some () | _ -> None)
    (fun () -> Previously_revealed_nonce) ;
  register_error_kind
    `Branch
    ~id:"nonce.unexpected"
    ~title:"Unexpected nonce"
    ~description:"The provided nonce is inconsistent with the commit nonce hash."
    ~pp: (fun ppf () ->
        Format.fprintf ppf "This nonce revelation is invalid (inconsistent with the commited hash)")
    Data_encoding.unit
    (function Unexpected_nonce -> Some () | _ -> None)
    (fun () -> Unexpected_nonce)

let get_unrevealed ctxt level =
  let revealed_cycle =
    let cur_level = Level_storage.current ctxt in
    match Cycle_repr.pred cur_level.cycle with
    | None -> Cycle_repr.root
    | Some min_cycle -> min_cycle in
  if Cycle_repr.(revealed_cycle < level.Level_repr.cycle) then
    fail Too_early_revelation
  else if Cycle_repr.(level.Level_repr.cycle < revealed_cycle) then
    fail Too_late_revelation
  else
    Storage.Seed.Nonce.get ctxt level >>=? function
    | Revealed _ -> fail Previously_revealed_nonce
    | Unrevealed status -> return status

let record_hash ctxt unrevealed =
  let level = Level_storage.current ctxt in
  Storage.Seed.Nonce.init ctxt level (Unrevealed unrevealed)

let reveal ctxt level nonce =
  get_unrevealed ctxt level >>=? fun unrevealed ->
  fail_unless
    (Seed_repr.check_hash nonce unrevealed.nonce_hash)
    Unexpected_nonce >>=? fun () ->
  Storage.Seed.Nonce.set ctxt level (Revealed nonce) >>=? fun ctxt ->
  return ctxt

type unrevealed = Storage.Seed.unrevealed_nonce = {
  nonce_hash: Nonce_hash.t ;
  delegate: Signature.Public_key_hash.t ;
  deposit: Tez_repr.t ;
  rewards: Tez_repr.t ;
  fees: Tez_repr.t ;
}

type status = Storage.Seed.nonce_status =
  | Unrevealed of unrevealed
  | Revealed of Seed_repr.nonce

let get = Storage.Seed.Nonce.get

let of_bytes = Seed_repr.make_nonce
let hash = Seed_repr.hash
let check_hash = Seed_repr.check_hash
