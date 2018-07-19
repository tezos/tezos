(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

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
    ~description:"The provided nonce is inconsistent with the committed nonce hash."
    ~pp: (fun ppf () ->
        Format.fprintf ppf "This nonce revelation is invalid (inconsistent with the committed hash)")
    Data_encoding.unit
    (function Unexpected_nonce -> Some () | _ -> None)
    (fun () -> Unexpected_nonce)

(* checks that the level of a revelation is not too early or too late wrt to the
   current context and that a nonce has not been already revealed for that level *)
let get_unrevealed ctxt level =
  let cur_level = Level_storage.current ctxt in
  match Cycle_repr.pred cur_level.cycle with
  | None -> fail Too_early_revelation (* no revelations during cycle 0 *)
  | Some revealed_cycle ->
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
