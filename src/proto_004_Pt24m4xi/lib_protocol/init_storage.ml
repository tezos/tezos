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

let invoice ctxt pkh amount =
  let amount = Tez_repr.of_mutez_exn (Int64.(mul 1_000_000L (of_int amount))) in
  let recipient =
    Contract_repr.implicit_contract
      (Signature.Public_key_hash.of_b58check_exn pkh) in
  Contract_storage.credit ctxt recipient amount >>=? fun ctxt ->
  return ctxt

let prepare_first_block ctxt ~typecheck ~level ~timestamp ~fitness =
  Raw_context.prepare_first_block
    ~level ~timestamp ~fitness ctxt >>=? fun (previous_protocol, ctxt) ->
  match previous_protocol with
  | Genesis param ->
      Commitment_storage.init ctxt param.commitments >>=? fun ctxt ->
      Roll_storage.init ctxt >>=? fun ctxt ->
      Seed_storage.init ctxt >>=? fun ctxt ->
      Contract_storage.init ctxt >>=? fun ctxt ->
      Bootstrap_storage.init ctxt
        ~typecheck
        ?ramp_up_cycles:param.security_deposit_ramp_up_cycles
        ?no_reward_cycles:param.no_reward_cycles
        param.bootstrap_accounts
        param.bootstrap_contracts >>=? fun ctxt ->
      Roll_storage.init_first_cycles ctxt >>=? fun ctxt ->
      Vote_storage.init ctxt >>=? fun ctxt ->
      Storage.Last_block_priority.init ctxt 0 >>=? fun ctxt ->
      Vote_storage.freeze_listings ctxt >>=? fun ctxt ->
      return ctxt
  | Alpha_003 ->
      Contract_storage.reindex_contract ctxt >>=? fun ctxt ->
      (* Reallocated rolls to delegates, since rolls are now 8k tez. *)
      Roll_storage.update_tokens_per_roll ctxt
        Constants_repr.default.tokens_per_roll >>=? fun ctxt ->
      (* Update hard gas limits to the new defaults. *)
      Raw_context.patch_constants ctxt begin fun constants ->
        { constants with
          hard_gas_limit_per_operation = Constants_repr.default.hard_gas_limit_per_operation ;
          hard_gas_limit_per_block = Constants_repr.default.hard_gas_limit_per_block }
      end >>= fun ctxt ->
      (* Invoice example: credit the author(s) of this protocol with 100 tez.*)
      invoice ctxt
        "tz1iSQEcaGpUn6EW5uAy3XhPiNg7BHMnRSXi"
        100 >>=? fun ctxt ->
      return ctxt

let prepare ctxt ~level ~timestamp ~fitness =
  Raw_context.prepare ~level ~timestamp ~fitness ctxt
