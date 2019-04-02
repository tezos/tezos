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

(* This is the genesis protocol: initialise the state *)
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
      return ctxt
  | Alpha_002 ->
      Storage.Delegates.fold ctxt ~init:(Ok ctxt) ~f:begin fun delegate ctxt ->
        Lwt.return ctxt >>=? fun ctxt ->
        Storage.Contract.Inactive_delegate.mem ctxt
          (Contract_repr.implicit_contract delegate) >>= fun inactive ->
        if inactive then
          return ctxt
        else
          Storage.Roll.Delegate_roll_list.get_option ctxt delegate >>=? function
          | None ->
              return ctxt
          | Some _ ->
              Storage.Active_delegates_with_rolls.add ctxt delegate >>= fun ctxt ->
              return ctxt
      end >>=? fun ctxt ->
      let { Level_repr.cycle = current_cycle } = Raw_context.current_level ctxt in
      let { Constants_repr.preserved_cycles } = Raw_context.constants ctxt in
      let first_cycle =
        match Cycle_repr.sub current_cycle preserved_cycles with
        | None -> Cycle_repr.root
        | Some first_cycle -> first_cycle in
      Storage.Delegates.fold ctxt ~init:(Ok ctxt) ~f:begin fun delegate ctxt ->
        Lwt.return ctxt >>=? fun ctxt ->
        let rec loop ctxt cycle =
          if Cycle_repr.(cycle > current_cycle) then
            return ctxt
          else
            Delegate_storage.has_frozen_balance ctxt delegate cycle >>=? fun has_frozen_balance ->
            begin
              if has_frozen_balance then
                Storage.Delegates_with_frozen_balance.add (ctxt, cycle) delegate
              else
                Lwt.return ctxt
            end >>= fun ctxt ->
            loop ctxt (Cycle_repr.succ cycle) in
        loop ctxt first_cycle
      end >>=? fun ctxt ->
      return ctxt

let prepare ctxt ~level ~timestamp ~fitness =
  Raw_context.prepare ~level ~timestamp ~fitness ctxt
