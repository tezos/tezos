(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type error +=
  | Too_late_reward_recording
  | Too_late_reward_discarding
  | Incorrect_discard

let record c delegate cycle amount =
  Storage.Rewards.Next.get c >>=? fun min_cycle ->
  fail_unless Cycle_repr.(min_cycle <= cycle)
    Too_late_reward_recording >>=? fun () ->
  Storage.Rewards.Amount.get_option (c, cycle) delegate >>=? function
  | None ->
      Storage.Rewards.Amount.init (c, cycle) delegate amount
  | Some previous_amount ->
      Lwt.return Tez_repr.(previous_amount +? amount) >>=? fun amount ->
      Storage.Rewards.Amount.set (c, cycle) delegate amount

let discard c delegate cycle amount =
  Storage.Rewards.Next.get c >>=? fun min_cycle ->
  fail_unless Cycle_repr.(min_cycle <= cycle)
    Too_late_reward_discarding >>=? fun () ->
  Storage.Rewards.Amount.get_option (c, cycle) delegate >>=? function
  | None ->
      fail Incorrect_discard
  | Some previous_amount ->
      match Tez_repr.(previous_amount -? amount) with
      | Ok amount ->
          if Tez_repr.(amount = zero) then
            Storage.Rewards.Amount.remove (c, cycle) delegate >>= fun ctxt ->
            return ctxt
          else
            Storage.Rewards.Amount.set (c, cycle) delegate amount
      | Error _ ->
          fail Incorrect_discard

let pay_rewards_for_cycle c cycle =
  Storage.Rewards.Amount.fold (c, cycle) ~init:(Ok c)
    ~f:(fun delegate amount c ->
        match c with
        | Error _ -> Lwt.return c
        | Ok c ->
            Contract_storage.credit c
              (Contract_repr.default_contract delegate)
              amount) >>=? fun c ->
  Storage.Rewards.Amount.clear (c, cycle) >>= fun c ->
  return c

let pay_due_rewards c =
  let timestamp = Raw_context.current_timestamp c in
  let rec loop c cycle =
    Storage.Rewards.Date.get_option c cycle >>=? function
    | None ->
        Storage.Rewards.Next.set c cycle
    | Some reward_time ->
        if Time_repr.(reward_time > timestamp)
        then
          Storage.Rewards.Next.set c cycle
        else
          pay_rewards_for_cycle c cycle >>=? fun c ->
          loop c (Cycle_repr.succ cycle) in
  Storage.Rewards.Next.get c >>=? fun cycle ->
  loop c cycle

let set_reward_time_for_cycle = Storage.Rewards.Date.init

let init c =
  Storage.Rewards.Next.init c Cycle_repr.root
