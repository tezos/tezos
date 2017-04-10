(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
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
  Storage.Rewards.Amount.get_option c (delegate, cycle) >>=? function
  | None ->
      Storage.Rewards.Amount.init c (delegate, cycle) amount
  | Some previous_amount ->
      Lwt.return Tez_repr.(previous_amount +? amount) >>=? fun amount ->
      Storage.Rewards.Amount.set c (delegate, cycle) amount

let discard c delegate cycle amount =
  Storage.Rewards.Next.get c >>=? fun min_cycle ->
  fail_unless Cycle_repr.(min_cycle <= cycle)
    Too_late_reward_discarding >>=? fun () ->
  Storage.Rewards.Amount.get_option c (delegate, cycle) >>=? function
  | None ->
      fail Incorrect_discard
  | Some previous_amount ->
      match Tez_repr.(previous_amount -? amount) with
      | Ok amount ->
          if Tez_repr.(amount = zero) then
            Storage.Rewards.Amount.remove c (delegate, cycle) >>= fun ctxt ->
            return ctxt
          else
            Storage.Rewards.Amount.set c (delegate, cycle) amount
      | Error _ ->
          fail Incorrect_discard

let pay_rewards_for_cycle c cycle =
  Storage.Rewards.Amount.fold c (Ok c)
    ~f:(fun (delegate, reward_cycle) amount c ->
        match c with
        | Error _ -> Lwt.return c
        | Ok c ->
            if not Cycle_repr.(cycle = reward_cycle)
            then return c
            else
              Storage.Rewards.Amount.remove c (delegate, reward_cycle) >>= fun c ->
              Contract_storage.credit c
                (Contract_repr.default_contract delegate)
                amount)

let pay_due_rewards c =
  let timestamp = Storage.current_timestamp c in
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
