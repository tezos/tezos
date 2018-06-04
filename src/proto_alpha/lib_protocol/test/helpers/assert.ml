(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Proto_alpha

let error ~loc v f =
  match v with
  | Error err when List.exists f err ->
      return ()
  | Ok _ ->
      failwith "Unexpected successful result (%s)" loc
  | Error err ->
      failwith "@[Unexpected error (%s): %a@]" loc pp_print_error err

let proto_error ~loc v f =
  error ~loc v
    (function
      | Alpha_environment.Ecoproto_error err -> f err
      | _ -> false)

let equal ~loc (cmp : 'a -> 'a -> bool) msg pp a b  =
  if not (cmp a b) then
    failwith "@[@[[%s]@] - @[%s : %a is not equal to %a@]@]" loc msg pp a pp b
  else
    return ()

let not_equal ~loc (cmp : 'a -> 'a -> bool) msg pp a b  =
  if cmp a b then
    failwith "@[@[[%s]@] - @[%s : %a is equal to %a@]@]" loc msg pp a pp b
  else
    return ()

let equal_tez ~loc (a:Alpha_context.Tez.t) (b:Alpha_context.Tez.t) =
  let open Alpha_context in
  equal ~loc Tez.(=) "Tez aren't equal" Tez.pp a b

let not_equal_tez ~loc (a:Alpha_context.Tez.t) (b:Alpha_context.Tez.t) =
  let open Alpha_context in
  not_equal ~loc Tez.(=) "Tez are equal" Tez.pp a b

let equal_int ~loc (a:int) (b:int) =
  equal ~loc (=) "Integers aren't equal" Format.pp_print_int a b

let not_equal_int ~loc (a:int) (b:int) =
  not_equal ~loc (=) "Integers are equal" Format.pp_print_int a b

let equal_bool ~loc (a:bool) (b:bool) =
  equal ~loc (=) "Booleans aren't equal" Format.pp_print_bool a b

let not_equal_bool ~loc (a:bool) (b:bool) =
  not_equal ~loc (=) "Booleans are equal" Format.pp_print_bool a b


open Context
(* Some asserts for account operations *)

(** [balance_is b c amount] checks that the current balance of contract [c] is
    [amount].
    Default balance type is [Main], pass [~kind] with [Deposit], [Fees] or
    [Rewards] for the others. *)
let balance_is ~loc b contract ?(kind = Contract.Main) expected =
  Contract.balance b contract ~kind >>=? fun balance ->
  equal_tez ~loc balance expected

(** [balance_was_operated ~operand b c old_balance amount] checks that the
    current balance of contract [c] is [operand old_balance amount] and
    returns the current balance.
    Default balance type is [Main], pass [~kind] with [Deposit], [Fees] or
    [Rewards] for the others. *)
let balance_was_operated ~(operand) ~loc b contract ?(kind = Contract.Main) old_balance amount =
  operand old_balance amount |>
  Alpha_environment.wrap_error |> Lwt.return >>=? fun expected ->
  balance_is ~loc b contract ~kind expected

let balance_was_credited = balance_was_operated ~operand:Alpha_context.Tez.(+?)

let balance_was_debited = balance_was_operated ~operand:Alpha_context.Tez.(-?)


(* debug *)

let print_balances ctxt id =
  Contract.balance ~kind:Main ctxt id >>=? fun main ->
  Contract.balance ~kind:Deposit ctxt id >>=? fun deposit ->
  Contract.balance ~kind:Fees ctxt id >>=? fun fees ->
  Contract.balance ~kind:Rewards ctxt id >>|? fun rewards ->
  Format.printf "\nMain: %s\nDeposit: %s\nFees: %s\nRewards: %s\n"
    (Alpha_context.Tez.to_string main)
    (Alpha_context.Tez.to_string deposit)
    (Alpha_context.Tez.to_string fees)
    (Alpha_context.Tez.to_string rewards)
