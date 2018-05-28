(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Proto_alpha
open Error_monad

let name = "Isolate Transactions"
module Logger = Logging.Make(struct let name = name end)
open Logger

module Helpers = Isolate_helpers
module Assert = Helpers.Assert

let test_basic (): unit tzresult Lwt.t =
  Helpers.Init.main () >>=? fun starting_block ->
  let init_tc = starting_block.tezos_context in

  Helpers.Account.make_2_accounts ~tc: init_tc >>=? fun ((account_a, account_b), init_tc) ->
  Helpers.Account.make_account ~tc: init_tc >>=? fun (_baker, init_tc) ->
  let account_unknown_foo = Helpers.Account.new_account () in
  debug "Accounts set" ;

  let transfer ?(tc=init_tc) ?fee (src, dst, amount) =
    Helpers.Apply.transaction_pred
      ~tc
      ~pred: starting_block
      (src, dst, amount, fee)
  in
  let originate ?(tc=init_tc) =
    Helpers.Apply.origination_pred
      ~tc
      ~pred: starting_block
  in

  let init_amount = Helpers.Account.init_amount in

  (* Send from a sender with no balance (never seen). *)
  (* TODO: Is it OK to get Storage_error and not something more specific? *)
  transfer (account_unknown_foo, account_b, 10000) >|=
  Assert.unknown_contract ~msg: __LOC__ >>= fun _ ->
  debug "Transfer from no balance V2" ;

  (* Send 10 tz to unknown account. *)
  transfer (account_a, account_unknown_foo, 10000) >>=
  Assert.ok >>=? fun (_, tc) ->
  Assert.equal_cents_balance ~msg: __LOC__ ~tc (account_unknown_foo.contract, 10000) >>=? fun () ->
  debug "Reception" ;

  (* Unknown account transfers back tz. *)
  transfer ~tc (account_unknown_foo, account_a, 9990) >>=
  Assert.ok >>=? fun _ ->
  debug "Transfer back" ;

  (* Check that a basic transfer originates no contracts. *)
  transfer (account_a, account_b, 1000) >>=? fun (contracts, _) ->
  Assert.equal_int ~msg: __LOC__ 0 (List.length contracts) ;
  debug "No contracts originated" ;

  (* Check sender/receiver balance post transaction *)
  transfer (account_a, account_b, 1000) >>=
  Assert.ok ~msg: __LOC__ >>=? fun (_,tc) ->
  Proto_alpha.Alpha_context.Contract.get_balance tc account_a.contract >>=? fun _balance ->
  Assert.equal_cents_balance ~msg: __LOC__ ~tc (account_a.contract, init_amount * 100 - 1000 - 10) >>=? fun () ->
  Assert.equal_cents_balance ~msg: __LOC__ ~tc (account_b.contract, 1001000) >>=? fun () ->
  debug "Transfer balances" ;

  (* Check balance too low. *)
  transfer (account_a, account_b, 10000000) >|=
  Assert.balance_too_low ~msg: __LOC__ >>= fun _ ->
  debug "Too low" ;

  (* Check non-spendability of a non-spendable contract *)
  (* TODO: Unspecified economic error: should be more specific. *)
  originate (account_a, 1000, false, true, 0)
  >>= Assert.ok ~msg: __LOC__ >>=? fun (contracts, tc) ->
  Assert.equal_int (List.length contracts) 1 ;
  let non_spendable = List.hd contracts in
  let account = {account_a with contract = non_spendable} in
  debug "Contract created" ;

  transfer (account, account_b, 50) ~tc >>= Assert.wrap >>= fun result ->
  Assert.non_spendable ~msg: __LOC__ result ;
  debug "Non Spendable" ;

  (* Check spendability of a spendable contract *)
  originate (account_a, 1000, true, true, 100)
  >>= Assert.ok ~msg: __LOC__ >>=? fun (contracts, spendable_tc) ->
  Assert.equal_int (List.length contracts) 1 ;
  let contract_spendable = List.hd contracts in
  let account_spendable = {account_a with contract = contract_spendable} in
  debug "Contract created" ;
  transfer (account_spendable, account_b, 50) ~tc: spendable_tc >>= Assert.ok ~msg: __LOC__ >>=? fun _ ->
  debug "Spendable" ;


  (* Try spending a default account with unmatching pk/sk pairs. *)
  let account = { account_a with ppk = account_b.ppk } in
  transfer (account, account_b, 50)
  >>= Assert.wrap >>= fun result ->
  Assert.generic_economic_error ~msg: __LOC__ result ;
  debug "Unmatching keys" ;

  (* Try spending a default account with keys not matching the
     contract pkh. *)
  let account = {account_a with contract = account_b.contract } in
  transfer (account, account_unknown_foo, 50)
  >>= Assert.wrap >>= fun result ->
  Assert.inconsistent_pkh ~msg: __LOC__ result ;
  debug "Unmatching contract" ;

  (* Try spending an originated contract without the manager's key. *)
  let account = {account_b with contract = contract_spendable } in
  transfer
    ~tc: spendable_tc
    (account, account_unknown_foo, 50)
  >>= Assert.wrap >>= fun result ->
  Assert.inconsistent_pkh ~msg: __LOC__ result ;
  debug "No manager key" ;
  return ()

let test_cycle_transfer () =
  Helpers.Init.main () >>=? fun pred ->
  let transfer = Helpers.Apply.transaction_pred ~pred in
  let tc = pred.tezos_context in
  let cycle n =
    Helpers.Account.make_accounts ~tc n >>=? fun (accounts, tc) ->
    let pairs = List.combine accounts @@ List.shift accounts in
    let aux tc (src, dst) =
      transfer ~tc (src, dst, 10000, Some(10)) >>=? fun (_, tc) -> return tc
    in
    fold_left_s aux tc pairs >>=? fun tc ->
    let aux (account: Helpers.Account.t) =
      Helpers.Assert.equal_cents_balance ~tc ~msg: __LOC__ (account.contract, Helpers.Account.init_amount * 100 - 10)
    in
    iter_s aux accounts
  in
  cycle 2 >>=? fun _ ->
  cycle 13 >>=? fun _ ->
  cycle 50 >>=? fun _ ->
  return ()

let tests =
  List.map
    (fun (n, f) -> (n, (fun () -> f () >>= Assert.wrap)))
    [ "transaction.basic", test_basic ;
      "transaction.cycle_transfer", test_cycle_transfer
    ]
