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
open Helpers_logger
open Isolate_helpers

exception No_error

let test_dsl () : unit proto_tzresult Lwt.t =

  Init.main () >>=? fun starting_block ->
  let init_tc = starting_block.tezos_context in

  Account.make_2_accounts ~tc: init_tc >>=? fun ((account_a, account_b), init_tc) ->
  Account.make_account ~tc: init_tc >>=? fun (_baker, init_tc) ->
  let account_unknown_foo = Account.new_account () in
  debug "Accounts set" ;

  let default_fee = 10 in

  let transfer ?(tc=init_tc) ?fee a b c =
    Apply.transaction_pred
      ~tc
      ~pred: starting_block
      (a,b,c, fee)
  in
  let originate ?(tc=init_tc) ?fee ?(spendable=true) ?(delegatable=true) a b =
    let fee = Option.unopt ~default:default_fee fee in
    Apply.origination_pred
      ~tc
      ~pred: starting_block
      (a, b, spendable, delegatable, fee)
  in

  (* Send from a sender with no balance (never seen). *)
  (* TODO: Is it OK to get Storage_error and not something more specific? *)
  transfer
    account_unknown_foo
    account_b
    10000 >|= Assert.unknown_contract ~msg: __LOC__ >>= fun _ ->
  debug "Transfer from no balance V2" ;

  (* Send 1000 tz to unknown account. *)
  transfer
    account_a
    account_unknown_foo
    10000 >>= Assert.ok_contract >>=? fun (_, tc) ->
  Assert.equal_cents_balance ~msg: __LOC__ ~tc (account_unknown_foo.contract, 10000) >>=? fun () ->
  debug "Reception" ;

  (* Check that a basic transfer originates no contracts. *)
  transfer
    ~tc
    account_a
    account_b
    1000
  >>=? fun ((contracts, _), _) ->
  Assert.equal_int ~msg: __LOC__ 0 (List.length contracts) ;
  debug "No contracts originated" ;

  (* Check sender/receiver balance post transaction *)
  transfer
    account_a
    account_b
    1000
  >>= Assert.ok_contract ~msg: __LOC__ >>=? fun (_,tc) ->
  Assert.equal_cents_balance ~msg: __LOC__ ~tc (account_a.contract, 998990) >>=? fun () ->
  Assert.equal_cents_balance ~msg: __LOC__ ~tc (account_b.contract, 1001000) >>=? fun () ->
  debug "Transfer balances" ;

  (* Check balance too low. *)
  transfer
    account_a
    account_b
    10000000
  >|= Assert.balance_too_low ~msg: __LOC__ >>= fun _ ->
  debug "Too low" ;

  (* Check non-spendability of a non-spendable contract *)
  (* TODO: Unspecified economic error: should be more specific. *)
  originate
    ~spendable: false
    account_a
    1000
  >>= Assert.ok_contract ~msg: __LOC__ >>=? fun ((contracts,_), tc) ->
  Assert.equal_int (List.length contracts) 1 ;
  let non_spendable = List.hd contracts in
  let account = {account_a with contract = non_spendable} in
  debug "Contract created" ;
  transfer account account_b 50 ~tc >>= Assert.wrap >>= fun result ->
  Assert.non_spendable ~msg: __LOC__ result ;
  debug "Non Spendable" ;

  (* Check spendability of a spendable contract *)
  originate
    ~spendable: true
    ~fee: 100
    account_a
    1000
  >>= Assert.ok_contract ~msg: __LOC__ >>=? fun ((contracts, _), spendable_tc) ->
  Assert.equal_int (List.length contracts) 1 ;
  let contract_spendable = List.hd contracts in
  let account_spendable = {account_a with contract = contract_spendable} in
  debug "Contract created" ;
  transfer account_spendable account_b 50 ~tc: spendable_tc >>= Assert.ok ~msg: __LOC__ >>=? fun _ ->
  debug "Spendable" ;


  (* Try spending a default account with unmatching pk/sk pairs. *)
  let account = { account_a with ppk = account_b.ppk } in
  transfer
    account
    account_b
    50
  >>= Assert.wrap >>= fun result ->
  Assert.generic_economic_error ~msg: __LOC__ result ;
  debug "Unmatching keys" ;

  (* Try spending a default account with keys not matching the
     contract pkh. *)
  let account = {account_a with contract = account_b.contract } in
  transfer
    account
    account_unknown_foo
    50
  >>= Assert.wrap >>= fun result ->
  Assert.inconsistent_pkh ~msg: __LOC__ result ;
  debug "Unmatching contract" ;

  (* Try spending an originated contract without the manager's key. *)
  let account = {account_b with contract = contract_spendable } in
  transfer
    ~tc: spendable_tc
    account
    account_unknown_foo
    50
  >>= Assert.wrap >>= fun result ->
  Assert.inconsistent_pkh ~msg: __LOC__ result ;
  debug "No manager key" ;

  return ()


let tests =
  List.map
    (fun (n, f) -> (n, (fun () -> f () >>= Assert.wrap)))
    [ "dsl", test_dsl
    ]
