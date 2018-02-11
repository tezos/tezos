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

let name = "Isolate Origination"
module Logger = Logging.Make(struct let name = name end)

exception No_error

open Isolate_helpers
open Helpers_block
let (>>?=) = Assert.(>>?=)
let (>>=??) = Assert.(>>=??)

let originate root ?(tc=root.tezos_context) ?baker ?spendable ?fee ?delegatable src amount =
  let delegatable = Option.unopt ~default:true delegatable in
  let spendable = Option.unopt ~default:true spendable in
  let fee = Option.unopt ~default:10 fee in
  Apply.origination_pred
    ?baker
    ~tc
    ~pred: root
    (src, amount, spendable, delegatable, fee)

let test_simple_origination () =

  Init.main () >>=? fun root ->
  let src = List.hd Account.bootstrap_accounts in

  (* 0 balance should fail *)
  originate root src 0 >>= Assert.wrap >>= fun result ->
  Assert.initial_amount_too_low ~msg: __LOC__ result ;

  (* .5 Balance should fail *)
  originate root src 50 >>= Assert.wrap >>= fun result ->
  Assert.initial_amount_too_low ~msg: __LOC__ result ;

  (* 2. Balance should work *)
  originate root src 200 >>= Assert.ok >>= fun _ ->
  return ()


let delegate root ?(tc=root.tezos_context) ?baker ?fee src delegate =
  let fee = Option.unopt ~default:10 fee in
  Apply.delegation_pred
    ?baker
    ~tc
    ~pred: root
    (src, delegate, fee)

let test_delegation () =

  Init.main () >>=? fun root ->
  let account_a = List.nth Account.bootstrap_accounts 0 in
  let account_b = List.nth Account.bootstrap_accounts 1 in

  (* Delegatable should change delegate *)
  originate root ~delegatable: true account_a 200
  >>=? fun ((contracts, _errs), tc) ->
  let contract = List.hd contracts in
  let account_ac = {account_a with contract} in
  delegate root ~tc account_ac account_b.hpub >>= Assert.ok ~msg: __LOC__ >>= fun _ ->

  (* Not-Delegatable should not change delegate *)
  originate root ~delegatable: false account_a 200
  >>=? fun ((contracts, _errs), tc) ->
  let contract = List.hd contracts in
  let account_a = {account_a with contract} in
  delegate root ~tc account_a account_b.hpub >>= Assert.wrap >>= fun res ->
  Assert.non_delegatable ~msg: __LOC__ res ;

  return ()

let tests =
  List.map
    (fun (n, f) -> (n, (fun () -> f () >>= Assert.wrap)))
    [ "simple", test_simple_origination ;
      "delegate", test_delegation ;
    ]

