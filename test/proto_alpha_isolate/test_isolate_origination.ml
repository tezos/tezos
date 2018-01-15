(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)


let name = "Isolate Origination"
module Logger = Logging.Make(struct let name = name end)
let section = Lwt_log.Section.make name
let () =
  Lwt_log.Section.set_level section Lwt_log.Debug


exception No_error

open Isolate_helpers
let (>>?=) = Assert.(>>?=)
let (>>=??) = Assert.(>>=??)

let test_simple_origination originate =
  let open Proto_alpha.Error_monad in
  let src = List.hd Account.bootstrap_accounts in

  (* 0 balance should fail *)
  originate src 0 >>= Assert.wrap >>= fun result ->
  Assert.initial_amount_too_low ~msg: __LOC__ result ;

  (* .5 Balance should fail *)
  originate src 50 >>= Assert.wrap >>= fun result ->
  Assert.initial_amount_too_low ~msg: __LOC__ result ;

  (* 2. Balance should work *)
  originate src 200 >>= Assert.ok >>= fun _ ->
  return ()


let test_delegation
    (originate: ?tc: Proto_alpha.Tezos_context.t -> ?delegatable: bool -> 'a)
    (delegate: ?tc: Proto_alpha.Tezos_context.t -> 'b)
  =

  let open Proto_alpha.Error_monad in
  let account_a = List.nth Account.bootstrap_accounts 0 in
  let account_b = List.nth Account.bootstrap_accounts 1 in

  (* Delegatable should change delegate *)
  originate
    ~delegatable: true account_a 200
  >>=? fun ((contracts, _errs), tc) ->
  let contract = List.hd contracts in
  let account_ac = {account_a with contract} in
  delegate ~tc account_ac account_b.hpub >>= Assert.ok ~msg: __LOC__ >>= fun _ ->

  (* Not-Delegatable should not change delegate *)
  originate
    ~delegatable: false account_a 200
  >>=? fun ((contracts, _errs), tc) ->
  let contract = List.hd contracts in
  let account_a = {account_a with contract} in
  delegate ~tc account_a account_b.hpub >>= Assert.wrap >>= fun res ->
  Assert.non_delegatable ~msg: __LOC__ res ;

  return ()


let main (): unit Error_monad.tzresult Lwt.t =

  Init.main () >>=? fun root ->

  let originate ?(tc=root.tezos_context) ?baker ?spendable ?fee ?delegatable src amount =
    let delegatable = Option.unopt ~default:true delegatable in
    let spendable = Option.unopt ~default:true spendable in
    let fee = Option.unopt ~default:10 fee in
    Apply.origination_pred
      ?baker
      ~tc
      ~pred: root
      (src, amount, spendable, delegatable, fee)
  in
  let delegate ?(tc=root.tezos_context) ?baker ?fee src delegate =
    let fee = Option.unopt ~default:10 fee in
    Apply.delegation_pred
      ?baker
      ~tc
      ~pred: root
      (src, delegate, fee)
  in

  test_simple_origination originate >>=?? fun () ->
  test_delegation
    (originate ?fee: None ?baker: None ~spendable: true)
    (delegate ?fee: None ?baker: None) >>=?? fun () ->

  Error_monad.return ()


let tests = [
  "main", (fun _ -> main ()) ;
]

let main () =
  Test.run "origination." tests
