(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

module Helpers = Proto_alpha_helpers
module Assert = Helpers.Assert

let run blkid ({ b1 ; b2 ; b3 ; _ } : Helpers.Account.bootstrap_accounts) =

  Helpers.Baking.mine blkid b1 [] >>=? fun blkh ->
  let foo = Helpers.Account.create "foo" in
  let bar = Helpers.Account.create "bar" in

  (* Send from a sender with no balance (never seen). *)
  (* TODO: Is it OK to get Storage_error and not something more specific? *)
  Helpers.Account.transfer
    ~account:foo
    ~destination:b1.contract
    ~amount:1000_00L () >>= fun result ->
  Assert.unknown_contract ~msg:__LOC__ result ;

  (* Send 1000 tz to "foo". *)
  Helpers.Account.transfer
    ~account:b1
    ~destination:foo.contract
    ~amount:1000_00L () >>=? fun (_oph, contracts) ->
  Assert.balance_equal ~msg:__LOC__ foo 1000_00L >>=? fun () ->

  (* Check that a basic transfer originates no contracts. *)
  Assert.equal_int ~msg:__LOC__ 0 (List.length contracts) ;

  (* Check sender/receiver balance post transaction *)
  Helpers.Account.transfer
    ~account:foo
    ~destination:bar.contract
    ~amount:50_00L () >>=? fun _contracts ->
  Assert.balance_equal ~msg:__LOC__ foo 949_95L >>=? fun () ->
  Assert.balance_equal ~msg:__LOC__ bar 50_00L >>=? fun () ->

  (* Check balance too low. *)
  Helpers.Account.transfer
    ~account:bar
    ~destination:foo.contract
    ~amount:1000_00L () >>= fun result ->
  Assert.balance_too_low ~msg:__LOC__ result ;

  (* Check non-spendability of a non-spendable contract *)
  (* TODO: Unspecified economic error: should be more specific. *)
  Helpers.Account.originate
    ~src:foo
    ~manager_pkh:foo.pkh
    ~spendable:false
    ~balance:50_00L () >>=? fun (_oph, non_spendable) ->
  Format.printf "Created non-spendable contract %a@." Contract.pp non_spendable ;
  let account = { foo with contract = non_spendable } in
  Helpers.Account.transfer
    ~account
    ~destination:bar.contract
    ~amount:10_00L () >>= fun result ->
  Assert.non_spendable ~msg:__LOC__ result ;

  (* Check spendability of a spendable contract *)
  Helpers.Account.originate
    ~src:foo
    ~manager_pkh:foo.pkh
    ~spendable:true
    ~balance:50_00L () >>=? fun (_oph, spendable) ->
  Format.printf "Created contract %a@." Contract.pp spendable ;
  let account = { foo with contract = spendable } in
  Helpers.Account.transfer
    ~account
    ~destination:foo.contract
    ~amount:10_00L () >>=? fun _contracts ->

  (* Try spending a default account with unmatching pk/sk pairs. *)
  let account = { b1 with sk = b2.sk } in
  Helpers.Account.transfer
    ~account
    ~destination:b2.contract
    ~amount:10_00L () >>= fun result ->
  Assert.generic_economic_error ~msg:__LOC__ result ;

  (* Try spending a default account with keys not matching the
     contract pkh. *)
  let account = { b1 with contract = b2.contract } in
  Helpers.Account.transfer
    ~account
    ~destination:b3.contract
    ~amount:10_00L () >>= fun result ->
  Assert.inconsistent_pkh ~msg:__LOC__ result ;

  (* Try spending an originated contract without the manager's key. *)
  let account = { b1 with contract = spendable } in
  Helpers.Account.transfer
    ~account
    ~destination:b2.contract
    ~amount:10_00L () >>= fun result ->
  Assert.inconsistent_public_key ~msg:__LOC__ result ;

  return blkh

let main () =
  Helpers.init ~rpc_port:18300 () >>=? fun (_node_pid, hash) ->
  run (`Hash hash) Helpers.Account.bootstrap_accounts >>=? fun _blkh ->
  return ()

let tests = [
  "main", (fun _ -> main ()) ;
]

let () =
  Test.run "transactions." tests
