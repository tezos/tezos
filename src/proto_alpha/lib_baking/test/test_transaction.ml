(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Proto_alpha
open Alpha_context
module Helpers = Proto_alpha_helpers
module Assert = Helpers.Assert

let run blkid ({ b1 ; b2 ; b3 ; _ } : Helpers.Account.bootstrap_accounts) =

  Helpers.Baking.bake blkid b1 [] >>=? fun blkh ->
  let foo = Helpers.Account.create "foo" in
  let bar = Helpers.Account.create "bar" in

  let tez v =
    match Tez.( *? ) Tez.one v with
    | Error _ -> Pervasives.failwith "cents"
    | Ok r -> r in

  (* Send from a sender with no balance (never seen). *)
  (* TODO: Is it OK to get Storage_error and not something more specific? *)
  Helpers.Account.transfer
    ~account:foo
    ~destination:b1.contract
    ~amount:(tez 1000L) () >>= fun result ->
  Assert.unknown_contract ~msg:__LOC__ result ;

  (* Send 1000 tz to "foo". *)
  Helpers.Account.transfer
    ~account:b1
    ~destination:foo.contract
    ~fee:Tez.zero
    ~amount:(tez 1000L) () >>=? fun (_oph, contracts) ->
  Assert.balance_equal ~msg:__LOC__ foo 1000_000_000L >>=? fun () ->

  (* Check that a basic transfer originates no contracts. *)
  Assert.equal_int ~msg:__LOC__ 0 (List.length contracts) ;

  (* Check sender/receiver balance post transaction *)
  Helpers.Account.transfer
    ~account:foo
    ~destination:bar.contract
    ~fee:Tez.zero
    ~amount:(tez 50L) () >>=? fun _contracts ->
  Assert.balance_equal ~msg:__LOC__ foo 950_000_000L >>=? fun () ->
  Assert.balance_equal ~msg:__LOC__ bar 50_000_000L >>=? fun () ->

  (* Check balance too low. *)
  Helpers.Account.transfer
    ~account:bar
    ~destination:foo.contract
    ~amount:(tez 1000L) () >>= fun result ->
  Assert.balance_too_low ~msg:__LOC__ result ;

  (* Check spendability of a spendable contract *)
  Helpers.Account.originate
    ~src:foo
    ~manager_pkh:foo.pkh
    ~balance:(tez 50L) () >>=? fun (_oph, spendable) ->
  Format.printf "Created contract %a@." Contract.pp spendable ;
  let account = { foo with contract = spendable } in
  Helpers.Account.transfer
    ~account
    ~destination:foo.contract
    ~amount:(tez 10L) () >>=? fun _contracts ->

  (* Try spending a default account with unmatching pk/sk pairs. *)
  let account = { b1 with sk = b2.sk } in
  Helpers.Account.transfer
    ~account
    ~destination:b2.contract
    ~amount:(tez 10L) () >>= fun result ->
  Assert.generic_economic_error ~msg:__LOC__ result ;

  (* Try spending a default account with keys not matching the
     contract pkh. *)
  let account = { b1 with contract = b2.contract } in
  Helpers.Account.transfer
    ~account
    ~destination:b3.contract
    ~amount:(tez 10L) () >>= fun result ->
  Assert.inconsistent_public_key ~msg:__LOC__ result ;

  (* Try spending an originated contract without the manager's key. *)
  let account = { b1 with contract = spendable } in
  Helpers.Account.transfer
    ~account
    ~destination:b2.contract
    ~amount:(tez 10L) () >>= fun result ->
  Assert.inconsistent_public_key ~msg:__LOC__ result ;

  return blkh

let exe = try Sys.argv.(1) with _ -> "tezos-node"
let sandbox = try Sys.argv.(2) with _ -> "sandbox.json"
let rpc_port = try int_of_string Sys.argv.(3) with _ -> 18300

let main () =
  Helpers.init ~exe ~sandbox ~rpc_port () >>=? fun (_node_pid, genesis) ->
  run (`Hash genesis) Helpers.Account.bootstrap_accounts >>=? fun _blkh ->
  return ()

let tests = [
  "main", (fun _ -> main ()) ;
]

let wrap (n, f) =
  Alcotest_lwt.test_case n `Quick begin fun _ () ->
    f () >>= function
    | Ok () -> Lwt.return_unit
    | Error error ->
        Format.kasprintf Pervasives.failwith "%a" pp_print_error error
  end

let () =
  Alcotest.run ~argv:[|""|] "tezos-client-alpha" [
    "transactions", List.map wrap tests
  ]
