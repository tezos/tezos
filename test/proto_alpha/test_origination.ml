(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Proto_alpha
open Tezos_context
module Helpers = Proto_alpha_helpers
module Assert = Helpers.Assert

let run blkid ({ b1 ; b2 ; _ } : Helpers.Account.bootstrap_accounts) =

  let cents v =
    match Tez.( *? ) Tez.one_cent v with
    | Error _ -> Pervasives.failwith "cents"
    | Ok r -> r in

  Helpers.Baking.bake blkid b1 [] >>=? fun blkh ->
  let foo = Helpers.Account.create "foo" in

  (* Origination with amount = 0 tez *)
  Helpers.Account.originate
    ~src:foo
    ~manager_pkh:foo.pkh
    ~balance:Tez.zero () >>= fun result ->
  Assert.unknown_contract ~msg:__LOC__ result ;

  (* Origination with amount = .5 tez *)
  Helpers.Account.originate
    ~src:b1
    ~manager_pkh:foo.pkh
    ~balance:Tez.fifty_cents () >>= fun result ->
  Assert.initial_amount_too_low ~msg:__LOC__ result ;

  (* Origination with amount = 1 tez *)
  Helpers.Account.originate
    ~src:b1
    ~manager_pkh:foo.pkh
    ~balance:(cents 99L) () >>= fun result ->
  Assert.initial_amount_too_low ~msg:__LOC__ result ;

  (* Origination with amount > 1 tez *)
  Helpers.Account.originate
    ~src:b1
    ~manager_pkh:foo.pkh
    ~balance:Tez.one () >>=? fun _result ->
  (* TODO: test if new contract exists *)

  (* Non-delegatable contract *)
  Helpers.Account.originate
    ~src:b1
    ~manager_pkh:b1.pkh
    ~balance:(cents 1000L) () >>=? fun (_oph, nd_contract) ->

  (* Delegatable contract *)
  Helpers.Account.originate
    ~src:b1
    ~manager_pkh:b1.pkh
    ~delegate:b1.pkh
    ~balance:(cents 1000L) () >>=? fun (_oph, d_contract) ->

  (* Change delegate of a non-delegatable contract *)
  let manager_sk = Client_keys.Secret_key_locator.create
      ~scheme:"unencrypted"
      ~location:(Ed25519.Secret_key.to_b58check b1.sk) in

  Helpers.Account.set_delegate
    ~fee:(cents 5L)
    ~contract:nd_contract
    ~manager_sk
    ~src_pk:b1.pk
    (Some b2.pkh) >>= fun result ->
  Assert.non_delegatable ~msg:__LOC__ result ;

  (* Change delegate of a delegatable contract *)
  Helpers.Account.set_delegate
    ~contract:d_contract
    ~manager_sk
    ~src_pk:b1.pk
    (Some b2.pkh) >>=? fun _result ->
  Assert.delegate_equal ~msg:__LOC__ d_contract (Some b2.pkh) >>=? fun () ->

  return blkh

let main () =
  Helpers.init ~rpc_port:18200 () >>=? fun (_node_pid, hash) ->
  run (`Hash hash) Helpers.Account.bootstrap_accounts >>=? fun _blkh ->
  return ()

let tests = [
  "main", (fun _ -> main ()) ;
]

let () =
  let module Test = Tezos_test_helpers.Test.Make(Error_monad) in
  Test.run "origination." tests
