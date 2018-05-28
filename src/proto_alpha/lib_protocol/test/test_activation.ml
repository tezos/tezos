(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Proto_alpha
open Alpha_context
open Error_monad

let name = "Isolate Activation"

exception No_error

open Isolate_helpers

let pk =
  Ed25519.Public_key.of_b58check_exn
    "edpkuSR6ywqsk17myFVRcw2eXhVib2MeLc9D1QkEQb98ctWUBwSJpF"
let pkh = Ed25519.Public_key.hash pk
let half_pkh =
  let len = Ed25519.Public_key_hash.size / 2 in
  MBytes.sub (Ed25519.Public_key_hash.to_bytes pkh) 0 (len / 2)

let given_secret =
  Blinded_public_key_hash.secret_of_hex
    "0f39ed0b656509c2ecec4771712d9cddefe2afac"

let expected_blinded_pkh =
  Blinded_public_key_hash.of_b58check_exn
    "btz1bRL4X5BWo2Fj4EsBdUwexXqgTf75uf1qa"

let expected_amount =
  match Tez.of_mutez 23932454669343L with
  | Some s -> s
  | _ -> assert false

let test_hash_correctness () =

  let blinded_pkh =
    Blinded_public_key_hash.of_ed25519_pkh given_secret pkh in

  Assert.equal
    ~msg: __LOC__
    ~eq: Blinded_public_key_hash.(=)
    blinded_pkh expected_blinded_pkh ;

  return ()

let test_simple_activation () =

  Init.main () >>=? fun starting_block ->

  let activation_operation =
    Alpha_context.Activation
      { id = pkh ; secret = given_secret } in

  Proto_alpha.Apply.apply_anonymous_operation
    starting_block.tezos_context
    None
    activation_operation >>=? fun ctxt ->

  let contract = Contract.implicit_contract pkh in

  Contract.get_balance ctxt contract >>=? fun amount ->

  Assert.equal
    ~msg: __LOC__
    ~eq:Tez.equal
    amount expected_amount ;

  return ()

let tests =
  List.map
    (fun (n, f) -> (n, (fun () -> f () >>= Assert.wrap)))
    [ "activation.hash_correctness", test_hash_correctness ;
      "activation.simple_activation", test_simple_activation
    ]
