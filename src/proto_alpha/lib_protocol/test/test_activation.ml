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


(* Generated commitment with secret included in commitment storage :

   (blind = "abc")

   pk = edpktiPG79C8CRTMxua67NEaVKH7AydAMWRiC5KHQv78Ckx4UrZYBy
   pkh = tz1fkmDXEQdua3u71vstaKwR4h8KY7oT1PDF
   amount = 1868898542104.130027

   secret = 0xc5422e3864b9e6c5260e2aac76ea0f3d28d4fff7

   half_pkh = 0xdca88243fece75e9c22e
   blinded_pkh : 0x4a6af2f5c466bf0a7a1001a1e9468cbfca82cef6

*)

let pk =
  Ed25519.Public_key.of_b58check_exn
    "edpktiPG79C8CRTMxua67NEaVKH7AydAMWRiC5KHQv78Ckx4UrZYBy"
let pkh = Ed25519.Public_key.hash pk
let half_pkh =
  let len = Ed25519.Public_key_hash.size / 2 in
  MBytes.sub (Ed25519.Public_key_hash.to_bytes pkh) 0 (len / 2)

let given_secret =
  Blinded_public_key_hash.secret_of_hex
    "c5422e3864b9e6c5260e2aac76ea0f3d28d4fff7"

let expected_blinded_pkh =
  Blinded_public_key_hash.of_b58check_exn
    "btz1T77Ly5U1bWNBR5KzDSgNFST5Bh5F1eB6g"

let expected_amount =
  match Tez.of_mutez 1868898542104130027L with
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
    starting_block.hash
    activation_operation >>=? fun (ctxt, _) ->

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
