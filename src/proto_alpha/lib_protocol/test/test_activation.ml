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

let name = "Isolate Activation"

exception No_error

open Isolate_helpers


(* Generated commitment with secret included in commitment storage :

   pk = 097291124abb881ccdeea4f9a6912f34e3379586853360fa8ced414c1a3dee11
   pkh =dca88243fece75e9c22e63d162a8ada8f0cf4d94
   pk_b58 =tz1fkmDXEQdua3u71vstaKwR4h8KY7oT1PDF
   amount = 1868898542104
   secret =c5422e3864b9e6c5260e2aac76ea0f3d28d4fff7

   half_pkh = dca88243fece75e9c22e
   blinded_pkh : 4a6af2f5c466bf0a7a1001a1e9468cbfca82cef6
   amount :1868898542104130027 *)

let used_blind = MBytes.of_string "abc"

let hash_bytes pkh_bytes bytes =
  let open Ed25519.Public_key_hash in
  let hb = to_bytes (hash_bytes [ bytes ; pkh_bytes ]) in
  hash_bytes [ bytes ; hb ]

let test_hash_correctness () =
  let open Ed25519 in

  let module PKH = Public_key_hash in

  let pk = Public_key.of_hex_exn (`Hex "097291124abb881ccdeea4f9a6912f34e3379586853360fa8ced414c1a3dee11") in
  let pkh = PKH.of_hex_exn "dca88243fece75e9c22e63d162a8ada8f0cf4d94" in
  let pkh_b58c = "tz1fkmDXEQdua3u71vstaKwR4h8KY7oT1PDF" in
  let given_secret = MBytes.of_hex (`Hex "c5422e3864b9e6c5260e2aac76ea0f3d28d4fff7") in
  let expected_half_pkh = MBytes.of_hex (`Hex "dca88243fece75e9c22e") in
  let expected_blinded_pkh = MBytes.of_hex (`Hex "4a6af2f5c466bf0a7a1001a1e9468cbfca82cef6") in

  Assert.equal ~eq:(Public_key_hash.equal) (Public_key.hash pk) pkh;
  let pkh_bytes = PKH.to_bytes pkh in

  let pkh' = PKH.of_b58check_exn pkh_b58c in

  Assert.equal ~eq:(PKH.equal) pkh pkh';

  let half_pkh'_bytes = MBytes.sub (PKH.to_bytes pkh') 0 10 in
  Assert.equal ~eq:(MBytes.equal) half_pkh'_bytes  expected_half_pkh;

  let blinded_pkh = PKH.to_bytes (PKH.hash_bytes ~key:given_secret [ pkh_bytes ]) in
  Assert.equal ~eq:(MBytes.equal) blinded_pkh expected_blinded_pkh;

  return ()

let test_simple_activation () =
  let module PKH = Ed25519.Public_key_hash in
  Init.main () >>=? fun starting_block ->

  let id = Ed25519.Public_key_hash.of_b58check_exn "tz1fkmDXEQdua3u71vstaKwR4h8KY7oT1PDF" in
  let secret =
    Blinded_public_key_hash.secret_of_hex
      "c5422e3864b9e6c5260e2aac76ea0f3d28d4fff7" in

  let activation_operation = Alpha_context.(Activation { id ; secret }) in

  Proto_alpha.Apply.apply_anonymous_operation
    starting_block.tezos_context
    None
    starting_block.hash
    activation_operation >>=? fun (ctxt, _, _, _) ->

  let open Proto_alpha.Alpha_context in

  Lwt.return @@ Contract.of_b58check "tz1fkmDXEQdua3u71vstaKwR4h8KY7oT1PDF" >>=? fun ctrt ->

  Proto_alpha.Alpha_context.Contract.get_balance ctxt ctrt >>=? fun amount ->

  let expected_amount =
    match Tez.of_mutez 1868898542104130027L with
    | Some s -> s
    | _ -> Assert.fail_msg "Invalid conversion"
  in

  Assert.equal ~eq:(Tez.equal) amount expected_amount;

  return ()

let tests =
  List.map
    (fun (n, f) -> (n, (fun () -> f () >>= Assert.wrap)))
    [ "activation.hash_correctness", test_hash_correctness ;
      "activation.simple_activation", test_simple_activation
    ]
