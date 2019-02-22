(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

let test_hash_matches (module X : S.SIGNATURE) () =
  let _, _, sk = X.generate_key () in
  let data = MBytes.of_string "ce input sa pun eu aici oare?" in
  let nonce = X.deterministic_nonce sk data in
  let nonce_hash = X.deterministic_nonce_hash sk data in
  let hashed_nonce = Blake2B.hash_bytes [nonce] in
  if nonce_hash <> Blake2B.to_bytes hashed_nonce then
    Alcotest.failf "the hash of deterministic_nonce is NOT deterministic_nonce_hash"


let ed25519 = (module Ed25519 : S.SIGNATURE)
let p256 = (module P256 : S.SIGNATURE)
let secp256k1 = (module Secp256k1 : S.SIGNATURE)

let tests = [
  "hash_matches_ed25519", `Quick, (test_hash_matches ed25519);
  "hash_matches_p256", `Quick, (test_hash_matches p256);
  "hash_matches_secp256k1", `Quick, (test_hash_matches secp256k1);
]

let () =
  Alcotest.run "tezos-crypto" [
    "deterministic_nonce", tests
  ]
