(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

module type B58CHECK = sig
  type t
  val pp: Format.formatter -> t -> unit
  include S.B58_DATA with type t := t
end

let test_b58check_roundtrip
  : type t. (module B58CHECK with type t = t) -> t -> unit
  = fun m input ->
    let module M = (val m) in
    let testable = Alcotest.testable M.pp (=) in
    Roundtrips.test_rt_opt
      "b58check"
      testable
      M.to_b58check M.of_b58check_opt
      input

let test_b58check_roundtrips () =
  let pubkey_hash, pubkey, seckey = Ed25519.generate_key () in
  test_b58check_roundtrip (module Ed25519.Public_key_hash) pubkey_hash;
  test_b58check_roundtrip (module Ed25519.Public_key) pubkey;
  test_b58check_roundtrip (module Ed25519.Secret_key) seckey


let test_b58check_invalid input =
  Roundtrips.test_decode_opt_fail
    "b58check"
    (Alcotest.testable Ed25519.Public_key_hash.pp Ed25519.Public_key_hash.(=))
    Ed25519.Public_key_hash.of_b58check_opt
    input

let test_b58check_invalids () =
  List.iter test_b58check_invalid [
    "ThisIsGarbageNotACheck";
    "\x00";
    (String.make 1000 '\x00');
    (String.make 2048 'a');
    (String.init 2048 (fun _ -> Char.chr (Random.int 256)));
    "";
  ]

let tests = [
  "b58check.roundtrip", `Quick, test_b58check_roundtrips;
  "b58check.invalid", `Slow, test_b58check_invalids;
]

let () =
  Alcotest.run "tezos-crypto" [
    "ed25519", tests
  ]
