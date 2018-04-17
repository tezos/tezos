(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

let get_keys () =
  let seed = Ed25519.Seed.generate () in
  Ed25519.generate_seeded_key seed

module type B58CHECK = sig
  type t
  val pp: Format.formatter -> t -> unit
  include S.B58_DATA with type t := t
end

let test_b58check_roundtrip
  : type t. (module B58CHECK with type t = t) -> t -> unit
  = fun m input ->
    let module M = (val m) in
    Roundtrips.test_rt_opt "b58check" M.pp M.to_b58check M.of_b58check_opt input

let test_b58check_roundtrips () =
  let (pubkey_hash, pubkey, seckey) = get_keys () in
  test_b58check_roundtrip (module Ed25519.Public_key_hash) pubkey_hash;
  test_b58check_roundtrip (module Ed25519.Public_key) pubkey;
  test_b58check_roundtrip (module Ed25519.Secret_key) seckey


let test_b58check_invalid input =
  Roundtrips.test_decode_opt_fail
    "b58check"
    Format.pp_print_string
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
