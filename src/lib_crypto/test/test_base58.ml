(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

let test_roundtrip_safe input =
  Roundtrips.test_rt_opt
    "safe base58"
    Alcotest.string
    Base58.safe_encode Base58.safe_decode
    input

let test_roundtrip_raw input =
  Roundtrips.test_rt_opt
    "raw base58"
    Alcotest.string
    Base58.raw_encode Base58.raw_decode
    input

let inputs = [
  "abc";
  (string_of_int max_int);
  "0";
  "00";
  "000";
  "0000";
  "0000000000000000";
  (String.make 64 '0');
  "1";
  "11";
  "111";
  "1111";
  (String.make 2048 '0');
  "2";
  "22";
  "5";
  "Z";
  (String.make 2048 'Z');
  "z";
  "zz";
  "zzzzzzzz";
  (String.make 2048 'z');
  (*loads of ascii characters: codes between 32 and 126 *)
  (String.init 1000 (fun i -> (Char.chr (32 + (i mod (126 - 32))))));
  "";
]

let test_roundtrip_safes () = List.iter test_roundtrip_safe inputs

let test_roundtrip_raws () = List.iter test_roundtrip_raw inputs


let test_safety input =
  Roundtrips.test_decode_opt_safe
    "safe base58"
    Alcotest.string
    Base58.safe_decode
    input

let test_safetys () = List.iter test_safety inputs

let tests = [
  "safe decoding", `Quick, test_safetys;
  "safe encoding/decoding", `Quick, test_roundtrip_safes;
  "raw encoding/decoding", `Quick, test_roundtrip_raws;
]

let () =
  Alcotest.run "tezos-crypto" [
    "base58", tests
  ]
