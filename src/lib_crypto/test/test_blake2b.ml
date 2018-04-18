(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

let test_hashed_roundtrip name enc dec input =
  (* this wrapper to start with hashing *)
  Roundtrips.test_rt_opt
    name
    (Alcotest.testable
       (fun fmt (input, _) -> Format.fprintf fmt "%s" input)
       (fun (_, hashed) (_, decoded) -> hashed = decoded)
    )
    (fun (_, hashed) -> enc hashed)
    (fun encoded -> match dec encoded with
       | None -> None
       | Some decoded -> Some (input, decoded)
    )
    (input, Blake2B.hash_string [input])

let test_roundtrip_hex input =
  test_hashed_roundtrip "Hex" Blake2B.to_hex Blake2B.of_hex_opt input

let test_roundtrip_string input =
  test_hashed_roundtrip "String" Blake2B.to_string Blake2B.of_string_opt input

let inputs = [
  "abc";
  (string_of_int max_int);
  "0";
  "00";
  (String.make 64 '0');
  (*loads of ascii characters: codes between 32 and 126 *)
  (String.init 1000 (fun i -> (Char.chr (32 + (i mod (126 - 32))))));
  "";
]

let test_roundtrip_hexs () = List.iter test_roundtrip_hex inputs

let test_roundtrip_strings () = List.iter test_roundtrip_string inputs

let tests = [
  "hash hex/dehex", `Quick, test_roundtrip_hexs;
  "hash print/parse", `Quick, test_roundtrip_strings;
]

let () =
  Alcotest.run "tezos-crypto" [
    "blake2b", tests
  ]
