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
