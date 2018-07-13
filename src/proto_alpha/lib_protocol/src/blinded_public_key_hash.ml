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

module H = Blake2B.Make(Base58)(struct
    let name = "Blinded public key hash"
    let title = "A blinded public key hash"
    let b58check_prefix = "\001\002\049\223"
    let size = Some Ed25519.Public_key_hash.size
  end)

include H

let () =
  Base58.check_encoded_prefix b58check_encoding "btz1" 37

let of_ed25519_pkh activation_code pkh =
  hash_bytes ~key:activation_code [ Ed25519.Public_key_hash.to_bytes pkh ]

type activation_code = MBytes.t

let activation_code_size = Ed25519.Public_key_hash.size
let activation_code_encoding = Data_encoding.Fixed.bytes activation_code_size

let activation_code_of_hex h =
  if Compare.Int.(String.length h <> activation_code_size * 2) then
    invalid_arg "Blinded_public_key_hash.activation_code_of_hex" ;
  MBytes.of_hex (`Hex h)

module Index = H
