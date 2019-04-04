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

type public_key_hash =
  | Ed25519 of Ed25519.Public_key_hash.t
  | Secp256k1 of Secp256k1.Public_key_hash.t
  | P256 of P256.Public_key_hash.t

type public_key =
  | Ed25519 of Ed25519.Public_key.t
  | Secp256k1 of Secp256k1.Public_key.t
  | P256 of P256.Public_key.t

type secret_key =
  | Ed25519 of Ed25519.Secret_key.t
  | Secp256k1 of Secp256k1.Secret_key.t
  | P256 of P256.Secret_key.t

type watermark =
  | Block_header of Chain_id.t
  | Endorsement of Chain_id.t
  | Generic_operation
  | Custom of MBytes.t

val bytes_of_watermark: watermark -> MBytes.t

val pp_watermark : Format.formatter -> watermark -> unit

include S.SIGNATURE with type Public_key_hash.t = public_key_hash
                     and type Public_key.t = public_key
                     and type Secret_key.t = secret_key
                     and type watermark := watermark

val append : ?watermark:watermark -> secret_key -> MBytes.t -> MBytes.t
(** [append sk buf] is the concatenation of [buf] and the
    serialization of the signature of [buf] signed by [sk]. *)

val concat : MBytes.t -> t -> MBytes.t
(** [concat buf t] is the concatenation of [buf] and the serialization
    of [t]. *)

include S.RAW_DATA with type t := t

val of_secp256k1 : Secp256k1.t -> t
val of_ed25519 : Ed25519.t -> t
val of_p256 : P256.t -> t

type algo =
  | Ed25519
  | Secp256k1
  | P256

val algo_param: unit -> (algo, 'a) Clic.parameter

val generate_key:
  ?algo:algo ->
  ?seed:MBytes.t ->
  unit -> public_key_hash * public_key * secret_key
