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

val sign :
  ([ `POST ], unit, unit * Signature.Public_key_hash.t,
   Signature.t option, MBytes.t, Signature.t) RPC_service.t

val deterministic_nonce :
  ([ `POST ], unit, unit * Signature.Public_key_hash.t,
   Signature.t option, MBytes.t, MBytes.t) RPC_service.t

val deterministic_nonce_hash :
  ([ `POST ], unit, unit * Signature.Public_key_hash.t,
   Signature.t option, MBytes.t, MBytes.t) RPC_service.t

val supports_deterministic_nonces :
  ([ `GET ], unit, unit * Signature.Public_key_hash.t,
   unit, unit, bool) RPC_service.t

val public_key :
  ([ `GET ], unit, unit * Signature.Public_key_hash.t,
   unit, unit, Signature.Public_key.t) RPC_service.t

val authorized_keys :
  ([ `GET ], unit, unit,
   unit, unit, Signature.Public_key_hash.t list option) RPC_service.t
