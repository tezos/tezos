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

module Authorized_key :
  Client_aliases.Alias with type t := Signature.public_key
(** Storage for keys that have been authorized for baking. *)

val public_key :
  #Client_context.wallet ->
  Signature.public_key_hash -> Signature.public_key tzresult Lwt.t
(** [public_key cctxt pkh] returns the public key whose hash is [pkh]
    iff it is present if [cctxt]. *)

val sign :
  #Client_context.wallet ->
  Signer_messages.Sign.Request.t ->
  ?magic_bytes:int list ->
  check_high_watermark:bool -> require_auth:bool -> Signature.t tzresult Lwt.t
(** [sign cctxt req ?magic_bytes ~check_high_watermark ~require_auth]
    signs [req] and returns a signature. *)

val deterministic_nonce :
  #Client_context.wallet ->
  Signer_messages.Deterministic_nonce.Request.t ->
  require_auth:bool -> MBytes.t tzresult Lwt.t
(** [deterministic_nonce cctxt req ~require_auth] generates
    deterministically a nonce from [req.data]. *)

val deterministic_nonce_hash :
  #Client_context.wallet ->
  Signer_messages.Deterministic_nonce_hash.Request.t ->
  require_auth:bool -> MBytes.t tzresult Lwt.t
(** [deterministic_nonce_hash cctxt req ~require_auth] generates
    deterministically a nonce from [req.data] and returns the hash of
    this nonce. *)

val supports_deterministic_nonces :
  #Client_context.wallet ->
  Signature.public_key_hash ->
  bool tzresult Lwt.t
(** [supports_deterministic_nonces cctxt pkh] determines whether the
    the signer provides the determinsitic nonce functionality. *)
