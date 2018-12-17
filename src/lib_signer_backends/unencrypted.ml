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

open Client_keys

let scheme = "unencrypted"

let title =
  "Built-in signer using raw unencrypted keys."

let description =
  "Please DO NOT USE this signer outside of test environments.\n\
   Valid secret key URIs are of the form\n\
  \ - unencrypted:<key>\n\
   where <key> is the secret key in Base58.\n\
   Valid public key URIs are of the form\n\
  \ - unencrypted:<public_key>\n\
   where <public_key> is the public key in Base58."

let secret_key sk_uri =
  Lwt.return
    (Signature.Secret_key.of_b58check (Uri.path (sk_uri : sk_uri :> Uri.t)))

let make_sk sk =
  Client_keys.make_sk_uri
    (Uri.make ~scheme ~path:(Signature.Secret_key.to_b58check sk) ())

let public_key ?interactive:(_) pk_uri =
  Lwt.return
    (Signature.Public_key.of_b58check (Uri.path (pk_uri : pk_uri :> Uri.t)))

let make_pk pk =
  Client_keys.make_pk_uri
    (Uri.make ~scheme ~path:(Signature.Public_key.to_b58check pk) ())

let neuterize sk_uri =
  secret_key sk_uri >>=? fun sk ->
  return (make_pk (Signature.Secret_key.to_public_key sk))

let public_key_hash ?interactive pk_uri =
  public_key ?interactive pk_uri >>=? fun pk ->
  return (Signature.Public_key.hash pk, Some pk)

let sign ?watermark sk_uri buf =
  secret_key sk_uri >>=? fun sk ->
  return (Signature.sign ?watermark sk buf)

let deterministic_nonce sk_uri buf =
  secret_key sk_uri >>=? fun sk ->
  return (Signature.deterministic_nonce sk buf)

let deterministic_nonce_hash sk_uri buf =
  secret_key sk_uri >>=? fun sk ->
  return (Signature.deterministic_nonce_hash sk buf)

let supports_deterministic_nonces _ = return_true
