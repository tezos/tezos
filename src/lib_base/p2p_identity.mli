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

type t = {
  peer_id : P2p_peer.Id.t ;
  public_key : Crypto_box.public_key ;
  secret_key : Crypto_box.secret_key ;
  proof_of_work_stamp : Crypto_box.nonce ;
}
(** Type of an identity, comprising a peer_id, a crypto keypair, and a
    proof of work stamp with enough difficulty so that the network
    accept this identity as genuine. *)

val encoding : t Data_encoding.t

val generate : Crypto_box.target -> t
(** [generate target] is a freshly minted identity whose proof of
    work stamp difficulty is at least equal to [target]. *)

val generate_with_animation :
  Format.formatter -> Crypto_box.target -> t
(** [generate_with_animation ppf target] is a freshly minted identity
    whose proof of work stamp difficulty is at least equal to [target]. *)

