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

(** PVSS protocol, following

    see Schoenmakers, B., 1999:
    A simple publicly verifiable secret sharing scheme
    and its application to electronic voting. Lecture Notes in Computer Science,
    pp.148-164.

    see https://www.win.tue.nl/~berry/papers/crypto99.pdf

    The protocol is expressed as a functor parametrized by a cyclic group
    of prime order. Algebraic properties are enforced at the type level,
    whenever reasonably possible.

*)

module type CYCLIC_GROUP = sig

  type t
  include S.B58_DATA with type t := t
  include S.ENCODER with type t := t

  val name      : string
  module Z_m    : Znz.ZN
  val e         : t
  val g1        : t
  val g2        : t
  val ( * )     : t -> t -> t
  val (=)       : t -> t -> bool
  val pow       : t -> Z_m.t -> t

  (** Binary representation *)
  val to_bits   : t -> String.t
  val of_bits   : String.t -> t option

end

(** PVSS construction, based on a cyclic group G of prime order *)
module type PVSS = sig

  module type ENCODED = sig
    type t
    include S.B58_DATA with type t := t
    include S.ENCODER with type t := t
  end

  module Commitment : ENCODED
  module Encrypted_share : ENCODED
  module Clear_share : ENCODED

  module Public_key : ENCODED
  module Secret_key : sig
    include ENCODED
    val to_public_key : t -> Public_key.t
  end

  type proof
  val proof_encoding : proof Data_encoding.t

  val dealer_shares_and_proof:
    secret:Secret_key.t -> t:int -> public_keys:Public_key.t list ->
    (Encrypted_share.t list * Commitment.t list * proof)
  (** Lets a dealer share a secret with a set of participant by breaking it into
      pieces, encrypting it with the participant's public keys, and publishing
      these encrypted shares. Any t participants can reconstruct the secret. A
      zero-knowledge proof is produced showing that the  dealer correctly
      followed the protocol, making the protocol publicly verifiable. *)

  val check_dealer_proof:
    Encrypted_share.t list -> Commitment.t list -> proof:proof ->
    public_keys:Public_key.t list -> bool
  (** Checks the proof produced by the dealer, given the encrypted shares,
      the commitment list, the proof, and the participant's public keys. *)

  val reveal_share : Encrypted_share.t -> secret_key:Secret_key.t
    -> public_key:Public_key.t -> Clear_share.t * proof
  (** Lets a participant provably decrypt an encrypted share. *)

  val check_revealed_share:
    Encrypted_share.t -> Clear_share.t -> public_key:Public_key.t -> proof
    -> bool
  (** Checks that the participant honestly decrypted its share. *)

  val reconstruct: Clear_share.t list -> int list -> Public_key.t

end

module MakePvss : functor (G: CYCLIC_GROUP) -> PVSS
