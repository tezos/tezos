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

open Secp256k1_group

module G : Pvss.CYCLIC_GROUP = struct

  module Z_m = struct
    include Group.Scalar
    let n = Group.order
    let ( + )   = Group.Scalar.add
    let ( * )   = Group.Scalar.mul
    let ( - )   = Group.Scalar.sub
    let ( = )   = Group.Scalar.equal
    let inv     = Group.Scalar.inverse
  end

  include Group
  let name = "secp256k1"

  (* This pvss algorithm assumes the public keys of the participants receiving
     shares are based on g2, so we set g2 to Group.g to match regular Secp256k1
     public keys.
  *)
  let g1 = Group.h
  let g2 = Group.g

  (* We use a multiplicative notation in the pvss module, but
     secp256k1 usually uses an additive notation. *)
  let ( * ) = Group.(( + ))
  let pow x n = Group.mul n x

  let of_bits b =
    try
      Some (Group.of_bits_exn b)
    with _ -> None

end

include Pvss.MakePvss (G)
