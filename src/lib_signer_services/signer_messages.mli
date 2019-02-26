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

module type Authenticated_request = sig
  type t = {
    pkh: Signature.Public_key_hash.t ;
    data: MBytes.t ;
    signature: Signature.t option ;
  }
  val to_sign:
    pkh: Signature.Public_key_hash.t ->
    data: MBytes.t ->
    MBytes.t
  val encoding : t Data_encoding.t
end

module Sign : sig

  module Request : Authenticated_request

  module Response : sig
    type t = Signature.t
    val encoding : t Data_encoding.t
  end

end

module Deterministic_nonce : sig

  module Request : Authenticated_request

  module Response : sig
    type t = MBytes.t
    val encoding : t Data_encoding.t
  end

end

module Deterministic_nonce_hash : sig

  module Request : Authenticated_request

  module Response : sig
    type t = MBytes.t
    val encoding : t Data_encoding.t
  end

end

module Supports_deterministic_nonces : sig

  module Request : sig
    type t = Signature.Public_key_hash.t
    val encoding : t Data_encoding.t
  end

  module Response : sig
    type t = bool
    val encoding : t Data_encoding.t
  end

end

module Public_key : sig

  module Request : sig
    type t = Signature.Public_key_hash.t
    val encoding : t Data_encoding.t
  end

  module Response : sig
    type t = Signature.Public_key.t
    val encoding : t Data_encoding.t
  end

end

module Authorized_keys : sig

  module Response : sig
    type t =
      | No_authentication
      | Authorized_keys of Signature.Public_key_hash.t list
    val encoding : t Data_encoding.t
  end

end


module Request : sig

  type t =
    | Sign of Sign.Request.t
    | Public_key of Public_key.Request.t
    | Authorized_keys
    | Deterministic_nonce of Deterministic_nonce.Request.t
    | Deterministic_nonce_hash of Deterministic_nonce_hash.Request.t
    | Supports_deterministic_nonces of Supports_deterministic_nonces.Request.t
  val encoding : t Data_encoding.t

end
