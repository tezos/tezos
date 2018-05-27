(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

module Sign : sig

  module Request : sig
    type t = {
      pkh: Signature.Public_key_hash.t ;
      data: MBytes.t ;
    }
    val encoding : t Data_encoding.t
  end

  module Response : sig
    type t = Signature.t
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

module Request : sig

  type t =
    | Sign of Sign.Request.t
    | Public_key of Public_key.Request.t
  val encoding : t Data_encoding.t

end
