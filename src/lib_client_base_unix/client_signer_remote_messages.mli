(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type error += Unkwnon_alias_key of string

type key = string

module Sign : sig
  module Request : sig
    type t = {
      key : key ;
      data: MBytes.t ;
    }
    val encoding : t Data_encoding.t
  end
  module Response : sig
    type t = {
      signature : Signature.t ;
    }
    val encoding : t Data_encoding.t
  end
end

module Public_key : sig
  module Request : sig
    type t = {
      key : key ;
    }
    val encoding : t Data_encoding.t
  end
  module Response : sig
    type t = {
      public_key : Signature.Public_key.t ;
    }
    val encoding : t Data_encoding.t
  end
end

module Request : sig
  type t =
    | Sign of Sign.Request.t
    | Public_key of Public_key.Request.t
  val encoding : t Data_encoding.t
end
