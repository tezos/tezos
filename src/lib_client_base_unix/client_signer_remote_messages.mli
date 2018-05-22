(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type error +=
  | Encoding_error
  | Decoding_error
  | Unkwnon_alias_key of string
  | Unkwnon_request_kind

type key = string

module Connection : sig
  type t = Lwt_unix.file_descr
  val bind : Uri.t -> t Lwt.t
  val connect : Uri.t -> t Lwt.t
  val read : len:int -> t -> MBytes.t -> unit tzresult Lwt.t
  val write : t -> MBytes.t -> unit tzresult Lwt.t
end

module Sign : sig
  module Request : sig
    type t = {
      key : string ;
      data: MBytes.t ;
    }
    val encoding : t Data_encoding.t
  end
  module Response : sig
    type t = {
      signature : Signature.t ;
    }
    val encoding : t tzresult Data_encoding.t
  end
end

module Public_key : sig
  module Request : sig
    type t = {
      key : string
    }
    val encoding : t Data_encoding.t
  end
  module Response : sig
    type t = {
      public_key : Signature.Public_key.t
    }
    val encoding : t tzresult Data_encoding.t
  end
end

module Request : sig
  type t =
    | Sign of Sign.Request.t
    | Public_key of Public_key.Request.t
  val encoding : t Data_encoding.t
end

val send : Connection.t -> 'a Data_encoding.t -> 'a -> unit tzresult Lwt.t
val recv : Connection.t -> 'a Data_encoding.t -> 'a tzresult Lwt.t
