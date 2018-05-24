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

type path =
  | Unix of string
  | Tcp of string * string

module Connection : sig
  type t = Lwt_unix.file_descr
  val bind : path -> (t * string) tzresult Lwt.t
  val connect : path -> t tzresult Lwt.t
  val read : len:int -> t -> MBytes.t -> unit tzresult Lwt.t
  val write : t -> MBytes.t -> unit tzresult Lwt.t
end

val send : Connection.t -> 'a Data_encoding.t -> 'a -> unit tzresult Lwt.t
val recv : Connection.t -> 'a Data_encoding.t -> 'a tzresult Lwt.t
