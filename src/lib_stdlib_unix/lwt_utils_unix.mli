(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Error_monad

val read_bytes:
  ?pos:int -> ?len:int -> Lwt_unix.file_descr -> bytes -> unit Lwt.t

val read_mbytes:
  ?pos:int -> ?len:int -> Lwt_unix.file_descr -> MBytes.t -> unit Lwt.t

val write_bytes:
  ?pos:int -> ?len:int -> Lwt_unix.file_descr -> bytes -> unit Lwt.t
val write_mbytes:
  ?pos:int -> ?len:int -> Lwt_unix.file_descr -> MBytes.t -> unit Lwt.t

val remove_dir: string -> unit Lwt.t
val create_dir: ?perm:int -> string -> unit Lwt.t
val read_file: string -> string Lwt.t
val create_file: ?perm:int -> string -> string -> unit Lwt.t

val with_tempdir: string -> (string -> 'a Lwt.t) -> 'a Lwt.t

val safe_close: Lwt_unix.file_descr -> unit Lwt.t

val getaddrinfo:
  passive:bool ->
  node:string -> service:string ->
  (Ipaddr.V6.t * int) list Lwt.t

(** [getpass ()] reads a password from stdio while setting-up the
    terminal to not display the password being typed. *)
val getpass : unit -> string

module Json : sig

  (** Loads a JSON file in memory *)
  val read_file : string -> Data_encoding.json tzresult Lwt.t

  (** (Over)write a JSON file from in memory data *)
  val write_file : string -> Data_encoding.json -> unit tzresult Lwt.t

end

module Protocol : sig

  val read_dir: string -> (Protocol_hash.t * Protocol.t) tzresult Lwt.t

  val write_dir: string -> ?hash:Protocol_hash.t -> Protocol.t -> unit tzresult Lwt.t

end

module Socket : sig

  type addr =
    | Unix of string
    | Tcp of string * int

  val connect: addr -> Lwt_unix.file_descr tzresult Lwt.t
  val bind: ?backlog:int -> addr -> Lwt_unix.file_descr tzresult Lwt.t

  type error +=
    | Encoding_error
    | Decoding_error

  val send:
    Lwt_unix.file_descr -> 'a Data_encoding.t -> 'a -> unit tzresult Lwt.t
  val recv:
    Lwt_unix.file_descr -> 'a Data_encoding.t -> 'a tzresult Lwt.t

end

val retry:
  ?log:('error -> unit Lwt.t) ->
  ?n:int ->
  ?sleep:float ->
  (unit -> ('a, 'error) result Lwt.t) -> ('a, 'error) result Lwt.t

