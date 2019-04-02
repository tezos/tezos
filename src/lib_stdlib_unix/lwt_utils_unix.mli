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

  val read_dir: string -> (Protocol_hash.t option * Protocol.t) tzresult Lwt.t

  val write_dir: string -> ?hash:Protocol_hash.t -> Protocol.t -> unit tzresult Lwt.t

end

module Socket : sig

  type addr =
    | Unix of string
    | Tcp of string * string * Unix.getaddrinfo_option list

  val connect:
    ?timeout:float -> addr -> Lwt_unix.file_descr tzresult Lwt.t
  (** [connect ?timeout addr] tries connecting to [addr] and returns
      the resulting socket file descriptor on success. When using TCP,
      [Unix.getaddrinfo] is used to resolve the hostname and service
      (port). The different socket addresses returned by
      [Unix.getaddrinfo] are tried sequentially, and the [?timeout]
      argument (default: 5s) governs how long it waits to get a
      connection. If a connection is not obtained in less than
      [?timeout], the connection is canceled and and the next socket
      address (if it exists) is tried. *)

  val bind:
    ?backlog:int -> addr -> Lwt_unix.file_descr list tzresult Lwt.t

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

