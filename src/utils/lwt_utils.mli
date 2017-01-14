(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

val may: f:('a -> unit Lwt.t) -> 'a option -> unit Lwt.t

val never_ending: 'a Lwt.t

val canceler : unit ->
  (unit -> unit Lwt.t) *
  (unit -> unit Lwt.t) *
  ((unit -> unit Lwt.t) -> unit)

module Canceler : sig

  type t
  val create : unit -> t
  val cancel : t -> unit Lwt.t
  val cancelation : t -> unit Lwt.t
  val on_cancel : t -> (unit -> unit Lwt.t) -> unit
  val canceled : t -> bool

end

val worker:
  string ->
  run:(unit -> unit Lwt.t) ->
  cancel:(unit -> unit Lwt.t) ->
  unit Lwt.t

val trigger: unit -> (unit -> unit) * (unit -> unit Lwt.t)
val queue: unit -> ('a -> unit) * (unit -> 'a list Lwt.t)
val sort: ('a -> 'a -> int Lwt.t) -> 'a list -> 'a list Lwt.t

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
val create_file: ?perm:int -> string -> string -> unit Lwt.t

val safe_close: Lwt_unix.file_descr -> unit Lwt.t

open Error_monad

type error += Canceled
val protect :
  ?on_error:(error list -> 'a tzresult Lwt.t) ->
  ?canceler:Canceler.t ->
  (unit -> 'a tzresult Lwt.t) -> 'a tzresult Lwt.t

type error += Timeout
val with_timeout:
  ?canceler:Canceler.t ->
  float -> (Canceler.t -> 'a tzresult Lwt.t) -> 'a tzresult Lwt.t

