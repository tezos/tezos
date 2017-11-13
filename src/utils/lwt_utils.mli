(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
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

module Idle_waiter : sig

  type t
  (** A lightweight scheduler to run tasks concurrently as well as
      special callbacks that must be run in mutual exclusion with the
      tasks (and each other). *)

  val create : unit -> t
  (** Creates a new task / idle callback scheduler *)

  val task : t -> (unit -> 'a Lwt.t) -> 'a Lwt.t
  (** Schedule a task to be run as soon as no idle callbacks is
      running, or as soon as the next idle callback has been run if it
      was scheduled by {!force_idle}. *)

  val when_idle : t -> (unit -> 'a Lwt.t) -> 'a Lwt.t
  (** Runs a callback as soon as no task is running. Does not prevent
      new tasks from being scheduled, the calling code should ensure
      that some idle time will eventually come. Calling this function
      from inside the callback will result in a dead lock. *)

  val force_idle : t -> (unit -> 'a Lwt.t) -> 'a Lwt.t
  (** Runs a callback as soon as possible. Lets all current tasks
      finish, but postpones all new tasks until the end of the
      callback. Calling this function from inside the callback will
      result in a dead lock. *)

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

val unless: bool -> (unit -> unit Lwt.t) -> unit Lwt.t

module Lock_file : sig
  val create :
    ?close_on_exec:bool ->
    ?unlink_on_exit:bool ->
    string -> unit tzresult Lwt.t

  val blocking_create :
    ?timeout:float ->
    ?close_on_exec:bool ->
    ?unlink_on_exit:bool ->
    string -> unit tzresult Lwt.t

  val is_locked : string -> bool tzresult Lwt.t
  val get_pid : string -> int tzresult Lwt.t
end

val getaddrinfo:
  passive:bool ->
  node:string -> service:string ->
  (Ipaddr.V6.t * int) list Lwt.t
