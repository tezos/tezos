(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

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
