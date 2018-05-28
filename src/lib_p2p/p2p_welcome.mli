(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(** Welcome worker.

    Accept incoming connections and add them to the pool.
*)

type t
(** Type of a welcome worker. *)

val run:
  ?addr:P2p_addr.t -> backlog:int ->
  ('msg, 'meta, 'meta_conn) P2p_pool.t -> P2p_addr.port -> t Lwt.t
(** [run ?addr ~backlog pool port] returns a running welcome worker
    adding connections into [pool] listening on [addr:port]. [backlog]
    is passed to [Lwt_unix.listen]. *)

val shutdown: t -> unit Lwt.t
(** [shutdown t] returns when [t] has completed shutdown. *)
