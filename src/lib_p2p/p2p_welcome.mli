(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(** Welcome worker. Accept incoming connections and add them to its
    connection pool. *)

type t
(** Type of a welcome worker, parametrized like a
    [P2p_connection_pool.pool]. *)

val run:
  backlog:int ->
  ('msg, 'meta) P2p_pool.t ->
  ?addr:P2p_addr.t -> P2p_addr.port -> t Lwt.t
(** [run ~backlog ~addr pool port] returns a running welcome worker
    feeding [pool] listening at [(addr, port)]. [backlog] is the
    argument passed to [Lwt_unix.accept]. *)

val shutdown: t -> unit Lwt.t
