(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(** Typed RPC services: server implementation. *)

type cors = {
  allowed_headers : string list ;
  allowed_origins : string list ;
}

(** A handle on the server worker. *)
type server

(** Promise a running RPC server.*)
val launch :
  ?host:string ->
  ?cors:cors ->
  media_types:Media_type.t list ->
  Conduit_lwt_unix.server ->
  unit RPC_directory.t ->
  server Lwt.t

(** Kill an RPC server. *)
val shutdown : server -> unit Lwt.t
