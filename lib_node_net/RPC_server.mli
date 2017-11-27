(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
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

type media_type = {
  name: string ;
  construct: 'a. 'a Data_encoding.t -> 'a -> string ;
  destruct: 'a. 'a Data_encoding.t -> string -> ('a, string) result ;
}

val json : media_type
val octet_stream : media_type

(** A handle on the server worker. *)
type server

(** Promise a running RPC server.*)
val launch :
  ?host:string ->
  ?cors:cors ->
  media_types:media_type list ->
  Conduit_lwt_unix.server ->
  unit RPC.Directory.t ->
  server Lwt.t

(** Kill an RPC server. *)
val shutdown : server -> unit Lwt.t
