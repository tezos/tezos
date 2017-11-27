(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)


module Directory :
  (module type of struct include RestoDirectory.MakeDirectory(RPC.Data) end)
include (module type of struct include RestoDirectory end)

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
  unit Directory.t ->
  server Lwt.t

(** Kill an RPC server. *)
val shutdown : server -> unit Lwt.t


(** Compatibility layer, to be removed ASAP. *)

type 'a directory = 'a Directory.t
val empty: 'a directory
val register:
  'prefix directory ->
  ('prefix, 'params, 'input, 'output) RPC.service ->
  ('params -> 'input -> [< ('output, unit) RestoDirectory.Answer.t ] Lwt.t) ->
  'prefix directory

val register0:
  unit directory ->
  (unit, unit, 'i, 'o) RPC.service ->
  ('i -> [< ('o, unit) Answer.t ] Lwt.t) ->
  unit directory

val register1:
  'prefix directory ->
  ('prefix, unit * 'a, 'i, 'o) RPC.service ->
  ('a -> 'i -> [< ('o, unit) Answer.t ] Lwt.t) ->
  'prefix directory

val register2:
  'prefix directory ->
  ('prefix, (unit * 'a) * 'b, 'i, 'o) RPC.service ->
  ('a -> 'b -> 'i -> [< ('o, unit) Answer.t ] Lwt.t) ->
  'prefix directory

val register_dynamic_directory1:
  ?descr:string ->
  'prefix directory ->
  ('prefix, unit * 'a) RPC.Path.path ->
  ('a -> (unit * 'a) directory Lwt.t) ->
  'prefix directory

