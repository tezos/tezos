(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)


module Directory :
  (module type of struct include Resto_directory.Make(RPC.Data) end)
include (module type of struct include Resto_directory end)

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

val register:
  'prefix Directory.t ->
  ([`POST], 'prefix, 'params, unit, 'input, 'output, unit) RPC.Service.t ->
  ('params -> 'input -> [< ('output, unit) RestoDirectory.Answer.t ] Lwt.t) ->
  'prefix Directory.t

val register0:
  unit Directory.t ->
  ([`POST], unit, unit, unit, 'i, 'o, unit) RPC.Service.t ->
  ('i -> [< ('o, unit) Answer.t ] Lwt.t) ->
  unit Directory.t

val register1:
  'prefix Directory.t ->
  ([`POST], 'prefix, unit * 'a, unit, 'i, 'o, unit) RPC.Service.t ->
  ('a -> 'i -> [< ('o, unit) Answer.t ] Lwt.t) ->
  'prefix Directory.t

val register2:
  'prefix Directory.t ->
  ([`POST], 'prefix, (unit * 'a) * 'b, unit, 'i, 'o, unit) RPC.Service.t ->
  ('a -> 'b -> 'i -> [< ('o, unit) Answer.t ] Lwt.t) ->
  'prefix Directory.t

val register_dynamic_directory1:
  ?descr:string ->
  'prefix Directory.t ->
  ('prefix, unit * 'a) RPC.Path.path ->
  ('a -> (unit * 'a) Directory.t Lwt.t) ->
  'prefix Directory.t

