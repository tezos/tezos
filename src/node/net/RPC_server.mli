(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(** Typed RPC services: server implementation. *)

(** A handle on the server worker. *)
type server

(** Promise a running RPC serve ; takes the port. To call
    an RPC at /p/a/t/h/ in the provided service, one must call the URI
    /call/p/a/t/h/. Calling /list/p/a/t/h/ will list the services
    prefixed by /p/a/t/h/, if any. Calling /schema/p/a/t/h/ will
    describe the input and output of the service, if it is
    callable. Calling /pipe will read a sequence of services to call in
    sequence from the request body, see {!pipe_encoding}.

    The arguments cors_allowed_origins and cors_allowed_headers define
    the cross-origin resource sharing using the headers
    Access-Control-Allow-Origin and Access-Control-Allow-Headers. The
    argument cors_allowed_headers sets the content of
    Access-Control-Allow-Headers. Since you cannot have multiple
    values for Access-Control-Allow-Origin, the server accepts a list
    in cors_allowed_origins and matches it against the origin of the
    incoming request; then returns the longest element of the passed
    list as the content of Access-Control-Allow-Origin.

    The optional [pre_hook] is called with the path part of the URL
    before resolving each request, to delegate the answering to
    another resolution mechanism. Its result is ignored if the return
    code is [404]. The optional [post_hook] is called if both the
    [pre_hook] and the serviced answered with a [404] code. *)
val launch : Conduit_lwt_unix.server ->
  ?pre_hook: (string -> string RPC.Answer.answer Lwt.t) ->
  ?post_hook: (string -> string RPC.Answer.answer Lwt.t) ->
  unit RPC.directory ->
  string list ->
  string list ->
  server Lwt.t

(** Kill an RPC server. *)
val shutdown : server -> unit Lwt.t

(** Retrieve the root service of the server *)
val root_service : server -> unit RPC.directory

(** Change the root service of the server *)
val set_root_service : server -> unit RPC.directory -> unit
