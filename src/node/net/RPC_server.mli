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

    The optional [pre_hook] is called with the path part of the URL
    before resolving each request, to delegate the answering to
    another resolution mechanism. Its result is ignored if the return
    code is [404]. The optional [post_hook] is called if both the
    [pre_hook] and the serviced answered with a [404] code. *)
val launch : int ->
  ?pre_hook: (string -> string RPC.Answer.answer Lwt.t) ->
  ?post_hook: (string -> string RPC.Answer.answer Lwt.t) ->
  unit RPC.directory -> server Lwt.t

(** Kill an RPC server. *)
val shutdown : server -> unit Lwt.t

(** Retrieve the root service of the server *)
val root_service : server -> unit RPC.directory

(** Change the root service of the server *)
val set_root_service : server -> unit RPC.directory -> unit
