(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

module type LOGGER = sig
  type request
  val log_empty_request: Uri.t -> request Lwt.t
  val log_request:
    ?media:Media_type.t -> 'a Data_encoding.t ->
    Uri.t -> string -> request Lwt.t
  val log_response:
    request -> ?media:Media_type.t -> 'a Data_encoding.t ->
    Cohttp.Code.status_code -> string Lwt.t Lazy.t -> unit Lwt.t
end

type logger = (module LOGGER)

val null_logger: logger
val timings_logger: Format.formatter -> logger
val full_logger: Format.formatter -> logger

type ('o, 'e) rest_result =
  [ `Ok of 'o
  | `Conflict of 'e
  | `Error of 'e
  | `Forbidden of 'e
  | `Not_found of 'e
  | `Unauthorized of 'e ] tzresult

type content_type = (string * string)
type raw_content = Cohttp_lwt.Body.t * content_type option
type content = Cohttp_lwt.Body.t * content_type option * Media_type.t option

type rest_error =
  | Empty_answer
  | Connection_failed of string
  | Not_found
  | Bad_request of string
  | Method_not_allowed of RPC_service.meth list
  | Unsupported_media_type of string option
  | Not_acceptable of { proposed: string ; acceptable: string }
  | Unexpected_status_code of { code: Cohttp.Code.status_code ;
                                content: string ;
                                media_type: string option }
  | Unexpected_content_type of { received: string ;
                                 acceptable: string list ;
                                 body : string }
  | Unexpected_content of { content: string ;
                            media_type: string ;
                            error: string }
  | Generic_error (* temporary *)

type error +=
  | Request_failed of { meth: RPC_service.meth ;
                        uri: Uri.t ;
                        error: rest_error }

val generic_call :
  ?logger:logger ->
  ?accept:Media_type.t list ->
  ?body:Cohttp_lwt.Body.t ->
  ?media:Media_type.t ->
  [< RPC_service.meth ] ->
  Uri.t -> (content, content) rest_result Lwt.t

val generic_json_call :
  ?logger:logger ->
  ?body:Data_encoding.json ->
  [< RPC_service.meth ] -> Uri.t ->
  (Data_encoding.json, Data_encoding.json option) rest_result Lwt.t

val call_service :
  Media_type.t list ->
  ?logger:logger ->
  base:Uri.t ->
  ([< Resto.meth ], unit, 'p, 'q, 'i, 'o, 'e) RPC_service.t ->
  'p -> 'q -> 'i -> 'o tzresult Lwt.t

val call_streamed_service :
  Media_type.t list ->
  ?logger:logger ->
  base:Uri.t ->
  ([< Resto.meth ], unit, 'p, 'q, 'i, 'o, 'e) RPC_service.t ->
  on_chunk: ('o -> unit) ->
  on_close: (unit -> unit) ->
  'p -> 'q -> 'i -> (unit -> unit) tzresult Lwt.t
