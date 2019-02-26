(**************************************************************************)
(*  ocplib-resto                                                          *)
(*  Copyright (C) 2016, OCamlPro.                                         *)
(*                                                                        *)
(*    All rights reserved.  This file is distributed under the terms      *)
(*    of the GNU Lesser General Public License version 2.1, with the      *)
(*    special exception on linking described in the file LICENSE.         *)
(*                                                                        *)
(**************************************************************************)

(** Typed RPC services: client implementation. *)

module Make (Encoding : Resto.ENCODING) : sig

  module Service : (module type of (struct include Resto.MakeService(Encoding) end))

  type content_type = (string * string)
  type raw_content = Cohttp_lwt.Body.t * content_type option
  type content =
    Cohttp_lwt.Body.t * content_type option * Media_type.Make(Encoding).t option

  type ('o, 'e) generic_rest_result =
    [ `Ok of 'o option
    | `Conflict of 'e
    | `Error of 'e
    | `Forbidden of 'e
    | `Not_found of 'e
    | `Unauthorized of 'e
    | `Bad_request of string
    | `Method_not_allowed of string list
    | `Unsupported_media_type
    | `Not_acceptable of string
    | `Unexpected_status_code of Cohttp.Code.status_code * content
    | `Connection_failed of string
    | `OCaml_exception of string
    | `Unauthorized_host of string option ]

  module type LOGGER = sig
    type request
    val log_empty_request: Uri.t -> request Lwt.t
    val log_request:
      ?media:Media_type.Make(Encoding).t -> 'a Encoding.t ->
      Uri.t -> string -> request Lwt.t
    val log_response:
      request -> ?media:Media_type.Make(Encoding).t -> 'a Encoding.t ->
      Cohttp.Code.status_code -> string Lwt.t Lazy.t -> unit Lwt.t
  end

  type logger = (module LOGGER)

  val null_logger: logger
  val timings_logger: Format.formatter -> logger
  val full_logger: Format.formatter -> logger

  val generic_call:
    [< Resto.meth ] ->
    ?logger:logger ->
    ?headers:(string * string) list ->
    ?accept:Media_type.Make(Encoding).t list ->
    ?body:Cohttp_lwt.Body.t ->
    ?media:Media_type.Make(Encoding).t ->
    Uri.t -> (content, content) generic_rest_result Lwt.t

  type ('o, 'e) service_result =
    [ ('o, 'e option) generic_rest_result
    | `Unexpected_content_type of raw_content
    | `Unexpected_content of (string * Media_type.Make(Encoding).t) * string
    | `Unexpected_error_content_type of raw_content
    | `Unexpected_error_content of (string * Media_type.Make(Encoding).t) * string ]

  val call_service:
    Media_type.Make(Encoding).t list ->
    ?logger:logger ->
    ?headers:(string * string) list ->
    ?base:Uri.t ->
    ([< Resto.meth ], unit, 'p, 'q, 'i, 'o, 'e) Service.t ->
    'p -> 'q -> 'i -> (Resto.meth * Uri.t * ('o, 'e) service_result) Lwt.t

  val call_streamed_service:
    Media_type.Make(Encoding).t list ->
    ?logger:logger ->
    ?headers:(string * string) list ->
    ?base:Uri.t ->
    ([< Resto.meth ], unit, 'p, 'q, 'i, 'o, 'e) Service.t ->
    on_chunk: ('o -> unit) ->
    on_close: (unit -> unit) ->
    'p -> 'q -> 'i ->
    (Resto.meth * Uri.t * (unit -> unit, 'e) service_result) Lwt.t

end
