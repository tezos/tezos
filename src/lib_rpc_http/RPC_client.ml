(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

module Client = Resto_cohttp.Client.Make(RPC_encoding)

module type LOGGER = Client.LOGGER
type logger = (module LOGGER)
let null_logger = Client.null_logger
let timings_logger = Client.timings_logger
let full_logger = Client.full_logger

type rpc_error =
  | Empty_answer
  | Connection_failed of string
  | Bad_request of string
  | Method_not_allowed of RPC_service.meth list
  | Unsupported_media_type of string option
  | Not_acceptable of { proposed: string ; acceptable: string }
  | Unexpected_status_code of { code: Cohttp.Code.status_code ;
                                content: string ;
                                media_type: string option }
  | Unexpected_content_type of { received: string ;
                                 acceptable: string list ;
                                 body : string}
  | Unexpected_content of { content: string ;
                            media_type: string ;
                            error: string }
  | OCaml_exception of string
  | Unauthorized_host of string option

let rpc_error_encoding =
  let open Data_encoding in
  union
    [ case (Tag  0)
        ~title:"Empty_answer"
        (obj1
           (req "kind" (constant "empty_answer")))
        (function Empty_answer -> Some () | _ -> None)
        (fun () -> Empty_answer) ;
      case (Tag  1)
        ~title:"Connection_failed"
        (obj2
           (req "kind" (constant "connection_failed"))
           (req "message" string))
        (function Connection_failed msg -> Some ((), msg) | _ -> None)
        (function (), msg -> Connection_failed msg) ;
      case (Tag  2)
        ~title:"Bad_request"
        (obj2
           (req "kind" (constant "bad_request"))
           (req "message" string))
        (function Bad_request msg -> Some ((), msg) | _ -> None)
        (function (), msg -> Bad_request msg) ;
      case (Tag  3)
        ~title:"Method_not_allowed"
        (obj2
           (req "kind" (constant "method_not_allowed"))
           (req "allowed" (list RPC_service.meth_encoding)))
        (function Method_not_allowed meths -> Some ((), meths) | _ -> None)
        (function ((), meths) -> Method_not_allowed meths) ;
      case (Tag  4)
        ~title:"Unsupported_media_type"
        (obj2
           (req "kind" (constant "unsupported_media_type"))
           (opt "content_type" string))
        (function Unsupported_media_type m -> Some ((), m) | _ -> None)
        (function ((), m) -> Unsupported_media_type m) ;
      case (Tag  5)
        ~title:"Not_acceptable"
        (obj3
           (req "kind" (constant "not_acceptable"))
           (req "proposed" string)
           (req "acceptable" string))
        (function
          | Not_acceptable { proposed ; acceptable } ->
              Some ((), proposed, acceptable)
          | _ -> None)
        (function ((), proposed, acceptable) ->
           Not_acceptable { proposed ; acceptable }) ;
      case (Tag  6)
        ~title:"Unexpected_status_code"
        (obj4
           (req "kind" (constant "unexpected_status_code"))
           (req "code" uint16)
           (req "content" string)
           (opt "media_type" string))
        (function
          | Unexpected_status_code { code ; content ; media_type } ->
              Some ((), Cohttp.Code.code_of_status code, content, media_type)
          | _ -> None)
        (function ((), code, content, media_type) ->
           let code = Cohttp.Code.status_of_code code in
           Unexpected_status_code { code ; content ; media_type }) ;
      case (Tag  7)
        ~title:"Unexpected_content_type"
        (obj4
           (req "kind" (constant "unexpected_content_type"))
           (req "received" string)
           (req "acceptable" (list string))
           (req "body" string))
        (function
          | Unexpected_content_type { received ; acceptable ; body } ->
              Some ((), received, acceptable, body)
          | _ -> None)
        (function ((), received, acceptable, body) ->
           Unexpected_content_type { received ; acceptable ; body }) ;
      case (Tag  8)
        ~title:"Unexpected_content"
        (obj4
           (req "kind" (constant "unexpected_content"))
           (req "content" string)
           (req "media_type" string)
           (req "error" string))
        (function
          | Unexpected_content { content ; media_type ; error  } ->
              Some ((), content, media_type, error)
          | _ -> None)
        (function ((), content, media_type, error) ->
           Unexpected_content { content ; media_type ; error  }) ;
      case (Tag  9)
        ~title:"OCaml_exception"
        (obj2
           (req "kind" (constant "ocaml_exception"))
           (req "content" string))
        (function OCaml_exception msg -> Some ((), msg) | _ -> None)
        (function ((), msg) -> OCaml_exception msg) ;
    ]

let pp_rpc_error ppf err =
  match err with
  | Empty_answer ->
      Format.fprintf ppf
        "The server answered with an empty response."
  | Connection_failed msg ->
      Format.fprintf ppf
        "Unable to connect to the node: \"%s\"" msg
  | Bad_request msg ->
      Format.fprintf ppf
        "@[<v 2>Oups! It looks like we forged an invalid HTTP request.@,%s@]"
        msg
  | Method_not_allowed meths ->
      Format.fprintf ppf
        "@[<v 2>The requested service only accepts the following method:@ %a@]"
        (Format.pp_print_list
           (fun ppf m -> Format.pp_print_string ppf (RPC_service.string_of_meth m)))
        meths
  | Unsupported_media_type None ->
      Format.fprintf ppf
        "@[<v 2>The server wants to known the media type we used.@]"
  | Unsupported_media_type (Some media) ->
      Format.fprintf ppf
        "@[<v 2>The server does not support the media type we used: %s.@]"
        media
  | Not_acceptable { proposed ; acceptable } ->
      Format.fprintf ppf
        "@[<v 2>No intersection between the media types we accept and \
        \ the ones the server is able to send.@,\
        \ We proposed: %s@,\
        \ The server is only able to serve: %s."
        proposed acceptable
  | Unexpected_status_code { code ; content ; _ } ->
      Format.fprintf ppf
        "@[<v 2>Unexpected error %d:@,%S"
        (Cohttp.Code.code_of_status code) content
  | Unexpected_content_type { received ; acceptable = _ ; body } ->
      Format.fprintf ppf
        "@[<v 0>The server answered with a media type we do not understand: %s.@,\
         The response body was:@,\
         %s@]" received body
  | Unexpected_content { content ; media_type ; error } ->
      Format.fprintf ppf
        "@[<v 2>Failed to parse the answer (%s):@,@[<v 2>error:@ %s@]@,@[<v 2>content:@ %S@]@]"
        media_type error content
  | OCaml_exception msg ->
      Format.fprintf ppf
        "@[<v 2>The server failed with an unexpected exception:@ %s@]"
        msg
  | Unauthorized_host host ->
      Format.fprintf ppf
        "@[<v 2>The server refused connection to host \"%s\", \
         please check the node settings for CORS allowed origins.@]"
        (Option.unopt ~default:"" host)

type error +=
  | Request_failed of { meth: RPC_service.meth ;
                        uri: Uri.t ;
                        error: rpc_error }

let uri_encoding =
  let open Data_encoding in
  conv
    Uri.to_string
    Uri.of_string
    string

let () =
  register_error_kind `Permanent
    ~id:"rpc_client.request_failed"
    ~title:""
    ~description:""
    ~pp:(fun ppf (meth, uri, error) ->
        Format.fprintf ppf
          "@[<v 2>Rpc request failed:@ \
          \ - meth: %s@ \
          \ - uri: %s@ \
          \ - error: %a@]"
          (RPC_service.string_of_meth meth)
          (Uri.to_string uri)
          pp_rpc_error error)
    Data_encoding.(obj3
                     (req "meth" RPC_service.meth_encoding)
                     (req "uri" uri_encoding)
                     (req "error" rpc_error_encoding))
    (function
      | Request_failed { uri ; error ; meth } -> Some (meth, uri, error)
      | _ -> None)
    (fun (meth, uri, error) -> Request_failed { uri ; meth ; error })

let request_failed meth uri error =
  let meth = ( meth : [< RPC_service.meth ] :> RPC_service.meth) in
  fail (Request_failed { meth ; uri ; error })

type content_type = (string * string)
type content = Cohttp_lwt.Body.t * content_type option * Media_type.t option

let generic_call ?logger ?headers ?accept ?body ?media meth uri : (content, content) RPC_context.rest_result Lwt.t =
  Client.generic_call meth ?logger ?headers ?accept ?body ?media uri >>= function
  | `Ok (Some v) -> return (`Ok v)
  | `Ok None -> request_failed meth uri Empty_answer
  | `Conflict _
  | `Error _
  | `Forbidden _
  | `Unauthorized _
  | `Not_found _ as v -> return v
  | `Unexpected_status_code (code, (content, _, media_type)) ->
      let media_type = Option.map media_type ~f:Media_type.name in
      Cohttp_lwt.Body.to_string content >>= fun content ->
      request_failed meth uri
        (Unexpected_status_code { code ; content ; media_type })
  | `Method_not_allowed allowed ->
      let allowed = List.filter_map RPC_service.meth_of_string allowed in
      request_failed meth uri (Method_not_allowed allowed)
  | `Unsupported_media_type ->
      let media = Option.map media ~f:Media_type.name in
      request_failed meth uri (Unsupported_media_type media)
  | `Not_acceptable acceptable ->
      let proposed =
        Option.unopt_map accept ~default:"" ~f:Media_type.accept_header in
      request_failed meth uri (Not_acceptable { proposed ; acceptable })
  | `Bad_request msg ->
      request_failed meth uri (Bad_request msg)
  | `Connection_failed msg ->
      request_failed meth uri (Connection_failed msg)
  | `OCaml_exception msg ->
      request_failed meth uri (OCaml_exception msg)
  | `Unauthorized_host host ->
      request_failed meth uri (Unauthorized_host host)

let handle_error meth uri (body, media, _) f =
  Cohttp_lwt.Body.is_empty body >>= fun empty ->
  if empty then
    return (f None)
  else
    match media with
    | Some ("application", "json") | None -> begin
        Cohttp_lwt.Body.to_string body >>= fun body ->
        match Data_encoding.Json.from_string body with
        | Ok body -> return (f (Some body))
        | Error msg ->
            request_failed meth uri
              (Unexpected_content { content = body ;
                                    media_type = Media_type.(name json) ;
                                    error = msg })
      end
    | Some (l, r) ->
        Cohttp_lwt.Body.to_string body >>= fun body ->
        request_failed meth uri
          (Unexpected_content_type { received = l^"/"^r ;
                                     acceptable = [Media_type.(name json)] ;
                                     body })

let generic_json_call ?logger ?headers ?body meth uri : (Data_encoding.json, Data_encoding.json option) RPC_context.rest_result Lwt.t =
  let body =
    Option.map body ~f:begin fun b ->
      (Cohttp_lwt.Body.of_string (Data_encoding.Json.to_string b))
    end in
  let media = Media_type.json in
  generic_call meth ?logger ?headers ~accept:Media_type.[bson ; json] ?body ~media uri >>=? function
  | `Ok (body, (Some ("application", "json") | None), _) -> begin
      Cohttp_lwt.Body.to_string body >>= fun body ->
      match Data_encoding.Json.from_string body with
      | Ok json -> return (`Ok json)
      | Error msg ->
          request_failed meth uri
            (Unexpected_content { content = body ;
                                  media_type = Media_type.(name json) ;
                                  error = msg })
    end
  | `Ok (body, Some ("application", "bson"), _) -> begin
      Cohttp_lwt.Body.to_string body >>= fun body ->
      match Json_repr_bson.bytes_to_bson ~laziness:false ~copy:false
              (Bytes.unsafe_of_string body) with
      | exception Json_repr_bson.Bson_decoding_error (msg, _, pos) ->
          let error = Format.asprintf "(at offset: %d) %s" pos msg in
          request_failed meth uri
            (Unexpected_content { content = body ;
                                  media_type = Media_type.(name bson) ;
                                  error })
      | bson ->
          return (`Ok (Json_repr.convert
                         (module Json_repr_bson.Repr)
                         (module Json_repr.Ezjsonm)
                         bson))
    end
  | `Ok (body, Some (l, r), _) ->
      Cohttp_lwt.Body.to_string body >>= fun body ->
      request_failed meth uri
        (Unexpected_content_type { received = l^"/"^r ;
                                   acceptable = [Media_type.(name json)] ;
                                   body })
  | `Conflict body ->
      handle_error meth uri body (fun v -> `Conflict v)
  | `Error body ->
      handle_error meth uri body (fun v -> `Error v)
  | `Forbidden body ->
      handle_error meth uri body (fun v -> `Forbidden v)
  | `Not_found body ->
      handle_error meth uri body (fun v -> `Not_found v)
  | `Unauthorized body ->
      handle_error meth uri body (fun v -> `Unauthorized v)

let handle accept (meth, uri, ans) =
  match ans with
  | `Ok (Some v) -> return v
  | `Ok None -> request_failed meth uri Empty_answer
  | `Not_found None -> fail (RPC_context.Not_found { meth ; uri })
  | `Conflict (Some err) | `Error (Some err)
  | `Forbidden (Some err) | `Unauthorized (Some err)
  | `Not_found (Some err) -> Lwt.return_error err
  | `Conflict None | `Error None | `Forbidden None | `Unauthorized None ->
      fail (RPC_context.Generic_error { meth ; uri })
  | `Unexpected_status_code (code, (content, _, media_type)) ->
      let media_type = Option.map media_type ~f:Media_type.name in
      Cohttp_lwt.Body.to_string content >>= fun content ->
      request_failed meth uri (Unexpected_status_code { code ; content ; media_type })
  | `Method_not_allowed allowed ->
      let allowed = List.filter_map RPC_service.meth_of_string allowed in
      request_failed meth uri (Method_not_allowed allowed)
  | `Unsupported_media_type ->
      let name =
        match Media_type.first_complete_media accept with
        | None -> None
        | Some ((l, r), _) -> Some (l^"/"^r) in
      request_failed meth uri (Unsupported_media_type name)
  | `Not_acceptable acceptable ->
      let proposed =
        Option.unopt_map (Some accept) ~default:"" ~f:Media_type.accept_header in
      request_failed meth uri (Not_acceptable { proposed ; acceptable })
  | `Bad_request msg ->
      request_failed meth uri (Bad_request msg)
  | `Unexpected_content ((content, media_type), error)
  | `Unexpected_error_content ((content, media_type), error) ->
      let media_type = Media_type.name media_type in
      request_failed meth uri (Unexpected_content { content ; media_type ; error })
  | `Unexpected_error_content_type (body, media)
  | `Unexpected_content_type (body, media) ->
      Cohttp_lwt.Body.to_string body >>= fun body ->
      let received =
        Option.unopt_map media ~default:"" ~f:(fun (l, r) -> l^"/"^r) in
      request_failed meth uri
        (Unexpected_content_type { received ;
                                   acceptable = List.map Media_type.name accept ;
                                   body})
  | `Connection_failed msg ->
      request_failed meth uri (Connection_failed msg)
  | `OCaml_exception msg ->
      request_failed meth uri (OCaml_exception msg)
  | `Unauthorized_host host ->
      request_failed meth uri (Unauthorized_host host)

let call_streamed_service
    (type p q i o )
    accept ?logger ?headers ~base (service : (_,_,p,q,i,o) RPC_service.t)
    ~on_chunk ~on_close
    (params : p) (query : q) (body : i) : (unit -> unit) tzresult Lwt.t =
  Client.call_streamed_service
    accept ?logger ?headers ~base ~on_chunk ~on_close
    service params query body >>= fun ans ->
  handle accept ans

let call_service
    (type p q i o )
    accept ?logger ?headers ~base (service : (_,_,p,q,i,o) RPC_service.t)
    (params : p)
    (query : q) (body : i) : o tzresult Lwt.t =
  Client.call_service
    ?logger ?headers ~base accept service params query body >>= fun ans ->
  handle accept ans

type config = {
  host : string ;
  port : int ;
  tls : bool ;
  logger : logger ;
}

let config_encoding =
  let open Data_encoding in
  conv
    (fun { host ; port ; tls ; logger = _ } -> (host, port, tls))
    (fun (host, port, tls) -> { host ; port ; tls ; logger = null_logger })
    (obj3
       (req "host" string)
       (req "port" uint16)
       (req "tls" bool))

let default_config = {
  host = "localhost" ;
  port = 8732 ;
  tls = false ;
  logger = null_logger ;
}

class http_ctxt config media_types : RPC_context.json =
  let base =
    Uri.make
      ~scheme:(if config.tls then "https" else "http")
      ~host:config.host
      ~port:config.port
      () in
  let logger = config.logger in
  object
    method generic_json_call meth ?body uri =
      let path = Uri.path uri and query = Uri.query uri in
      let uri = Uri.with_path base path in
      let uri = Uri.with_query uri query in
      generic_json_call ~logger meth ?body uri
    method call_service
      : 'm 'p 'q 'i 'o.
        ([< Resto.meth ] as 'm, unit, 'p, 'q, 'i, 'o) RPC_service.t ->
        'p -> 'q -> 'i -> 'o tzresult Lwt.t =
      fun service params query body ->
        call_service media_types
          ~logger ~base service params query body
    method call_streamed_service
      : 'm 'p 'q 'i 'o.
        ([< Resto.meth ] as 'm, unit, 'p, 'q, 'i, 'o) RPC_service.t ->
        on_chunk: ('o -> unit) ->
      on_close: (unit -> unit) ->
      'p -> 'q -> 'i -> (unit -> unit) tzresult Lwt.t =
      fun service ~on_chunk ~on_close params query body ->
        call_streamed_service media_types service
          ~logger ~base ~on_chunk ~on_close params query body
    method base = base
  end
