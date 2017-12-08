(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

module Client = Resto_cohttp.Client.Make(RPC_encoding)

type config = {
  host : string ;
  port : int ;
  tls : bool ;
  logger : RPC_client.logger ;
}

let config_encoding =
  let open Data_encoding in
  conv
    (fun { host ; port ; tls } -> (host, port, tls))
    (fun (host, port, tls) -> { host ; port ; tls ; logger = RPC_client.null_logger})
    (obj3
       (req "host" string)
       (req "port" uint16)
       (req "tls" bool))

let default_config = {
  host = "localhost" ;
  port = 8732 ;
  tls = false ;
  logger = RPC_client.null_logger ;
}

class type json_ctxt = object
  method generic_json_call :
    RPC_service.meth ->
    ?body:Data_encoding.json ->
    Uri.t ->
    (Data_encoding.json, Data_encoding.json option) RPC_client.rest_result Lwt.t
end

class type service_ctxt = object
  method call_service :
    'm 'p 'q 'i 'o 'e.
    ([< Resto.meth ] as 'm, unit, 'p, 'q, 'i, 'o, 'e) RPC_service.t ->
    'p -> 'q -> 'i -> 'o tzresult Lwt.t
  method call_streamed_service :
    'm 'p 'q 'i 'o 'e.
    ([< Resto.meth ] as 'm, unit, 'p, 'q, 'i, 'o, 'e) RPC_service.t ->
    on_chunk: ('o -> unit) ->
    on_close: (unit -> unit) ->
    'p -> 'q -> 'i -> (unit -> unit) tzresult Lwt.t
end

class type ctxt = object
  inherit json_ctxt
  inherit service_ctxt
end

class http_ctxt config : ctxt =
  let base =
    Uri.make
      ~scheme:(if config.tls then "https" else "http")
      ~host:config.host
      ~port:config.port
      () in
  let logger = config.logger in
  object
    method generic_json_call meth ?body uri =
      let uri = Uri.with_path base (Uri.path uri) in
      let uri = Uri.with_query uri (Uri.query uri) in
      RPC_client.generic_json_call ~logger meth ?body uri
    method call_service
      : 'm 'p 'q 'i 'o 'e.
        ([< Resto.meth ] as 'm, unit, 'p, 'q, 'i, 'o, 'e) RPC_service.t ->
        'p -> 'q -> 'i -> 'o tzresult Lwt.t =
      fun service params query body ->
        RPC_client.call_service Media_type.all_media_types ~logger ~base service params query body
    method call_streamed_service
      : 'm 'p 'q 'i 'o 'e.
        ([< Resto.meth ] as 'm, unit, 'p, 'q, 'i, 'o, 'e) RPC_service.t ->
        on_chunk: ('o -> unit) ->
      on_close: (unit -> unit) ->
      'p -> 'q -> 'i -> (unit -> unit) tzresult Lwt.t =
      fun service ~on_chunk ~on_close params query body ->
        RPC_client.call_streamed_service Media_type.all_media_types service
          ~logger ~base ~on_chunk ~on_close params query body
  end

let call_service (ctxt : #service_ctxt) service params query body =
  ctxt#call_service service params query body

let call_err_service ctxt service params query body =
  call_service ctxt service params query body >>=? Lwt.return

let call_streamed_service (ctxt : #service_ctxt) service param query body =
  let stream, push = Lwt_stream.create () in
  ctxt#call_streamed_service
    ~on_chunk:(fun o -> push (Some o)) ~on_close:(fun () -> push None)
    service param query body >>= function
  | Error _ as err -> Lwt.return err
  | Ok _finalizer ->
      return stream

(* Currified params *)

let call_service0 ctxt service body =
  call_service ctxt service () () body

let call_service1 ctxt service a1 body =
  call_service ctxt service ((), a1) () body

let call_service2 ctxt service a1 a2 body =
  call_service ctxt service (((), a1), a2) () body

let call_streamed_service0 ctxt service body =
  call_streamed_service ctxt service () () body

let call_streamed_service1 ctxt service a1 body =
  call_streamed_service ctxt service ((), a1) () body

let call_err_service0 ctxt service body =
  call_err_service ctxt service () () body

let call_err_service1 ctxt service a1 body =
  call_err_service ctxt service ((), a1) () body

let call_err_service2 ctxt service a1 a2 body =
  call_err_service ctxt service (((), a1), a2) () body

type block = Node_rpc_services.Blocks.block

let last_baked_block = function
  | `Prevalidation -> `Head 0
  | `Test_prevalidation -> `Test_head 0
  | `Genesis | `Head _ | `Test_head _ | `Hash _ as block -> block
