(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type config = {
  host : string ;
  port : int ;
  tls : bool ;
  logger : logger ;
}

and logger =
    Logger : {
      log_request : Uri.t -> Data_encoding.json -> 'a Lwt.t ;
      log_success :
        'a -> Cohttp.Code.status_code -> Data_encoding.json  -> unit Lwt.t ;
      log_error :
        'a -> Cohttp.Code.status_code -> string  -> unit Lwt.t ;
    } -> logger

class type ctxt = object
  method get_json :
    RPC_service.meth ->
    string list -> Data_encoding.json ->
    Data_encoding.json tzresult Lwt.t
  method get_streamed_json :
    RPC_service.meth ->
    string list ->
    Data_encoding.json ->
    Data_encoding.json tzresult Lwt_stream.t tzresult Lwt.t
  method make_request :
    (Uri.t -> Data_encoding.json -> 'a Lwt.t) ->
    RPC_service.meth ->
    string list ->
    Data_encoding.json ->
    ('a * Cohttp.Code.status_code * Cohttp_lwt.Body.t) tzresult Lwt.t
  method parse_answer :
    'meth 'params 'input 'output.
    ([< Resto.meth ] as 'meth, unit, 'params, unit, 'input, 'output, unit) RPC_service.t ->
    string list ->
    Data_encoding.json -> 'output tzresult Lwt.t
  method parse_err_answer :
    'meth 'params 'input 'output.
    ([< Resto.meth ] as 'meth, unit, 'params, unit, 'input, 'output tzresult, unit) RPC_service.t ->
    string list ->
    Data_encoding.json -> 'output tzresult Lwt.t
end

class rpc : config -> ctxt

val default_config: config
val null_logger: logger
val timings_logger: Format.formatter -> logger
val full_logger: Format.formatter -> logger

val call_service0:
  #ctxt ->
  ([ `POST ], unit,
   unit, unit, 'i,
   'o, unit) RPC_service.t ->
  'i -> 'o tzresult Lwt.t

val call_service1:
  #ctxt ->
  ([ `POST ], unit,
   unit * 'a, unit, 'i,
   'o, unit) RPC_service.t ->
  'a -> 'i -> 'o tzresult Lwt.t

val call_service2:
  #ctxt ->
  ([ `POST ], unit,
   (unit * 'a) * 'b, unit, 'i,
   'o, unit) RPC_service.t ->
  'a -> 'b -> 'i -> 'o tzresult Lwt.t

val call_streamed_service0:
  #ctxt ->
  ([ `POST ], unit,
   unit, unit, 'a,
   'b, unit) RPC_service.t ->
  'a -> 'b tzresult Lwt_stream.t tzresult Lwt.t

val call_streamed_service1:
  #ctxt ->
  ([ `POST ], unit,
   unit * 'a, unit, 'b,
   'c, unit) RPC_service.t ->
  'a -> 'b -> 'c tzresult Lwt_stream.t tzresult Lwt.t

val call_err_service0:
  #ctxt ->
  ([ `POST ], unit,
   unit, unit, 'i,
   'o tzresult, unit) RPC_service.t ->
  'i -> 'o tzresult Lwt.t

val call_err_service1:
  #ctxt ->
  ([ `POST ], unit,
   unit * 'a, unit, 'i,
   'o tzresult, unit) RPC_service.t ->
  'a -> 'i -> 'o tzresult Lwt.t

val call_err_service2:
  #ctxt ->
  ([ `POST ], unit,
   (unit * 'a) * 'b, unit, 'i,
   'o tzresult, unit) RPC_service.t ->
  'a -> 'b -> 'i -> 'o tzresult Lwt.t

type block = Node_rpc_services.Blocks.block

val last_baked_block:
  block -> [>
    | `Genesis
    | `Head of int
    | `Test_head of int
    | `Hash of Block_hash.t
  ]
