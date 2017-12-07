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

class type rpc_sig = object
  method get_json :
    RPC.meth ->
    string list -> Data_encoding.json ->
    Data_encoding.json tzresult Lwt.t
  method get_streamed_json :
    RPC.meth ->
    string list ->
    Data_encoding.json ->
    Data_encoding.json tzresult Lwt_stream.t tzresult Lwt.t
  method make_request :
    (Uri.t -> Data_encoding.json -> 'a Lwt.t) ->
    RPC.meth ->
    string list ->
    Data_encoding.json ->
    ('a * Cohttp.Code.status_code * Cohttp_lwt.Body.t) tzresult Lwt.t
  method parse_answer :
    'meth 'params 'input 'output.
    ([< Resto.meth ] as 'meth, unit, 'params, unit, 'input, 'output, unit) RPC.Service.t ->
    string list ->
    Data_encoding.json -> 'output tzresult Lwt.t
  method parse_err_answer :
    'meth 'params 'input 'output.
    ([< Resto.meth ] as 'meth, unit, 'params, unit, 'input, 'output tzresult, unit) RPC.Service.t ->
    string list ->
    Data_encoding.json -> 'output tzresult Lwt.t
end

class rpc : config -> rpc_sig

val default_config: config
val null_logger: logger
val timings_logger: Format.formatter -> logger
val full_logger: Format.formatter -> logger

val call_service0:
  #rpc_sig ->
  ([ `POST ], unit,
   unit, unit, 'i,
   'o, unit) RPC.Service.t ->
  'i -> 'o tzresult Lwt.t

val call_service1:
  #rpc_sig ->
  ([ `POST ], unit,
   unit * 'a, unit, 'i,
   'o, unit) RPC.Service.t ->
  'a -> 'i -> 'o tzresult Lwt.t

val call_service2:
  #rpc_sig ->
  ([ `POST ], unit,
   (unit * 'a) * 'b, unit, 'i,
   'o, unit) RPC.Service.t ->
  'a -> 'b -> 'i -> 'o tzresult Lwt.t

val call_streamed_service0:
  #rpc_sig ->
  ([ `POST ], unit,
   unit, unit, 'a,
   'b, unit) RPC.Service.t ->
  'a -> 'b tzresult Lwt_stream.t tzresult Lwt.t

val call_streamed_service1:
  #rpc_sig ->
  ([ `POST ], unit,
   unit * 'a, unit, 'b,
   'c, unit) RPC.Service.t ->
  'a -> 'b -> 'c tzresult Lwt_stream.t tzresult Lwt.t

val call_err_service0:
  #rpc_sig ->
  ([ `POST ], unit,
   unit, unit, 'i,
   'o tzresult, unit) RPC.Service.t ->
  'i -> 'o tzresult Lwt.t

val call_err_service1:
  #rpc_sig ->
  ([ `POST ], unit,
   unit * 'a, unit, 'i,
   'o tzresult, unit) RPC.Service.t ->
  'a -> 'i -> 'o tzresult Lwt.t

val call_err_service2:
  #rpc_sig ->
  ([ `POST ], unit,
   (unit * 'a) * 'b, unit, 'i,
   'o tzresult, unit) RPC.Service.t ->
  'a -> 'b -> 'i -> 'o tzresult Lwt.t

type block = Node_rpc_services.Blocks.block

val last_baked_block:
  block -> [>
    | `Genesis
    | `Head of int
    | `Test_head of int
    | `Hash of Block_hash.t
  ]
