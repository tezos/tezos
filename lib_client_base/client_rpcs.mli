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
    Data_encoding.json Error_monad.tzresult Lwt.t
  method get_streamed_json :
    RPC.meth ->
    string list ->
    Data_encoding.json ->
    (Data_encoding.json, Error_monad.error list) result Lwt_stream.t
      Error_monad.tzresult Lwt.t
  method make_request :
    (Uri.t -> Data_encoding.json -> 'a Lwt.t) ->
    RPC.meth ->
    string list ->
    Data_encoding.json ->
    ('a * Cohttp.Code.status_code * Cohttp_lwt.Body.t)
      Error_monad.tzresult Lwt.t
  method parse_answer :
    (unit, 'b, 'c, 'd) RPC.service ->
    string list ->
    Data_encoding.json -> 'd Error_monad.tzresult Lwt.t
  method parse_err_answer :
    (unit, 'e, 'f, 'g Error_monad.tzresult) RPC.service ->
    string list ->
    Data_encoding.json -> 'g Error_monad.tzresult Lwt.t
end

class rpc : config -> rpc_sig

val default_config: config
val null_logger: logger
val timings_logger: Format.formatter -> logger
val full_logger: Format.formatter -> logger

val call_service0:
  #rpc_sig ->
  (unit, unit, 'i, 'o) RPC.service ->
  'i -> 'o tzresult Lwt.t

val call_service1:
  #rpc_sig ->
  (unit, unit * 'a, 'i, 'o) RPC.service ->
  'a -> 'i -> 'o tzresult Lwt.t

val call_service2:
  #rpc_sig ->
  (unit, (unit * 'a) * 'b, 'i, 'o) RPC.service ->
  'a -> 'b -> 'i -> 'o tzresult Lwt.t

val call_streamed_service0:
  #rpc_sig ->
  (unit, unit, 'a, 'b) RPC.service ->
  'a -> ('b, error list) result Lwt_stream.t tzresult Lwt.t

val call_streamed_service1:
  #rpc_sig ->
  (unit, unit * 'a, 'b, 'c) RPC.service ->
  'a -> 'b -> ('c, error list) result Lwt_stream.t tzresult Lwt.t

val call_err_service0:
  #rpc_sig ->
  (unit, unit, 'i, 'o tzresult) RPC.service ->
  'i -> 'o tzresult Lwt.t

val call_err_service1:
  #rpc_sig ->
  (unit, unit * 'a, 'i, 'o tzresult) RPC.service ->
  'a -> 'i -> 'o tzresult Lwt.t

val call_err_service2:
  #rpc_sig ->
  (unit, (unit * 'a) * 'b, 'i, 'o tzresult) RPC.service ->
  'a -> 'b -> 'i -> 'o tzresult Lwt.t

type block = Node_rpc_services.Blocks.block

val last_baked_block:
  block -> [>
    | `Genesis
    | `Head of int
    | `Test_head of int
    | `Hash of Block_hash.t
  ]
