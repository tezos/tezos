(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
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

val default_config: config
val null_logger: logger
val timings_logger: Format.formatter -> logger
val full_logger: Format.formatter -> logger

val get_json:
  config ->
  RPC.meth -> string list -> Data_encoding.json ->
  Data_encoding.json tzresult Lwt.t

val call_service0:
  config ->
  (unit, unit, 'i, 'o) RPC.service ->
  'i -> 'o tzresult Lwt.t

val call_service1:
  config ->
  (unit, unit * 'a, 'i, 'o) RPC.service ->
  'a -> 'i -> 'o tzresult Lwt.t

val call_service2:
  config ->
  (unit, (unit * 'a) * 'b, 'i, 'o) RPC.service ->
  'a -> 'b -> 'i -> 'o tzresult Lwt.t

val call_streamed_service0:
  config ->
  (unit, unit, 'a, 'b) RPC.service ->
  'a -> ('b, error list) result Lwt_stream.t tzresult Lwt.t

val call_streamed_service1:
  config ->
  (unit, unit * 'a, 'b, 'c) RPC.service ->
  'a -> 'b -> ('c, error list) result Lwt_stream.t tzresult Lwt.t

val call_err_service0:
  config ->
  (unit, unit, 'i, 'o tzresult) RPC.service ->
  'i -> 'o tzresult Lwt.t

val call_err_service1:
  config ->
  (unit, unit * 'a, 'i, 'o tzresult) RPC.service ->
  'a -> 'i -> 'o tzresult Lwt.t

val call_err_service2:
  config ->
  (unit, (unit * 'a) * 'b, 'i, 'o tzresult) RPC.service ->
  'a -> 'b -> 'i -> 'o tzresult Lwt.t

val call_describe0:
  config ->
  (unit, unit, 'a, 'b) RPC.service ->
  string list -> 'a -> 'b tzresult Lwt.t

type block = Node_rpc_services.Blocks.block

val last_mined_block:
  block -> [>
    | `Genesis
    | `Head of int
    | `Test_head of int
    | `Hash of Block_hash.t
  ]
