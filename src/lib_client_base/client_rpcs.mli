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
  logger : RPC_client.logger ;
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

val default_config: config
class http_ctxt: config -> ctxt

val call_service:
  #service_ctxt ->
  ('m, unit,
   'p, 'q, 'i,
   'o, 'e) RPC_service.t ->
  'p -> 'q -> 'i -> 'o tzresult Lwt.t

val call_service0:
  #service_ctxt ->
  ('m, unit,
   unit, unit, 'i,
   'o, 'e) RPC_service.t ->
  'i -> 'o tzresult Lwt.t

val call_service1:
  #service_ctxt ->
  ('m, unit,
   unit * 'a, unit, 'i,
   'o, 'e) RPC_service.t ->
  'a -> 'i -> 'o tzresult Lwt.t

val call_service2:
  #service_ctxt ->
  ('m, unit,
   (unit * 'a) * 'b, unit, 'i,
   'o, 'e) RPC_service.t ->
  'a -> 'b -> 'i -> 'o tzresult Lwt.t

val call_streamed_service0:
  #service_ctxt ->
  ('m, unit,
   unit, unit, 'a,
   'b, unit) RPC_service.t ->
  'a -> 'b Lwt_stream.t tzresult Lwt.t

val call_streamed_service1:
  #service_ctxt ->
  ('m, unit,
   unit * 'a, unit, 'b,
   'c, unit) RPC_service.t ->
  'a -> 'b -> 'c Lwt_stream.t tzresult Lwt.t

val call_err_service0:
  #service_ctxt ->
  ('m, unit,
   unit, unit, 'i,
   'o tzresult, 'e) RPC_service.t ->
  'i -> 'o tzresult Lwt.t

val call_err_service1:
  #service_ctxt ->
  ('m, unit,
   unit * 'a, unit, 'i,
   'o tzresult, 'e) RPC_service.t ->
  'a -> 'i -> 'o tzresult Lwt.t

val call_err_service2:
  #service_ctxt ->
  ('m, unit,
   (unit * 'a) * 'b, unit, 'i,
   'o tzresult, 'e) RPC_service.t ->
  'a -> 'b -> 'i -> 'o tzresult Lwt.t

type block = Node_rpc_services.Blocks.block

val last_baked_block:
  block -> [>
    | `Genesis
    | `Head of int
    | `Test_head of int
    | `Hash of Block_hash.t
  ]
