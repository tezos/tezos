(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type ('a, 'b) lwt_format =
  ('a, Format.formatter, unit, 'b Lwt.t) format4

class type logger_sig = object
  method error : ('a, 'b) lwt_format -> 'a
  method warning : ('a, unit) lwt_format -> 'a
  method message : ('a, unit) lwt_format -> 'a
  method answer :  ('a, unit) lwt_format -> 'a
  method log : string -> ('a, unit) lwt_format -> 'a
end

class logger log =
  let message =
    (fun x ->
       Format.kasprintf (fun msg -> log "stdout" msg) x) in
  object
    method error : type a b. (a, b) lwt_format -> a =
      Format.kasprintf
        (fun msg ->
           Lwt.fail (Failure msg))
    method warning : type a. (a, unit) lwt_format -> a =
      Format.kasprintf
        (fun msg -> log "stderr" msg)
    method message : type a. (a, unit) lwt_format -> a = message
    method answer : type a. (a, unit) lwt_format -> a = message
    method log : type a. string -> (a, unit) lwt_format -> a =
      fun name ->
        Format.kasprintf
          (fun msg -> log name msg)
  end

class type wallet = object
  method load : string -> default:'a -> 'a Data_encoding.encoding -> 'a tzresult Lwt.t
  method write : string -> 'a -> 'a Data_encoding.encoding -> unit tzresult Lwt.t
end

class type block = object
  method block : Block_services.block
end

class type logging_wallet = object
  inherit logger
  inherit wallet
end

class type logging_rpcs = object
  inherit logger
  inherit RPC_context.json
end

class type full_context = object
  inherit logger
  inherit wallet
  inherit RPC_context.json
  inherit block
end

class proxy_context (obj : full_context) = object
  method block = obj#block
  method answer : type a. (a, unit) lwt_format -> a = obj#answer
  method call_service :
    'm 'p 'q 'i 'o.
    ([< Resto.meth ] as 'm, 'pr, 'p, 'q, 'i, 'o) RPC_service.t ->
    'p -> 'q -> 'i -> 'o tzresult Lwt.t = obj#call_service
  method call_streamed_service :
    'm 'p 'q 'i 'o.
    ([< Resto.meth ] as 'm, 'pr, 'p, 'q, 'i, 'o) RPC_service.t ->
    on_chunk: ('o -> unit) ->
    on_close: (unit -> unit) ->
    'p -> 'q -> 'i -> (unit -> unit) tzresult Lwt.t = obj#call_streamed_service
  method error : type a b. (a, b) lwt_format -> a = obj#error
  method generic_json_call = obj#generic_json_call
  method load : type a. string -> default:a -> a Data_encoding.encoding -> a tzresult Lwt.t = obj#load
  method log : type a. string -> (a, unit) lwt_format -> a = obj#log
  method message : type a. (a, unit) lwt_format -> a = obj#message
  method warning : type a. (a, unit) lwt_format -> a  = obj#warning
  method write : type a. string -> a -> a Data_encoding.encoding -> unit tzresult Lwt.t = obj#write
end
