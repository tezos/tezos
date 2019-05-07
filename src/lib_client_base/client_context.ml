(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2018 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

type ('a, 'b) lwt_format =
  ('a, Format.formatter, unit, 'b Lwt.t) format4

class type printer = object
  method error : ('a, 'b) lwt_format -> 'a
  method warning : ('a, unit) lwt_format -> 'a
  method message : ('a, unit) lwt_format -> 'a
  method answer :  ('a, unit) lwt_format -> 'a
  method log : string -> ('a, unit) lwt_format -> 'a
end

class type prompter = object
  method prompt : ('a, string tzresult) lwt_format -> 'a
  method prompt_password : ('a, MBytes.t tzresult) lwt_format -> 'a
end

class type io = object
  inherit printer
  inherit prompter
end

class simple_printer log =
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
  method load_passwords : string Lwt_stream.t option
  method read_file : string -> string tzresult Lwt.t
  method with_lock : (unit -> 'a Lwt.t) -> 'a Lwt.t
  method load : string -> default:'a -> 'a Data_encoding.encoding -> 'a tzresult Lwt.t
  method write : string -> 'a -> 'a Data_encoding.encoding -> unit tzresult Lwt.t
end

class type chain = object
  method chain : Shell_services.chain
end

class type block = object
  method block : Shell_services.block
  method confirmations : int option
end

class type io_wallet = object
  inherit printer
  inherit prompter
  inherit wallet
end

class type io_rpcs = object
  inherit printer
  inherit prompter
  inherit RPC_context.json
end

class type ui = object
  method sleep : float -> unit Lwt.t
end

class type full = object
  inherit printer
  inherit prompter
  inherit wallet
  inherit RPC_context.json
  inherit chain
  inherit block
  inherit ui
end

class proxy_context (obj : full) = object
  method load_passwords = obj#load_passwords
  method read_file = obj#read_file
  method base = obj#base
  method chain = obj#chain
  method block = obj#block
  method confirmations = obj#confirmations
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
  method with_lock : type a. (unit -> a Lwt.t) -> a Lwt.t = obj#with_lock
  method load : type a. string -> default:a -> a Data_encoding.encoding -> a tzresult Lwt.t = obj#load
  method log : type a. string -> (a, unit) lwt_format -> a = obj#log
  method message : type a. (a, unit) lwt_format -> a = obj#message
  method warning : type a. (a, unit) lwt_format -> a  = obj#warning
  method write : type a. string -> a -> a Data_encoding.encoding -> unit tzresult Lwt.t = obj#write
  method prompt : type a. (a, string tzresult) lwt_format -> a = obj#prompt
  method prompt_password : type a. (a, MBytes.t tzresult) lwt_format -> a = obj#prompt_password
  method sleep : float -> unit Lwt.t = obj#sleep
end
