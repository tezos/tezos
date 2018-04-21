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

class type wallet = object
  method load : string -> default:'a -> 'a Data_encoding.encoding -> 'a tzresult Lwt.t
  method write : string -> 'a -> 'a Data_encoding.encoding -> unit tzresult Lwt.t
end

class type block = object
  method block : Block_services.block
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

class type full = object
  inherit printer
  inherit prompter
  inherit wallet
  inherit RPC_context.json
  inherit block
end

class simple_printer : (string -> string -> unit Lwt.t) -> printer
class proxy_context : full -> full
