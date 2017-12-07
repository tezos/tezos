(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(** Typed RPC services: definition, binding and dispatch. *)

module Data : Resto.ENCODING with type 'a t = 'a Data_encoding.t
                              and type schema = Data_encoding.json_schema

include (module type of struct include Resto end)
module Service : (module type of struct include Resto.MakeService(Data) end)

val meth_encoding: meth Data_encoding.t
