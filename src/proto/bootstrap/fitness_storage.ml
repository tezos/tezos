(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

let increase ctxt =
  Storage.Current_fitness.get ctxt >>=? fun fitness ->
  Storage.Current_fitness.set ctxt (Int64.succ fitness)

let raw_get = Storage.Current_fitness.get
let raw_read = Fitness_repr.to_int64

let get ctxt =
  Storage.Current_fitness.get ctxt >>=? fun fitness ->
  Fitness_repr.from_int64 fitness

let init ctxt =
  Storage.Current_fitness.init ctxt 0L
