(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

let get ctxt =
  Storage.get_fitness ctxt >>= fun fitness ->
  Fitness_repr.to_int64 fitness

let set ctxt v =
  Storage.set_fitness ctxt (Fitness_repr.from_int64 v) >>= fun ctxt ->
  Lwt.return ctxt

let increase ctxt =
  get ctxt >>=? fun v ->
  set ctxt (Int64.succ v) >>= fun ctxt ->
  return ctxt

let init ctxt = set ctxt 0L
