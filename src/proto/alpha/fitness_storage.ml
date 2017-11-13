(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

let current = Storage.current_fitness
let increase ctxt =
  let fitness = current ctxt in
  Storage.set_current_fitness ctxt (Int64.succ fitness)
