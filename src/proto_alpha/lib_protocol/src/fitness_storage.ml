(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

let current = Raw_context.current_fitness
let increase ?(gap = 1) ctxt =
  let fitness = current ctxt in
  Raw_context.set_current_fitness ctxt (Int64.add (Int64.of_int gap) fitness)
