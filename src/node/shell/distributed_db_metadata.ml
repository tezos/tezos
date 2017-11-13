(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type t = unit
let initial = ()
let encoding = Data_encoding.empty
let score () = 0.

let cfg : _ P2p.meta_config = { encoding ; initial ; score }
