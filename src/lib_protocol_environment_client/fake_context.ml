(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type t

type key = string list
type value = MBytes.t
let mem _ _ = assert false
let dir_mem _ _ = assert false
let get _ _ = assert false
let set _ _ _ = assert false
let del _ _ = assert false
let remove_rec _ _ = assert false
let fold _ _ ~init:_ ~f:_ = assert false
let keys _ _ = assert false
let fold_keys _ _ ~init:_ ~f:_ = assert false
let register_resolver _ _ = ()
let complete _ _ = assert false
