(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

let random_state_hash = "\076\064\204" (* rng(53): never used... *)

include Blake2B.Make(Base58)(struct
    let name = "random"
    let title = "A random generation state"
    let b58check_prefix = random_state_hash
    let size = None
  end)

let () =
  Base58.check_encoded_prefix b58check_encoding "rng" 53

