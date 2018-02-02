(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Tezos_shell

let genesis : State.Net.genesis = {
  time =
    Time.of_notation_exn "2017-09-22T00:00:00Z" ;
  block =
    Block_hash.of_b58check_exn
      "BLockGenesisGenesisGenesisGenesisGenesisCCCCCeZiLHU" ;
  protocol =
    Protocol_hash.of_b58check_exn
      "ProtoGenesisGenesisGenesisGenesisGenesisGenesk612im" ;
}

let alpha_hash : Tezos_base.TzPervasives.Protocol_hash.t =
  Protocol_hash.of_b58check_exn
    "ProtoALphaALphaALphaALphaALphaALphaALphaALphaDdp3zK"

let test_folder =
  let home = Sys.getenv "HOME" in
  Filename.concat home ".tezos-test"

let store_root = Filename.concat test_folder "store"
let context_root = Filename.concat test_folder "context"
