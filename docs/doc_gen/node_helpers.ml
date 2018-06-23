(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

let genesis : State.Chain.genesis = {
  time =
    Time.of_notation_exn "2018-04-17T11:46:23Z" ;
  block =
    Block_hash.of_b58check_exn
      "BLockGenesisGenesisGenesisGenesisGenesisa52f8bUWPcg" ;
  protocol =
    Protocol_hash.of_b58check_exn
      "ProtoGenesisGenesisGenesisGenesisGenesisGenesk612im" ;
}

let with_node f =
  let run dir =
    let (/) = Filename.concat in
    let node_config : Node.config = {
      genesis ;
      patch_context = None ;
      store_root = dir / "store" ;
      context_root = dir / "context" ;
      p2p = None ;
      test_chain_max_tll = None ;
      checkpoint = None ;
    } in
    Node.create
      node_config
      Node.default_peer_validator_limits
      Node.default_block_validator_limits
      Node.default_prevalidator_limits
      Node.default_chain_validator_limits >>=? fun node ->
    f node >>=? fun () ->
    return () in
  Lwt_utils_unix.with_tempdir "tezos_rpcdoc_" run >>= function
  | Ok () ->
      Lwt.return_unit
  | Error err ->
      Format.eprintf "%a@." pp_print_error err ;
      Pervasives.exit 1
