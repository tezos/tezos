(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Client_proto_args
open Client_baking_lib

let group =
  { Cli_entries.name = "delegate" ;
    title = "Commands related to delegate operations." }

let commands () =
  let open Cli_entries in
  [
    command ~group ~desc: "Launch a daemon that handles delegate operations."
      (args5 max_priority_arg endorsement_delay_arg
         Daemon.baking_switch Daemon.endorsement_switch Daemon.denunciation_switch)
      (prefixes [ "launch" ; "daemon" ]
       @@ seq_of_param Client_keys.Public_key_hash.alias_param)
      (fun (max_priority, endorsement_delay, baking, endorsement, denunciation) delegates cctxt ->
         let (endorsement, baking, denunciation) =
           if (not endorsement) && (not baking) && (not denunciation)
           then (true, true, true)
           else (endorsement, baking, denunciation) in
         run_daemon cctxt ?max_priority ~endorsement_delay ~endorsement ~baking ~denunciation delegates) ;
    command ~group ~desc: "Forge and inject an endorsement operation"
      (args2 force_switch max_priority_arg)
      (prefixes [ "endorse"; "for" ]
       @@ Client_keys.Public_key_hash.alias_param
         ~name:"baker" ~desc: "name of the delegate owning the endorsement right"
       @@ stop)
      (fun (force, max_priority) (_, delegate) cctxt ->
         endorse_block cctxt
           ~force ?max_priority delegate) ;
    command ~group ~desc: "Forge and inject block using the delegate rights"
      (args3 max_priority_arg force_switch free_baking_switch)
      (prefixes [ "bake"; "for" ]
       @@ Client_keys.Public_key_hash.alias_param
         ~name:"baker" ~desc: "name of the delegate owning the baking right"
       @@ stop)
      (fun (max_priority, force, free_baking) (_, delegate) cctxt ->
         bake_block cctxt cctxt#block
           ~force ?max_priority ~free_baking delegate) ;
    command ~group ~desc: "Forge and inject a seed-nonce revelation operation"
      (args1 force_switch)
      (prefixes [ "reveal"; "nonce"; "for" ]
       @@ seq_of_param Block_hash.param)
      (fun force block_hashes cctxt ->
         reveal_block_nonces cctxt
           ~force block_hashes) ;
    command ~group ~desc: "Forge and inject redemption operations"
      (args1 force_switch)
      (prefixes [ "reveal"; "nonces" ]
       @@ stop)
      (fun force cctxt ->
         reveal_nonces cctxt ~force ()) ;
  ]
