(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
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
    command ~group ~desc: "Forge and inject an endorsement operation."
      (args1 max_priority_arg)
      (prefixes [ "endorse"; "for" ]
       @@ Client_keys.Public_key_hash.alias_param
         ~name:"baker" ~desc: "name of the delegate owning the endorsement right"
       @@ stop)
      (fun max_priority (_, delegate) cctxt ->
         endorse_block cctxt ?max_priority delegate) ;
    command ~group ~desc: "Forge and inject block using the delegate rights."
      (args3 max_priority_arg force_switch free_baking_switch)
      (prefixes [ "bake"; "for" ]
       @@ Client_keys.Public_key_hash.alias_param
         ~name:"baker" ~desc: "name of the delegate owning the baking right"
       @@ stop)
      (fun (max_priority, force, free_baking) (_, delegate) cctxt ->
         bake_block cctxt cctxt#block
           ~force ?max_priority ~free_baking delegate) ;
    command ~group ~desc: "Forge and inject a seed-nonce revelation operation."
      no_options
      (prefixes [ "reveal"; "nonce"; "for" ]
       @@ seq_of_param Block_hash.param)
      (fun () block_hashes cctxt ->
         reveal_block_nonces cctxt block_hashes) ;
    command ~group ~desc: "Forge and inject all the possible seed-nonce revelation operations."
      no_options
      (prefixes [ "reveal"; "nonces" ]
       @@ stop)
      (fun () cctxt ->
         reveal_nonces cctxt ()) ;
  ]
