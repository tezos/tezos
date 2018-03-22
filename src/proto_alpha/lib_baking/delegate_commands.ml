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
  let open Clic in
  [
    command ~group ~desc: "Launch a daemon that handles delegate operations."
      (args5 max_priority_arg endorsement_delay_arg
         Daemon.baking_switch Daemon.endorsement_switch Daemon.denunciation_switch)
      (prefixes [ "launch" ; "daemon" ]
       @@ seq_of_param Client_keys.Public_key_hash.alias_param)
      (fun max_priority delegates cctxt ->
         Client_daemon.Baker.run cctxt
           ?max_priority
           ~min_date:((Time.add (Time.now ()) (Int64.neg 1800L)))
           (List.map snd delegates)
      ) ;

    command ~group ~desc: "Launch the accuser daemon"
      no_options
      (prefixes [ "launch" ; "accuser" ]
       @@ stop)
      (fun () cctxt -> Client_daemon.Accuser.run cctxt) ;

    command ~group ~desc: "Launch the endorser daemon"
      (args1 endorsement_delay_arg )
      (prefixes [ "launch" ; "endorser" ]
       @@ seq_of_param Client_keys.Public_key_hash.alias_param)
      (fun endorsement_delay delegates cctxt ->
         Client_daemon.Endorser.run cctxt
           ~delay:endorsement_delay
           ~min_date:((Time.add (Time.now ()) (Int64.neg 1800L)))
           (List.map snd delegates)
      ) ;

    command ~group ~desc: "Forge and inject an endorsement operation."
      no_options
      (prefixes [ "endorse"; "for" ]
       @@ Client_keys.Public_key_hash.source_param
         ~name:"baker" ~desc: "name of the delegate owning the endorsement right"
       @@ stop)
      (fun () delegate cctxt -> endorse_block cctxt delegate) ;
    command ~group ~desc: "Forge and inject block using the delegate rights."
      (args3 max_priority_arg force_switch minimal_timestamp_switch)
      (prefixes [ "bake"; "for" ]
       @@ Client_keys.Public_key_hash.source_param
         ~name:"baker" ~desc: "name of the delegate owning the baking right"
       @@ stop)
      (fun (max_priority, force, minimal_timestamp) delegate cctxt ->
         bake_block cctxt cctxt#block
           ~force ?max_priority ~minimal_timestamp delegate) ;
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
