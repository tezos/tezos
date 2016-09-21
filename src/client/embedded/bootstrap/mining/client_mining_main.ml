(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Cli_entries
open Client_proto_contracts

let mine_block block ?force ?max_priority ?src_sk delegate =
  begin
    match src_sk with
    | None ->
        Client_keys.get_key delegate >>=? fun (_, _, src_sk) ->
        return src_sk
    | Some sk -> return sk
  end >>=? fun src_sk ->
  Client_proto_rpcs.Context.level block >>=? fun level ->
  let level = Raw_level.succ level.level in
  let seed_nonce = Client_mining_forge.generate_seed_nonce () in
  Client_mining_forge.forge_block
    ~timestamp:(Time.now ())
    ?force ?max_priority
    ~seed_nonce ~src_sk block delegate >>=? fun block_hash ->
  Client_mining_forge.State.record_block level block_hash seed_nonce
  |> trace_exn (Failure "Error while recording block") >>=? fun () ->
  message "Injected block %a" Block_hash.pp_short block_hash ;
  return ()

let endorse_block ?force ?max_priority delegate =
  let block = Client_proto_args.block () in
  Client_keys.get_key delegate >>=? fun (_src_name, src_pk, src_sk) ->
  Client_mining_endorsement.forge_endorsement
    block ?force ?max_priority ~src_sk src_pk >>=? fun oph ->
  answer "Operation successfully injected in the node." ;
  answer "Operation hash is '%a'." Operation_hash.pp oph ;
  return ()

let get_predecessor_cycle cycle =
  match Cycle.pred cycle with
  | None ->
      if Cycle.(cycle = root) then
        error "No predecessor for the first cycle"
      else
        error
          "Cannot compute the predecessor of cycle %a"
          Cycle.pp cycle
  | Some cycle -> Lwt.return cycle

let do_reveal ?force block blocks =
  let nonces = List.map snd blocks in
  Client_mining_revelation.forge_seed_nonce_revelation
    block ?force nonces >>=? fun () ->
  Client_proto_nonces.dels (List.map fst blocks) >>=? fun () ->
  return ()

let reveal_block_nonces ?force block_hashes =
  let block = Client_proto_args.block () in
  Lwt_list.filter_map_p
    (fun hash ->
       Lwt.catch
         (fun () ->
            Client_mining_blocks.info (`Hash hash) >>= function
            | Ok bi -> Lwt.return (Some bi)
            | Error _ ->
                Lwt.fail Not_found)
         (fun _ ->
            Format.eprintf "Cannot find block %a in the chain. (ignoring)@."
              Block_hash.pp_short hash ;
            Lwt.return_none))
    block_hashes >>= fun block_infos ->
  map_filter_s (fun (bi : Client_mining_blocks.block_info) ->
      Client_proto_nonces.find bi.hash >>= function
      | None ->
          Format.eprintf "Cannot find nonces for block %a (ignoring)@."
            Block_hash.pp_short bi.hash ;
          return None
      | Some nonce ->
          return (Some (bi.hash, (bi.level.level, nonce))))
    block_infos >>=? fun blocks ->
  do_reveal ?force block blocks

let reveal_nonces ?force () =
  let block = Client_proto_args.block () in
  Client_proto_rpcs.Context.next_level block >>=? fun level ->
  let cur_cycle = level.cycle in
  get_predecessor_cycle cur_cycle >>= fun cycle ->
  Client_mining_blocks.blocks_from_cycle block cycle >>=? fun block_infos ->
  map_filter_s (fun (bi : Client_mining_blocks.block_info) ->
      Client_proto_nonces.find bi.hash >>= function
      | None -> return None
      | Some nonce ->
          Format.eprintf "Found nonce for %a (level: %a)@."
            Block_hash.pp_short bi.hash Level.pp bi.level ;
          return (Some (bi.hash, (bi.level.level, nonce))))
    block_infos >>=? fun blocks ->
  do_reveal ?force block blocks

open Client_proto_args

let run_daemon delegates =
  Client_mining_daemon.run
    ?max_priority:!max_priority
    ~delay:!endorsement_delay
    ~min_date:((Time.add (Time.now ()) (Int64.neg 1800L)))
    (List.map snd delegates)

let commands () =
  let open Cli_entries in
  register_group "delegate" "Commands related to delegate operations." ;
  [
    command
      ~group: "delegate"
      ~desc: "Launch a daemon that handles delegate operations."
      ~args: [endorsement_delay_arg; max_priority_arg;
              Daemon.mining_arg ; Daemon.endorsement_arg ; Daemon.denunciation_arg]
      (prefixes [ "launch" ; "daemon" ]
       @@ seq_of_param Client_keys.Public_key_hash.alias_param )
      run_daemon ;
    command
      ~group: "delegate"
      ~desc: "Forge and inject an endorsement operation"
      ~args: [ force_arg ]
      (prefixes [ "endorse"; "for" ]
       @@ Client_keys.Public_key_hash.alias_param
         ~n:"miner" ~desc: "name of the delegate owning the endorsement right"
       @@ stop)
      (fun (_, delegate) () ->
         endorse_block
           ~force:!force ?max_priority:!max_priority delegate >>=
         Client_proto_rpcs.handle_error) ;
    command
      ~group: "delegate"
      ~desc: "Forge and inject block using the delegate rights"
      ~args: [ max_priority_arg ; force_arg ]
      (prefixes [ "mine"; "for" ]
       @@ Client_keys.Public_key_hash.alias_param
         ~n:"miner" ~desc: "name of the delegate owning the mining right"
       @@ stop)
      (fun (_, delegate) () ->
         mine_block (block ())
           ~force:!force ?max_priority:!max_priority delegate >>=
         Client_proto_rpcs.handle_error) ;
    command
      ~group: "delegate"
      ~desc: "Forge and inject a seed-nonce revelation operation"
      ~args: [ force_arg ]
      (prefixes [ "reveal"; "nonce"; "for" ]
       @@ Cli_entries.seq_of_param Block_hash.param)
      (fun block_hashes ->
         reveal_block_nonces ~force:!force block_hashes >>= Client_proto_rpcs.handle_error) ;
    command
      ~group: "delegate"
      ~desc: "Forge and inject redemption operations"
      ~args: [ force_arg ]
      (prefixes [ "reveal"; "nonces" ]
       @@ stop)
      (fun () ->
         reveal_nonces ~force:!force () >>= Client_proto_rpcs.handle_error) ;
  ]

let () =
  Client_version.register Client_proto_main.protocol @@
  commands ()
