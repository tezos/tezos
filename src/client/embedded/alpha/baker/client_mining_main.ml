(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Cli_entries
open Client_commands
open Client_proto_contracts

let mine_block cctxt block
    ?force ?max_priority ?(free_mining=false) ?src_sk delegate =
  begin
    match src_sk with
    | None ->
        Client_keys.get_key cctxt delegate >>=? fun (_, _, src_sk) ->
        return src_sk
    | Some sk -> return sk
  end >>=? fun src_sk ->
  Client_proto_rpcs.Context.level cctxt.rpc_config block >>=? fun level ->
  let level = Raw_level.succ level.level in
  let seed_nonce = Client_mining_forge.generate_seed_nonce () in
  let seed_nonce_hash = Nonce.hash seed_nonce in
  Client_mining_forge.forge_block cctxt.rpc_config
    ~timestamp:(Time.now ())
    ?force
    ~seed_nonce_hash ~src_sk block
    ~priority:(`Auto (delegate, max_priority, free_mining)) () >>=? fun block_hash ->
  Client_mining_forge.State.record_block cctxt level block_hash seed_nonce
  |> trace_exn (Failure "Error while recording block") >>=? fun () ->
  cctxt.message "Injected block %a" Block_hash.pp_short block_hash >>= fun () ->
  return ()

let endorse_block cctxt ?force ?max_priority delegate =
  Client_keys.get_key cctxt delegate >>=? fun (_src_name, src_pk, src_sk) ->
  Client_mining_endorsement.forge_endorsement cctxt
    cctxt.config.block ?force ?max_priority ~src_sk src_pk >>=? fun oph ->
  cctxt.answer "Operation successfully injected in the node." >>= fun () ->
  cctxt.answer "Operation hash is '%a'." Operation_hash.pp oph >>= fun () ->
  return ()

let get_predecessor_cycle cctxt cycle =
  match Cycle.pred cycle with
  | None ->
      if Cycle.(cycle = root) then
        cctxt.Client_commands.error "No predecessor for the first cycle"
      else
        cctxt.error
          "Cannot compute the predecessor of cycle %a"
          Cycle.pp cycle
  | Some cycle -> Lwt.return cycle

let do_reveal cctxt ?force block blocks =
  let nonces = List.map snd blocks in
  Client_mining_revelation.forge_seed_nonce_revelation cctxt
    block ?force nonces >>=? fun () ->
  Client_proto_nonces.dels cctxt (List.map fst blocks) >>=? fun () ->
  return ()

let reveal_block_nonces cctxt ?force block_hashes =
  Lwt_list.filter_map_p
    (fun hash ->
       Lwt.catch
         (fun () ->
            Client_mining_blocks.info cctxt.rpc_config (`Hash hash) >>= function
            | Ok bi -> Lwt.return (Some bi)
            | Error _ ->
                Lwt.fail Not_found)
         (fun _ ->
            cctxt.warning
              "Cannot find block %a in the chain. (ignoring)@."
              Block_hash.pp_short hash >>= fun () ->
            Lwt.return_none))
    block_hashes >>= fun block_infos ->
  filter_map_s (fun (bi : Client_mining_blocks.block_info) ->
      Client_proto_nonces.find cctxt bi.hash >>= function
      | None ->
          cctxt.warning "Cannot find nonces for block %a (ignoring)@."
            Block_hash.pp_short bi.hash >>= fun () ->
          return None
      | Some nonce ->
          return (Some (bi.hash, (bi.level.level, nonce))))
    block_infos >>=? fun blocks ->
  do_reveal cctxt ?force cctxt.config.block blocks

let reveal_nonces cctxt ?force () =
  Client_mining_forge.get_unrevealed_nonces
    cctxt ?force cctxt.config.block >>=? fun nonces ->
  do_reveal cctxt ?force cctxt.config.block nonces

open Client_proto_args

let run_daemon cctxt delegates =
  Client_mining_daemon.run cctxt
    ?max_priority:!max_priority
    ~delay:!endorsement_delay
    ~min_date:((Time.add (Time.now ()) (Int64.neg 1800L)))
    (List.map snd delegates)

let group =
  { Cli_entries.name = "delegate" ;
    title = "Commands related to delegate operations." }

let commands () =
  let open Cli_entries in
  [
    command ~group ~desc: "Launch a daemon that handles delegate operations."
      ~args: [endorsement_delay_arg; max_priority_arg;
              Daemon.mining_arg ; Daemon.endorsement_arg ; Daemon.denunciation_arg]
      (prefixes [ "launch" ; "daemon" ]
       @@ seq_of_param Client_keys.Public_key_hash.alias_param )
      (fun delegates cctxt ->
         run_daemon cctxt delegates) ;
    command ~group ~desc: "Forge and inject an endorsement operation"
      ~args: [ force_arg ]
      (prefixes [ "endorse"; "for" ]
       @@ Client_keys.Public_key_hash.alias_param
         ~name:"miner" ~desc: "name of the delegate owning the endorsement right"
       @@ stop)
      (fun (_, delegate) cctxt ->
         endorse_block cctxt
           ~force:!force ?max_priority:!max_priority delegate) ;
    command ~group ~desc: "Forge and inject block using the delegate rights"
      ~args: [ max_priority_arg ; force_arg ; free_mining_arg ]
      (prefixes [ "mine"; "for" ]
       @@ Client_keys.Public_key_hash.alias_param
         ~name:"miner" ~desc: "name of the delegate owning the mining right"
       @@ stop)
      (fun (_, delegate) cctxt ->
         mine_block cctxt cctxt.config.block
           ~force:!force ?max_priority:!max_priority ~free_mining:!free_mining delegate) ;
    command ~group ~desc: "Forge and inject a seed-nonce revelation operation"
      ~args: [ force_arg ]
      (prefixes [ "reveal"; "nonce"; "for" ]
       @@ Cli_entries.seq_of_param Block_hash.param)
      (fun block_hashes cctxt ->
         reveal_block_nonces cctxt
           ~force:!force block_hashes) ;
    command ~group ~desc: "Forge and inject redemption operations"
      ~args: [ force_arg ]
      (prefixes [ "reveal"; "nonces" ]
       @@ stop)
      (fun cctxt ->
         reveal_nonces cctxt ~force:!force ()) ;
  ]

let () =
  Client_commands.register Client_proto_main.protocol @@
  commands ()
