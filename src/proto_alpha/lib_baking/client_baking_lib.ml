(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Proto_alpha
open Alpha_context

let bake_block (cctxt : #Proto_alpha.full_context) block
    ?force ?max_priority ?(free_baking=false) ?src_sk delegate =
  begin
    match src_sk with
    | None ->
        Client_keys.get_key cctxt delegate >>=? fun (_, _, src_sk) ->
        return src_sk
    | Some sk -> return sk
  end >>=? fun src_sk ->
  Alpha_services.Context.level cctxt block >>=? fun level ->
  let level = Raw_level.succ level.level in
  let seed_nonce = Client_baking_forge.generate_seed_nonce () in
  let seed_nonce_hash = Nonce.hash seed_nonce in
  Client_baking_forge.forge_block cctxt
    ~timestamp:(Time.now ())
    ?force
    ~seed_nonce_hash ~src_sk block
    ~priority:(`Auto (delegate, max_priority, free_baking)) () >>=? fun block_hash ->
  Client_baking_forge.State.record_block cctxt level block_hash seed_nonce
  |> trace_exn (Failure "Error while recording block") >>=? fun () ->
  cctxt#message "Injected block %a" Block_hash.pp_short block_hash >>= fun () ->
  return ()

let endorse_block cctxt ?max_priority delegate =
  Client_keys.get_key cctxt delegate >>=? fun (_src_name, src_pk, src_sk) ->
  Client_baking_endorsement.forge_endorsement cctxt
    cctxt#block ?max_priority ~src_sk src_pk >>=? fun oph ->
  cctxt#answer "Operation successfully injected in the node." >>= fun () ->
  cctxt#answer "Operation hash is '%a'." Operation_hash.pp oph >>= fun () ->
  return ()

let get_predecessor_cycle (cctxt : #Client_context.logger) cycle =
  match Cycle.pred cycle with
  | None ->
      if Cycle.(cycle = root) then
        cctxt#error "No predecessor for the first cycle"
      else
        cctxt#error
          "Cannot compute the predecessor of cycle %a"
          Cycle.pp cycle
  | Some cycle -> Lwt.return cycle

let do_reveal cctxt block blocks =
  let nonces = List.map snd blocks in
  Client_baking_revelation.forge_seed_nonce_revelation cctxt
    block nonces >>=? fun () ->
  Client_baking_nonces.dels cctxt (List.map fst blocks) >>=? fun () ->
  return ()

let reveal_block_nonces (cctxt : #Proto_alpha.full_context) block_hashes =
  Lwt_list.filter_map_p
    (fun hash ->
       Lwt.catch
         (fun () ->
            Client_baking_blocks.info cctxt (`Hash hash) >>= function
            | Ok bi -> Lwt.return (Some bi)
            | Error _ ->
                Lwt.fail Not_found)
         (fun _ ->
            cctxt#warning
              "Cannot find block %a in the chain. (ignoring)@."
              Block_hash.pp_short hash >>= fun () ->
            Lwt.return_none))
    block_hashes >>= fun block_infos ->
  filter_map_s (fun (bi : Client_baking_blocks.block_info) ->
      Client_baking_nonces.find cctxt bi.hash >>=? function
      | None ->
          cctxt#warning "Cannot find nonces for block %a (ignoring)@."
            Block_hash.pp_short bi.hash >>= fun () ->
          return None
      | Some nonce ->
          return (Some (bi.hash, (bi.level.level, nonce))))
    block_infos >>=? fun blocks ->
  do_reveal cctxt cctxt#block blocks

let reveal_nonces cctxt () =
  let block = Block_services.last_baked_block cctxt#block in
  Client_baking_forge.get_unrevealed_nonces
    cctxt block >>=? fun nonces ->
  do_reveal cctxt cctxt#block nonces

let run_daemon cctxt ?max_priority ~endorsement_delay delegates ~endorsement ~baking ~denunciation =
  Client_baking_daemon.run cctxt
    ?max_priority
    ~delay:endorsement_delay
    ~min_date:((Time.add (Time.now ()) (Int64.neg 1800L)))
    ~endorsement ~baking ~denunciation
    (List.map snd delegates)
