(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

let build_rpc_directory state =

  let dir : unit RPC_directory.t ref = ref RPC_directory.empty in
  let register0 s f =
    dir := RPC_directory.register !dir s (fun () p q -> f p q) in
  let register1 s f =
    dir := RPC_directory.register !dir s (fun ((), a) p q -> f a p q) in
  let register2 s f =
    dir := RPC_directory.register !dir s (fun (((), a), b) p q -> f a b p q) in

  (* Workers : Prevalidators *)

  register0  Worker_services.Prevalidators.S.list begin fun () () ->
    return
      (List.map
         (fun (id, w) -> (id, Prevalidator.status w))
         (Prevalidator.running_workers ()))
  end ;

  register1 Worker_services.Prevalidators.S.state begin fun chain () () ->
    Chain_directory.get_chain_id state chain >>= fun chain_id ->
    let w = List.assoc chain_id (Prevalidator.running_workers ()) in
    return
      { Worker_types.status = Prevalidator.status w ;
        pending_requests = Prevalidator.pending_requests w ;
        backlog = Prevalidator.last_events w ;
        current_request = Prevalidator.current_request w }
  end ;

  (* Workers : Block_validator *)

  register0 Worker_services.Block_validator.S.state begin fun () () ->
    let w = Block_validator.running_worker () in
    return
      { Worker_types.status = Block_validator.status w ;
        pending_requests = Block_validator.pending_requests w ;
        backlog = Block_validator.last_events w ;
        current_request = Block_validator.current_request w }
  end ;

  (* Workers : Peer validators *)

  register1 Worker_services.Peer_validators.S.list begin fun chain () () ->
    Chain_directory.get_chain_id state chain >>= fun chain_id ->
    return
      (List.filter_map
         (fun ((id, peer_id), w) ->
            if Chain_id.equal id chain_id then
              Some (peer_id, Peer_validator.status w)
            else None)
         (Peer_validator.running_workers ()))
  end ;

  register2 Worker_services.Peer_validators.S.state begin fun chain peer_id () () ->
    Chain_directory.get_chain_id state chain >>= fun chain_id ->
    let w = List.assoc (chain_id, peer_id) (Peer_validator.running_workers ()) in
    return
      { Worker_types.status = Peer_validator.status w ;
        pending_requests = [] ;
        backlog = Peer_validator.last_events w ;
        current_request = Peer_validator.current_request w }
  end ;

  (* Workers : Net validators *)

  register0 Worker_services.Chain_validators.S.list begin fun () () ->
    return
      (List.map
         (fun (id, w) -> (id, Chain_validator.status w))
         (Chain_validator.running_workers ()))
  end ;

  register1 Worker_services.Chain_validators.S.state begin fun chain () () ->
    Chain_directory.get_chain_id state chain >>= fun chain_id ->
    let w = List.assoc chain_id (Chain_validator.running_workers ()) in
    return
      { Worker_types.status = Chain_validator.status w ;
        pending_requests = Chain_validator.pending_requests w ;
        backlog = Chain_validator.last_events w ;
        current_request = Chain_validator.current_request w }
  end ;

  !dir
