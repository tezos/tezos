(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2018 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

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
    let workers = Prevalidator.running_workers () in
    let statuses =
      List.map
        (fun (chain_id, _, t) -> (chain_id, Prevalidator.status t))
        workers in
    return statuses
  end ;

  register1 Worker_services.Prevalidators.S.state begin fun chain () () ->
    Chain_directory.get_chain_id state chain >>= fun chain_id ->
    let workers = Prevalidator.running_workers () in
    let (_, _, t) =
      (* NOTE: it is technically possible to use the Prevalidator interface to
       * register multiple Prevalidator for a single chain (using distinct
       * protocols). However, this is never done. *)
      List.find (fun (c, _, _) -> Chain_id.equal c chain_id) workers in
    let status = Prevalidator.status t in
    let pending_requests = Prevalidator.pending_requests t in
    let backlog = Prevalidator.last_events t in
    let current_request = Prevalidator.current_request t in
    return
      { Worker_types.
        status ;
        pending_requests ;
        backlog ;
        current_request }
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
