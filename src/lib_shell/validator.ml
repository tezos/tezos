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

include Internal_event.Legacy_logging.Make_semantic(struct let name = "node.validator" end)

type t = {

  state: State.t ;
  db: Distributed_db.t ;
  block_validator: Block_validator.t ;
  chain_validator_limits: Chain_validator.limits ;
  peer_validator_limits: Peer_validator.limits ;
  block_validator_limits: Block_validator.limits ;
  prevalidator_limits: Prevalidator.limits ;
  start_testchain: bool ;

  valid_block_input: State.Block.t Lwt_watcher.input ;

  chains_input: (Chain_id.t * bool) Lwt_watcher.input ;
  active_chains: Chain_validator.t Chain_id.Table.t ;

}

let create state db
    peer_validator_limits
    block_validator_limits
    block_validator_kind
    prevalidator_limits
    chain_validator_limits
    ~start_testchain
  =
  Block_validator.create block_validator_limits db block_validator_kind ~start_testchain >>=? fun block_validator ->
  let valid_block_input = Lwt_watcher.create_input () in
  let chains_input = Lwt_watcher.create_input () in
  return
    { state ; db ;
      start_testchain ;
      block_validator ;
      block_validator_limits ; prevalidator_limits ;
      peer_validator_limits ; chain_validator_limits ;
      valid_block_input ;
      chains_input ;
      active_chains = Chain_id.Table.create 7 }

let activate v ?max_child_ttl
    ~start_prevalidator
    chain_state =
  let chain_id = State.Chain.id chain_state in
  lwt_log_notice Tag.DSL.(fun f ->
      f "activate chain %a"
      -% t event "active_chain"
      -% a State_logging.chain_id chain_id) >>= fun () ->
  match Chain_id.Table.find_opt v.active_chains chain_id with
  | Some chain -> return chain
  | None ->
      Chain_validator.create
        ?max_child_ttl
        ~start_prevalidator
        ~start_testchain:v.start_testchain
        ~active_chains:v.active_chains
        v.peer_validator_limits v.prevalidator_limits
        v.block_validator
        v.valid_block_input
        v.chains_input
        v.db chain_state
        v.chain_validator_limits

let get_exn { active_chains ; _ } chain_id =
  Chain_id.Table.find active_chains chain_id

let get { active_chains ; _ } chain_id =
  match Chain_id.Table.find_opt active_chains chain_id with
  |Some nv -> Ok nv
  |None -> error (Validation_errors.Inactive_chain chain_id)

let get_active_chains { active_chains ; _ } =
  let l = Chain_id.Table.fold (fun c _ acc -> c :: acc) active_chains [] in
  List.rev l

let validate_block v ?(force = false) ?chain_id bytes operations =
  let hash = Block_hash.hash_bytes [bytes] in
  match Block_header.of_bytes bytes with
  | None -> failwith "Cannot parse block header."
  | Some block ->
      begin
        match chain_id with
        | None -> begin
            Distributed_db.read_block_header
              v.db block.shell.predecessor >>= function
            | None ->
                failwith "Unknown predecessor (%a), cannot inject the block."
                  Block_hash.pp_short block.shell.predecessor
            | Some (chain_id, _bh) -> Lwt.return (get v chain_id)
          end
        | Some chain_id ->
            Lwt.return (get v chain_id) >>=? fun nv ->
            if force then
              return nv
            else
              Distributed_db.Block_header.known
                (Chain_validator.chain_db nv)
                block.shell.predecessor >>= function
              | true ->
                  return nv
              | false ->
                  failwith "Unknown predecessor (%a), cannot inject the block."
                    Block_hash.pp_short block.shell.predecessor
      end >>=? fun nv ->
      let validation =
        Chain_validator.validate_block nv ~force hash block operations in
      return (hash, validation)

let shutdown { active_chains ; block_validator ; _ } =
  let jobs =
    Block_validator.shutdown block_validator ::
    Chain_id.Table.fold
      (fun _ nv acc -> Chain_validator.shutdown nv :: acc)
      active_chains [] in
  Lwt.join jobs >>= fun () ->
  Lwt.return_unit

let watcher { valid_block_input ; _ } =
  Lwt_watcher.create_stream valid_block_input

let chains_watcher { chains_input ; _ } =
  Lwt_watcher.create_stream chains_input

let inject_operation v ?chain_id op =
  begin
    match chain_id with
    | None -> begin
        Distributed_db.read_block_header
          v.db op.Operation.shell.branch >>= function
        | None ->
            failwith "Unknown branch (%a), cannot inject the operation."
              Block_hash.pp_short op.shell.branch
        | Some (chain_id, _bh) -> Lwt.return (get v chain_id)
      end
    | Some chain_id ->
        Lwt.return (get v chain_id) >>=? fun nv ->
        Distributed_db.Block_header.known
          (Chain_validator.chain_db nv)
          op.shell.branch >>= function
        | true ->
            return nv
        | false ->
            failwith "Unknown branch (%a), cannot inject the operation."
              Block_hash.pp_short op.shell.branch
  end >>=? fun nv ->
  let pv_opt = Chain_validator.prevalidator nv in
  match pv_opt with
  | Some pv -> Prevalidator.inject_operation pv op
  | None -> failwith "Prevalidator is not running, cannot inject the operation."

let distributed_db { db ; _ } = db
