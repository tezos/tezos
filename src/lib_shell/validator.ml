(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

include Logging.Make(struct let name = "node.validator" end)

type t = {

  state: State.t ;
  db: Distributed_db.t ;
  block_validator: Block_validator.t ;
  chain_validator_limits: Chain_validator.limits ;
  peer_validator_limits: Peer_validator.limits ;
  block_validator_limits: Block_validator.limits ;
  prevalidator_limits: Prevalidator.limits ;

  valid_block_input: State.Block.t Lwt_watcher.input ;
  active_chains: Chain_validator.t Lwt.t Chain_id.Table.t ;

}

let create state db
    peer_validator_limits
    block_validator_limits
    prevalidator_limits
    chain_validator_limits =
  Block_validator.create block_validator_limits db >>= fun block_validator ->
  let valid_block_input = Lwt_watcher.create_input () in
  Lwt.return
    { state ; db ; block_validator ;
      block_validator_limits ; prevalidator_limits ;
      peer_validator_limits ; chain_validator_limits ;
      valid_block_input ;
      active_chains = Chain_id.Table.create 7 }

let activate v ?max_child_ttl chain_state =
  let chain_id = State.Chain.id chain_state in
  lwt_log_notice "activate chain %a" Chain_id.pp chain_id >>= fun () ->
  try Chain_id.Table.find v.active_chains chain_id
  with Not_found ->
    let nv =
      Chain_validator.create
        ?max_child_ttl
        v.peer_validator_limits v.prevalidator_limits
        v.block_validator v.valid_block_input v.db chain_state
        v.chain_validator_limits in
    Chain_id.Table.add v.active_chains chain_id nv ;
    nv

let get_exn { active_chains } chain_id =
  Chain_id.Table.find active_chains chain_id

type error +=
  | Inactive_chain of Chain_id.t

let () =
  register_error_kind `Branch
    ~id: "node.validator.inactive_chain"
    ~title: "Inactive chain"
    ~description: "Attempted validation of a block from an inactive chain."
    ~pp: (fun ppf chain ->
        Format.fprintf ppf
          "Tried to validate a block from chain %a, \
           that is not currently considered active."
          Chain_id.pp chain)
    Data_encoding.(obj1 (req "inactive_chain" Chain_id.encoding))
    (function Inactive_chain chain -> Some chain | _ -> None)
    (fun chain -> Inactive_chain chain)

let get v chain_id =
  try get_exn v chain_id >>= fun nv -> return nv
  with Not_found -> fail (Inactive_chain chain_id)

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
            | Some (chain_id, _bh) -> get v chain_id
          end
        | Some chain_id ->
            get v chain_id >>=? fun nv ->
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

let shutdown { active_chains ; block_validator } =
  let jobs =
    Block_validator.shutdown block_validator ::
    Chain_id.Table.fold
      (fun _ nv acc -> (nv >>= Chain_validator.shutdown) :: acc)
      active_chains [] in
  Lwt.join jobs >>= fun () ->
  Lwt.return_unit

let watcher { valid_block_input } =
  Lwt_watcher.create_stream valid_block_input

let inject_operation v ?chain_id op =
  begin
    match chain_id with
    | None -> begin
        Distributed_db.read_block_header
          v.db op.Operation.shell.branch >>= function
        | None ->
            failwith "Unknown branch (%a), cannot inject the operation."
              Block_hash.pp_short op.shell.branch
        | Some (chain_id, _bh) -> get v chain_id
      end
    | Some chain_id ->
        get v chain_id >>=? fun nv ->
        Distributed_db.Block_header.known
          (Chain_validator.chain_db nv)
          op.shell.branch >>= function
        | true ->
            return nv
        | false ->
            failwith "Unknown branch (%a), cannot inject the operation."
              Block_hash.pp_short op.shell.branch
  end >>=? fun nv ->
  let pv = Chain_validator.prevalidator nv in
  Prevalidator.inject_operation pv op
