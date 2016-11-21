(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Misc
open Tezos_context

type error +=
  | Too_early of Timestamp.t * Timestamp.t
  | Invalid_level of Raw_level.t * Raw_level.t
  | Cannot_pay_mining_bond
  | Cannot_pay_endorsement_bond
  | Bad_slot
  | Bad_delegate

let minimal_time c priority =
  Timestamp.get_current c >>=? fun prev_timestamp ->
  Lwt.return
    (Period.mult (Int32.succ priority)
       (Constants.time_between_slots c)) >>=? fun period ->
  Lwt.return Timestamp.(prev_timestamp +? period)

let check_timestamp c priority timestamp =
  minimal_time c priority >>=? fun minimal_time ->
  fail_unless Timestamp.(minimal_time <= timestamp)
    (Too_early (minimal_time, timestamp))

let check_mining_rights c
    { Block.shell = { timestamp } ;
      proto = { mining_slot = (raw_level, priority) } } =
  Level.current c >>=? fun current_level ->
  fail_unless
    Raw_level.(raw_level = current_level.level)
    (Invalid_level (current_level.Level.level, raw_level)) >>=? fun () ->
  let level = Level.from_raw c raw_level in
  Roll.mining_rights_owner c level ~priority >>=? fun delegate ->
  check_timestamp c priority timestamp >>=? fun () ->
  return delegate

let pay_mining_bond c
    { Block.proto = { mining_slot = (_raw_level, priority) } }
    id =
  if Compare.Int32.(priority >= Constants.first_free_mining_slot c)
  then return c
  else
    Contract.unconditional_spend c
      (Contract.default_contract id) Constants.mining_bond_cost
    |> trace Cannot_pay_mining_bond

let pay_endorsement_bond c id =
  let bond = Constants.endorsement_bond_cost in
  Contract.unconditional_spend c (Contract.default_contract id) bond
  |> trace Cannot_pay_endorsement_bond >>=? fun c ->
  return (c, bond)

let check_signing_rights c slot delegate =
  fail_unless Compare.Int.(slot <= Constants.max_signing_slot c)
    Bad_slot >>=? fun () ->
  Level.current c >>=? fun level ->
  Roll.endorsement_rights_owner c level ~slot >>=? fun owning_delegate ->
  fail_unless (Ed25519.Public_key_hash.equal owning_delegate delegate)
    Bad_delegate

let paying_priorities c =
  0l ---> Constants.first_free_mining_slot c

let bond_and_reward =
  match Tez.(Constants.mining_bond_cost +? Constants.mining_reward) with
  | Ok v -> v
  | Error _ -> assert false

let base_mining_reward c ~priority =
  if Compare.Int32.(priority < Constants.first_free_mining_slot c)
  then bond_and_reward
  else Constants.mining_reward

type error += Incorect_priority

let endorsement_reward ~block_priority:prio =
  if Compare.Int32.(prio >= 0l)
  then
    return
      Tez.(Constants.endorsement_reward / (Int64.(succ (of_int32 prio))))
  else fail Incorect_priority

let mining_priorities c level =
  let rec f priority =
    Roll.mining_rights_owner c level ~priority >>=? fun delegate ->
    return (LCons (delegate, (fun () -> f (Int32.succ priority))))
  in
  f 0l

let endorsement_priorities c level =
  let rec f slot =
    Roll.endorsement_rights_owner c level ~slot >>=? fun delegate ->
    return (LCons (delegate, (fun () -> f (succ slot))))
  in
  f 0

let select_delegate delegate delegate_list max_priority =
  let rec loop acc l n =
    if Compare.Int32.(n >= max_priority)
    then return (List.rev acc)
    else
      let LCons (pkh, t) = l in
      let acc =
        if Ed25519.Public_key_hash.equal delegate pkh
        then n :: acc
        else acc in
      t () >>=? fun t ->
      loop acc t (Int32.succ n)
  in
  loop [] delegate_list 0l

let first_mining_priorities
    ctxt
    ?(max_priority = Constants.first_free_mining_slot ctxt)
    delegate level =
  mining_priorities ctxt level >>=? fun delegate_list ->
  select_delegate delegate delegate_list max_priority

let first_endorsement_slots
    ctxt
    ?(max_priority =
      Int32.of_int (Constants.max_signing_slot ctxt))
    delegate level =
  endorsement_priorities ctxt level >>=? fun delegate_list ->
  select_delegate delegate delegate_list max_priority

let check_hash hash stamp_threshold =
  let bytes = Block_hash.to_raw hash in
  let word = String.get_int64 bytes 0 in
  Compare.Uint64.(word < stamp_threshold)

let check_header_hash {Block.shell;proto;signature} stamp_threshold =
  let hash =
    Block_hash.hash_bytes [
      Data_encoding.Binary.to_bytes
        (Data_encoding.tup2
           Block.unsigned_header_encoding Ed25519.signature_encoding)
        ((shell, proto), signature)] in
  check_hash hash stamp_threshold

type error +=
  | Invalid_signature
  | Invalid_stamp

let check_proof_of_work_stamp ctxt block =
  let proof_of_work_threshold = Constants.proof_of_work_threshold ctxt in
  if check_header_hash block proof_of_work_threshold then
    return ()
  else
    fail Invalid_stamp

let check_signature ctxt block id =
  Public_key.get ctxt id >>=? fun key ->
  let check_signature key { Block.proto ; shell ; signature } =
    let unsigned_header = Block.forge_header shell proto in
    Ed25519.check_signature key signature unsigned_header in
  if check_signature key block then
    return ()
  else
    fail Invalid_signature

let max_fitness_gap ctxt =
  let slots = Int64.of_int (Constants.max_signing_slot ctxt + 1) in
  Int64.add slots 1L

type error += Invalid_fitness_gap

let check_fitness_gap ctxt (block : Block.header)  =
  Fitness.raw_get ctxt >>=? fun current_fitness ->
  Fitness.raw_read block.shell.fitness >>=? fun announced_fitness ->
  let gap = Int64.sub announced_fitness current_fitness in
  if Compare.Int64.(gap <= 0L || max_fitness_gap ctxt < gap) then
    fail Invalid_fitness_gap
  else
    return ()

let first_of_a_cycle l =
  Compare.Int32.(l.Level.cycle_position = 0l)

let dawn_of_a_new_cycle ctxt =
  Level.current ctxt >>=? fun level ->
  if first_of_a_cycle level then
    return (Some level.cycle)
  else
    return None

