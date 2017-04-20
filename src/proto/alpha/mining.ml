(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)


open Tezos_context
open Misc

type error += Invalid_fitness_gap of int64 * int64 (* `Permanent *)
type error += Invalid_endorsement_slot of int * int (* `Permanent *)
type error += Timestamp_too_early of Timestamp.t * Timestamp.t (* `Permanent *)
type error += Wrong_delegate of public_key_hash * public_key_hash (* `Permanent *)
type error += Cannot_pay_mining_bond (* `Permanent *)
type error += Cannot_pay_endorsement_bond (* `Permanent *)

let () =
  register_error_kind
    `Permanent
    ~id:"mining.timestamp_too_early"
    ~title:"Block forged too early"
    ~description:"The block timestamp is before the first slot \
                  for this miner at this level"
    ~pp:(fun ppf (r, p) ->
      Format.fprintf ppf "Block forged too early (%a is before %a)"
                     Time.pp_hum p Time.pp_hum r)
    Data_encoding.(obj2
                     (req "minimum" Time.encoding)
                     (req "provided" Time.encoding))
    (function Timestamp_too_early (r, p)   -> Some (r, p) | _ -> None)
    (fun (r, p) -> Timestamp_too_early (r, p)) ;
  register_error_kind
    `Permanent
    ~id:"mining.invalid_fitness_gap"
    ~title:"Invalid fitness gap"
    ~description:"The gap of fitness is out of bounds"
    ~pp:(fun ppf (m, g) ->
        Format.fprintf ppf
          "The gap of fitness %Ld is not between 0 and %Ld" g m)
    Data_encoding.(obj2
                     (req "maximum" int64)
                     (req "provided" int64))
    (function Invalid_fitness_gap (m, g)   -> Some (m, g) | _ -> None)
    (fun (m, g) -> Invalid_fitness_gap (m, g)) ;
  register_error_kind
    `Permanent
    ~id:"mining.invalid_slot"
    ~title:"Invalid slot"
    ~description:"The mining slot is out of bounds"
    ~pp:(fun ppf (m, g) ->
        Format.fprintf ppf
          "The mining slot %d is not between 0 and %d" g m)
    Data_encoding.(obj2
                     (req "maximum" int16)
                     (req "provided" int16))
    (function Invalid_endorsement_slot (m, g)   -> Some (m, g) | _ -> None)
    (fun (m, g) -> Invalid_endorsement_slot (m, g)) ;
  register_error_kind
    `Permanent
    ~id:"mining.wrong_delegate"
    ~title:"Wrong delegate"
    ~description:"The block delegate is not the expected one"
    ~pp:(fun ppf (e, g) ->
        Format.fprintf ppf
          "The declared delegate %a is not %a"
          Ed25519.Public_key_hash.pp g Ed25519.Public_key_hash.pp e)
    Data_encoding.(obj2
                     (req "expected" Ed25519.Public_key_hash.encoding)
                     (req "provided" Ed25519.Public_key_hash.encoding))
    (function Wrong_delegate (e, g)   -> Some (e, g) | _ -> None)
    (fun (e, g) -> Wrong_delegate (e, g)) ;
  register_error_kind
    `Permanent
    ~id:"mining.cannot_pay_mining_bond"
    ~title:"Cannot pay mining bond"
    ~description:
      "Impossible to take the required tokens on the miner's contract"
    ~pp:(fun ppf () -> Format.fprintf ppf "Cannot pay the mining bond")
    Data_encoding.unit
    (function Cannot_pay_mining_bond -> Some () | _ -> None)
    (fun () -> Cannot_pay_mining_bond) ;
  register_error_kind
    `Permanent
    ~id:"mining.cannot_pay_endorsement_bond"
    ~title:"Cannot pay endorsement bond"
    ~description:
      "Impossible to take the required tokens on the endorser's contract"
    ~pp:(fun ppf () -> Format.fprintf ppf "Cannot pay the endorsement bond")
    Data_encoding.unit
    (function Cannot_pay_endorsement_bond -> Some () | _ -> None)
    (fun () -> Cannot_pay_endorsement_bond)

let minimal_time c priority pred_timestamp =
  let priority = Int32.of_int priority in
  let rec cumsum_slot_durations acc durations p =
    if Compare.Int32.(<=) p 0l then
      ok acc
    else match durations with
      | [] -> cumsum_slot_durations acc [ Period.one_minute ] p
      | [ last ] ->
         Period.mult p last >>? fun period ->
         Timestamp.(acc +? period)
      | first :: durations ->
         Timestamp.(acc +? first) >>? fun acc ->
         let p = Int32.pred p in
         cumsum_slot_durations acc durations p in
  Lwt.return
    (cumsum_slot_durations
       pred_timestamp (Constants.slot_durations c) (Int32.succ priority))

let check_timestamp c priority pred_timestamp =
  minimal_time c priority pred_timestamp >>=? fun minimal_time ->
  let timestamp = Tezos_context.Timestamp.current c in
  fail_unless Timestamp.(minimal_time <= timestamp)
    (Timestamp_too_early (minimal_time, timestamp))

let check_mining_rights c { Block_header.proto = { priority } }
    pred_timestamp =
  let level = Level.current c in
  Roll.mining_rights_owner c level ~priority >>=? fun delegate ->
  check_timestamp c priority pred_timestamp >>=? fun () ->
  return delegate

let pay_mining_bond c { Block_header.proto = { priority } } id =
  if Compare.Int.(priority >= Constants.first_free_mining_slot c)
  then return c
  else
    Contract.spend c (Contract.default_contract id) Constants.mining_bond_cost
    |> trace Cannot_pay_mining_bond

let pay_endorsement_bond c id =
  let bond = Constants.endorsement_bond_cost in
  Contract.spend c (Contract.default_contract id) bond
  |> trace Cannot_pay_endorsement_bond >>=? fun c ->
  return (c, bond)

let check_signing_rights c slot delegate =
  fail_unless Compare.Int.(0 <= slot && slot <= Constants.max_signing_slot c)
    (Invalid_endorsement_slot (Constants.max_signing_slot c, slot)) >>=? fun () ->
  let level = Level.current c in
  Roll.endorsement_rights_owner c level ~slot >>=? fun owning_delegate ->
  fail_unless (Ed25519.Public_key_hash.equal owning_delegate delegate)
    (Wrong_delegate (owning_delegate, delegate))

let paying_priorities c =
  0 --> Constants.first_free_mining_slot c

let bond_and_reward =
  match Tez.(Constants.mining_bond_cost +? Constants.mining_reward) with
  | Ok v -> v
  | Error _ -> assert false

let base_mining_reward c ~priority =
  if Compare.Int.(priority < Constants.first_free_mining_slot c)
  then bond_and_reward
  else Constants.mining_reward

type error += Incorect_priority

let endorsement_reward ~block_priority:prio =
  if Compare.Int.(prio >= 0)
  then
    Lwt.return
      Tez.(Constants.endorsement_reward /? (Int64.(succ (of_int prio))))
  else fail Incorect_priority

let mining_priorities c level =
  let rec f priority =
    Roll.mining_rights_owner c level ~priority >>=? fun delegate ->
    return (LCons (delegate, (fun () -> f (succ priority))))
  in
  f 0

let endorsement_priorities c level =
  let rec f slot =
    Roll.endorsement_rights_owner c level ~slot >>=? fun delegate ->
    return (LCons (delegate, (fun () -> f (succ slot))))
  in
  f 0

let select_delegate delegate delegate_list max_priority =
  let rec loop acc l n =
    if Compare.Int.(n >= max_priority)
    then return (List.rev acc)
    else
      let LCons (pkh, t) = l in
      let acc =
        if Ed25519.Public_key_hash.equal delegate pkh
        then n :: acc
        else acc in
      t () >>=? fun t ->
      loop acc t (succ n)
  in
  loop [] delegate_list 0

let first_mining_priorities
    ctxt
    ?(max_priority = Constants.first_free_mining_slot ctxt)
    delegate level =
  mining_priorities ctxt level >>=? fun delegate_list ->
  select_delegate delegate delegate_list max_priority

let first_endorsement_slots
    ctxt
    ?(max_priority = Constants.max_signing_slot ctxt)
    delegate level =
  endorsement_priorities ctxt level >>=? fun delegate_list ->
  select_delegate delegate delegate_list max_priority

let check_hash hash stamp_threshold =
  let bytes = Block_hash.to_string hash in
  let word = String.get_int64 bytes 0 in
  Compare.Uint64.(word < stamp_threshold)

let check_header_hash header stamp_threshold =
  let hash = Block_header.hash header in
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
  let check_signature key { Block_header.proto ; shell ; signature } =
    let unsigned_header = Block_header.forge_unsigned shell proto in
    Ed25519.Signature.check key signature unsigned_header in
  if check_signature key block then
    return ()
  else
    fail Invalid_signature

let max_fitness_gap ctxt =
  let slots = Int64.of_int (Constants.max_signing_slot ctxt + 1) in
  Int64.add slots 1L

let check_fitness_gap ctxt (block : Block_header.t) =
  let current_fitness = Fitness.current ctxt in
  Lwt.return (Fitness.to_int64 block.shell.fitness) >>=? fun announced_fitness ->
  let gap = Int64.sub announced_fitness current_fitness in
  if Compare.Int64.(gap <= 0L || max_fitness_gap ctxt < gap) then
    fail (Invalid_fitness_gap (max_fitness_gap ctxt, gap))
  else
    return ()

let last_of_a_cycle ctxt l =
  Compare.Int32.(Int32.succ l.Level.cycle_position =
                 Constants.cycle_length ctxt)

let dawn_of_a_new_cycle ctxt =
  let level = Level.current ctxt in
  if last_of_a_cycle ctxt level then
    return (Some level.cycle)
  else
    return None

