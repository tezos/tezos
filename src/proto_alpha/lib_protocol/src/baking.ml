(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)


open Alpha_context
open Misc

type error += Invalid_fitness_gap of int64 * int64 (* `Permanent *)
type error += Invalid_endorsement_slot of int * int (* `Permanent *)
type error += Timestamp_too_early of Timestamp.t * Timestamp.t (* `Permanent *)
type error += Cannot_freeze_baking_deposit (* `Permanent *)
type error += Cannot_freeze_endorsement_deposit (* `Permanent *)
type error += Inconsistent_endorsement of public_key_hash list (* `Permanent *)
type error += Empty_endorsement
type error += Invalid_block_signature of Block_hash.t * Ed25519.Public_key_hash.t (* `Permanent *)
type error += Invalid_signature  (* `Permanent *)
type error += Invalid_stamp  (* `Permanent *)

let () =
  register_error_kind
    `Permanent
    ~id:"baking.timestamp_too_early"
    ~title:"Block forged too early"
    ~description:"The block timestamp is before the first slot \
                  for this baker at this level"
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
    ~id:"baking.invalid_fitness_gap"
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
    ~id:"baking.invalid_slot"
    ~title:"Invalid slot"
    ~description:"The baking slot is out of bounds"
    ~pp:(fun ppf (m, g) ->
        Format.fprintf ppf
          "The baking slot %d is not between 0 and %d" g m)
    Data_encoding.(obj2
                     (req "maximum" int16)
                     (req "provided" int16))
    (function Invalid_endorsement_slot (m, g)   -> Some (m, g) | _ -> None)
    (fun (m, g) -> Invalid_endorsement_slot (m, g)) ;
  register_error_kind
    `Permanent
    ~id:"baking.cannot_freeze_baking_deposit"
    ~title:"Cannot freeze baking deposit"
    ~description:
      "Impossible to debit the required tokens on the baker's contract"
    ~pp:(fun ppf () -> Format.fprintf ppf "Cannot freeze the baking deposit")
    Data_encoding.unit
    (function Cannot_freeze_baking_deposit -> Some () | _ -> None)
    (fun () -> Cannot_freeze_baking_deposit) ;
  register_error_kind
    `Permanent
    ~id:"baking.cannot_freeze_endorsement_deposit"
    ~title:"Cannot freeze endorsement deposit"
    ~description:
      "Impossible to debit the required tokens on the endorser's contract"
    ~pp:(fun ppf () -> Format.fprintf ppf "Cannot freeze the endorsement deposit")
    Data_encoding.unit
    (function Cannot_freeze_endorsement_deposit -> Some () | _ -> None)
    (fun () -> Cannot_freeze_endorsement_deposit) ;
  register_error_kind
    `Permanent
    ~id:"baking.inconsisten_endorsement"
    ~title:"Multiple delegates for a single endorsement"
    ~description:"The operation tries to endorse slots with distinct delegates"
    ~pp:(fun ppf l ->
        Format.fprintf ppf
          "@[<v 2>The endorsement is inconsistent. Delegates:@ %a@]"
          (Format.pp_print_list Ed25519.Public_key_hash.pp) l)
    Data_encoding.(obj1
                     (req "delegates" (list Ed25519.Public_key_hash.encoding)))
    (function Inconsistent_endorsement l -> Some l | _ -> None)
    (fun l -> Inconsistent_endorsement l) ;
  register_error_kind
    `Permanent
    ~id:"baking.invalid_block_signature"
    ~title:"Invalid block signature"
    ~description:
      "A block was not signed with the expected private key."
    ~pp:(fun ppf (block, pkh) ->
        Format.fprintf ppf "Invalid signature for block %a. Expected: %a."
          Block_hash.pp_short block
          Ed25519.Public_key_hash.pp_short pkh)
    Data_encoding.(obj2
                     (req "block" Block_hash.encoding)
                     (req "expected" Ed25519.Public_key_hash.encoding))
    (function Invalid_block_signature (block, pkh) -> Some (block, pkh) | _ -> None)
    (fun (block, pkh) -> Invalid_block_signature (block, pkh));
  register_error_kind
    `Permanent
    ~id:"baking.invalid_signature"
    ~title:"Invalid block signature"
    ~description:"The block's signature is invalid"
    ~pp:(fun ppf () ->
        Format.fprintf ppf "Invalid block signature")
    Data_encoding.empty
    (function Invalid_signature -> Some () | _ -> None)
    (fun () -> Invalid_signature) ;
  register_error_kind
    `Permanent
    ~id:"baking.insufficient_proof_of_work"
    ~title:"Insufficient block proof-of-work stamp"
    ~description:"The block's proof-of-work stamp is insufficient"
    ~pp:(fun ppf () ->
        Format.fprintf ppf "Insufficient proof-of-work stamp")
    Data_encoding.empty
    (function Invalid_stamp -> Some () | _ -> None)
    (fun () -> Invalid_stamp)


let minimal_time c priority pred_timestamp =
  let priority = Int32.of_int priority in
  let rec cumsum_time_between_blocks acc durations p =
    if Compare.Int32.(<=) p 0l then
      ok acc
    else match durations with
      | [] -> cumsum_time_between_blocks acc [ Period.one_minute ] p
      | [ last ] ->
          Period.mult p last >>? fun period ->
          Timestamp.(acc +? period)
      | first :: durations ->
          Timestamp.(acc +? first) >>? fun acc ->
          let p = Int32.pred p in
          cumsum_time_between_blocks acc durations p in
  Lwt.return
    (cumsum_time_between_blocks
       pred_timestamp (Constants.time_between_blocks c) (Int32.succ priority))

let freeze_baking_deposit ctxt { Block_header.priority ; _ } delegate =
  if Compare.Int.(priority >= Constants.first_free_baking_slot ctxt)
  then return (ctxt, Tez.zero)
  else
    let deposit = Constants.block_security_deposit in
    Delegate.freeze_deposit ctxt delegate deposit
    |> trace Cannot_freeze_baking_deposit >>=? fun ctxt ->
    return (ctxt, deposit)

let freeze_endorsement_deposit ctxt delegate =
  let deposit = Constants.endorsement_security_deposit in
  Delegate.freeze_deposit ctxt delegate deposit
  |> trace Cannot_freeze_endorsement_deposit

let check_timestamp c priority pred_timestamp =
  minimal_time c priority pred_timestamp >>=? fun minimal_time ->
  let timestamp = Alpha_context.Timestamp.current c in
  fail_unless Timestamp.(minimal_time <= timestamp)
    (Timestamp_too_early (minimal_time, timestamp))

let check_baking_rights c { Block_header.priority ; _ }
    pred_timestamp =
  let level = Level.current c in
  Roll.baking_rights_owner c level ~priority >>=? fun delegate ->
  check_timestamp c priority pred_timestamp >>=? fun () ->
  return delegate

let check_endorsements_rights c level slots =
  map_p (fun slot ->
      fail_unless Compare.Int.(0 <= slot && slot <= Constants.endorsers_per_block c)
        (Invalid_endorsement_slot (Constants.endorsers_per_block c, slot)) >>=? fun () ->
      Roll.endorsement_rights_owner c level ~slot)
    slots >>=? function
  | [] -> fail Empty_endorsement
  | delegate :: delegates as all_delegates ->
      fail_unless
        (List.for_all (fun d -> Ed25519.Public_key.equal d delegate) delegates)
        (Inconsistent_endorsement (List.map Ed25519.Public_key.hash all_delegates)) >>=? fun () ->
      return delegate

let paying_priorities c =
  0 --> (Constants.first_free_baking_slot c - 1)

type error += Incorect_priority

let endorsement_reward ~block_priority:prio =
  if Compare.Int.(prio >= 0)
  then
    Lwt.return
      Tez.(Constants.endorsement_reward /? (Int64.(succ (of_int prio))))
  else fail Incorect_priority

let baking_priorities c level =
  let rec f priority =
    Roll.baking_rights_owner c level ~priority >>=? fun delegate ->
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
      let LCons (pk, t) = l in
      let acc =
        if Ed25519.Public_key_hash.equal delegate (Ed25519.Public_key.hash pk)
        then n :: acc
        else acc in
      t () >>=? fun t ->
      loop acc t (succ n)
  in
  loop [] delegate_list 0

let first_baking_priorities
    ctxt
    ?(max_priority = Constants.first_free_baking_slot ctxt)
    delegate level =
  baking_priorities ctxt level >>=? fun delegate_list ->
  select_delegate delegate delegate_list max_priority

let first_endorsement_slots
    ctxt
    ?(max_priority = Constants.endorsers_per_block ctxt)
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

let check_proof_of_work_stamp ctxt block =
  let proof_of_work_threshold = Constants.proof_of_work_threshold ctxt in
  if check_header_hash block proof_of_work_threshold then
    return ()
  else
    fail Invalid_stamp

let check_signature block key =
  let check_signature key { Block_header.protocol_data ; shell ; signature } =
    let unsigned_header = Block_header.forge_unsigned shell protocol_data in
    Ed25519.Signature.check key signature unsigned_header in
  if check_signature key block then
    return ()
  else
    fail (Invalid_block_signature (Block_header.hash block,
                                   Ed25519.Public_key.hash key))

let max_fitness_gap ctxt =
  let slots = Int64.of_int (Constants.endorsers_per_block ctxt + 1) in
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
                 Constants.blocks_per_cycle ctxt)

let dawn_of_a_new_cycle ctxt =
  let level = Level.current ctxt in
  if last_of_a_cycle ctxt level then
    return (Some level.cycle)
  else
    return None
