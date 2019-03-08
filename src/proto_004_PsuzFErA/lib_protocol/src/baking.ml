(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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


open Alpha_context
open Misc

type error += Invalid_fitness_gap of int64 * int64 (* `Permanent *)
type error += Timestamp_too_early of Timestamp.t * Timestamp.t (* `Permanent *)
type error += Unexpected_endorsement (* `Permanent *)
type error += Invalid_block_signature of Block_hash.t * Signature.Public_key_hash.t (* `Permanent *)
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
    ~id:"baking.invalid_block_signature"
    ~title:"Invalid block signature"
    ~description:
      "A block was not signed with the expected private key."
    ~pp:(fun ppf (block, pkh) ->
        Format.fprintf ppf "Invalid signature for block %a. Expected: %a."
          Block_hash.pp_short block
          Signature.Public_key_hash.pp_short pkh)
    Data_encoding.(obj2
                     (req "block" Block_hash.encoding)
                     (req "expected" Signature.Public_key_hash.encoding))
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
    (fun () -> Invalid_stamp) ;
  register_error_kind
    `Permanent
    ~id:"baking.unexpected_endorsement"
    ~title:"Endorsement from unexpected delegate"
    ~description:"The operation is signed by a delegate without endorsement rights."
    ~pp:(fun ppf () ->
        Format.fprintf ppf
          "The endorsement is signed by a delegate without endorsement rights.")
    Data_encoding.unit
    (function Unexpected_endorsement -> Some () | _ -> None)
    (fun () -> Unexpected_endorsement)

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

let earlier_predecessor_timestamp ctxt level =
  let current = Level.current ctxt in
  let current_timestamp = Timestamp.current ctxt in
  let gap = Level.diff level current in
  let step = List.hd (Constants.time_between_blocks ctxt) in
  if Compare.Int32.(gap < 1l) then
    failwith "Baking.earlier_block_timestamp: past block."
  else
    Lwt.return (Period.mult (Int32.pred gap) step) >>=? fun delay ->
    Lwt.return Timestamp.(current_timestamp +? delay) >>=? fun result ->
    return result

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

type error += Incorrect_priority (* `Permanent *)

let () =
  register_error_kind
    `Permanent
    ~id:"incorrect_priority"
    ~title:"Incorrect priority"
    ~description:"Block priority must be non-negative."
    ~pp:(fun ppf () ->
        Format.fprintf ppf "The block priority must be non-negative.")
    Data_encoding.unit
    (function Incorrect_priority -> Some () | _ -> None)
    (fun () -> Incorrect_priority)

let endorsement_reward ctxt ~block_priority:prio n =
  if Compare.Int.(prio >= 0)
  then
    Lwt.return
      Tez.(Constants.endorsement_reward ctxt /? (Int64.(succ (of_int prio)))) >>=? fun tez ->
    Lwt.return Tez.(tez *? Int64.of_int n)
  else fail Incorrect_priority

let baking_priorities c level =
  let rec f priority =
    Roll.baking_rights_owner c level ~priority >>=? fun delegate ->
    return (LCons (delegate, (fun () -> f (succ priority))))
  in
  f 0

let endorsement_rights c level =
  fold_left_s
    (fun acc slot ->
       Roll.endorsement_rights_owner c level ~slot >>=? fun pk ->
       let pkh = Signature.Public_key.hash pk in
       let right =
         match Signature.Public_key_hash.Map.find_opt pkh acc with
         | None -> (pk, [slot], false)
         | Some (pk, slots, used) -> (pk, slot :: slots, used) in
       return (Signature.Public_key_hash.Map.add pkh right acc))
    Signature.Public_key_hash.Map.empty
    (0 --> (Constants.endorsers_per_block c - 1))

let check_endorsement_rights ctxt chain_id (op : Kind.endorsement Operation.t) =
  let current_level = Level.current ctxt in
  let Single (Endorsement { level ; _ }) = op.protocol_data.contents in
  begin
    if Raw_level.(succ level = current_level.level) then
      return (Alpha_context.allowed_endorsements ctxt)
    else
      endorsement_rights ctxt (Level.from_raw ctxt level)
  end >>=? fun endorsements ->
  match
    Signature.Public_key_hash.Map.fold (* no find_first *)
      (fun pkh (pk, slots, used) acc ->
         match Operation.check_signature_sync pk chain_id op with
         | Error _ -> acc
         | Ok () -> Some (pkh, slots, used))
      endorsements None
  with
  | None -> fail Unexpected_endorsement
  | Some v -> return v

let select_delegate delegate delegate_list max_priority =
  let rec loop acc l n =
    if Compare.Int.(n >= max_priority)
    then return (List.rev acc)
    else
      let LCons (pk, t) = l in
      let acc =
        if Signature.Public_key_hash.equal delegate (Signature.Public_key.hash pk)
        then n :: acc
        else acc in
      t () >>=? fun t ->
      loop acc t (succ n)
  in
  loop [] delegate_list 0

let first_baking_priorities
    ctxt
    ?(max_priority = 32)
    delegate level =
  baking_priorities ctxt level >>=? fun delegate_list ->
  select_delegate delegate delegate_list max_priority

let check_hash hash stamp_threshold =
  let bytes = Block_hash.to_bytes hash in
  let word = MBytes.get_int64 bytes 0 in
  Compare.Uint64.(word <= stamp_threshold)

let check_header_proof_of_work_stamp shell contents stamp_threshold =
  let hash =
    Block_header.hash
      { shell ; protocol_data = { contents ; signature = Signature.zero } } in
  check_hash hash stamp_threshold

let check_proof_of_work_stamp ctxt block =
  let proof_of_work_threshold = Constants.proof_of_work_threshold ctxt in
  if check_header_proof_of_work_stamp
      block.Block_header.shell
      block.protocol_data.contents
      proof_of_work_threshold then
    return_unit
  else
    fail Invalid_stamp

let check_signature block chain_id key =
  let check_signature key
      { Block_header.shell ; protocol_data = { contents ; signature } } =
    let unsigned_header =
      Data_encoding.Binary.to_bytes_exn
        Block_header.unsigned_encoding
        (shell, contents) in
    Signature.check ~watermark:(Block_header chain_id) key signature unsigned_header in
  if check_signature key block then
    return_unit
  else
    fail (Invalid_block_signature (Block_header.hash block,
                                   Signature.Public_key.hash key))

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
    return_unit

let last_of_a_cycle ctxt l =
  Compare.Int32.(Int32.succ l.Level.cycle_position =
                 Constants.blocks_per_cycle ctxt)

let dawn_of_a_new_cycle ctxt =
  let level = Level.current ctxt in
  if last_of_a_cycle ctxt level then
    return_some level.cycle
  else
    return_none
