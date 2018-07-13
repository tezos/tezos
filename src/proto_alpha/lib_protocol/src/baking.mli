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
type error += Invalid_block_signature of Block_hash.t * Signature.Public_key_hash.t (* `Permanent *)
type error += Unexpected_endorsement
type error += Invalid_signature  (* `Permanent *)
type error += Invalid_stamp  (* `Permanent *)

(** [minimal_time ctxt priority pred_block_time] returns the minimal
    time, given the predecessor block timestamp [pred_block_time],
    after which a baker with priority [priority] is allowed to
    bake. Fail with [Invalid_time_between_blocks_constant] if the minimal
    time cannot be computed. *)
val minimal_time: context -> int -> Time.t -> Time.t tzresult Lwt.t

(** [check_baking_rights ctxt block pred_timestamp] verifies that:
    * the contract that owned the roll at cycle start has the block signer as delegate.
    * the timestamp is coherent with the announced slot.
*)
val check_baking_rights:
  context -> Block_header.contents -> Time.t ->
  public_key tzresult Lwt.t

(** For a given level computes who has the right to
    include an endorsement in the next block.
    The result can be stored in Alpha_context.allowed_endorsements *)
val endorsement_rights:
  context ->
  Level.t ->
  (public_key * int list * bool) Signature.Public_key_hash.Map.t tzresult Lwt.t

(** Check that the operation was signed by a delegate allowed
    to endorse at the level specified by the endorsement. *)
val check_endorsement_rights:
  context -> Chain_id.t -> Kind.endorsement Operation.t ->
  (public_key_hash * int list * bool) tzresult Lwt.t

(** Returns the endorsement reward calculated w.r.t a given priority.  *)
val endorsement_reward: context -> block_priority:int -> int -> Tez.t tzresult Lwt.t

(** [baking_priorities ctxt level] is the lazy list of contract's
    public key hashes that are allowed to bake for [level]. *)
val baking_priorities:
  context -> Level.t -> public_key lazy_list

(** [first_baking_priorities ctxt ?max_priority contract_hash level]
    is a list of priorities of max [?max_priority] elements, where the
    delegate of [contract_hash] is allowed to bake for [level]. If
    [?max_priority] is [None], a sensible number of priorities is
    returned. *)
val first_baking_priorities:
  context ->
  ?max_priority:int ->
  public_key_hash ->
  Level.t ->
  int list tzresult Lwt.t

(** [check_signature ctxt chain_id block id] check if the block is
    signed with the given key, and belongs to the given [chain_id] *)
val check_signature: Block_header.t -> Chain_id.t -> public_key -> unit tzresult Lwt.t

(** Checks if the header that would be built from the given components
    is valid for the given diffculty. The signature is not passed as it
    is does not impact the proof-of-work stamp. The stamp is checked on
    the hash of a block header whose signature has been zeroed-out. *)
val check_header_proof_of_work_stamp:
  Block_header.shell_header -> Block_header.contents -> int64 -> bool

(** verify if the proof of work stamp is valid *)
val check_proof_of_work_stamp:
  context -> Block_header.t -> unit tzresult Lwt.t

(** check if the gap between the fitness of the current context
    and the given block is within the protocol parameters *)
val check_fitness_gap:
  context -> Block_header.t -> unit tzresult Lwt.t

val dawn_of_a_new_cycle: context -> Cycle.t option tzresult Lwt.t

val earlier_predecessor_timestamp: context -> Level.t -> Timestamp.t tzresult Lwt.t
