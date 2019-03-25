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

type shell_header = {
  level: Int32.t ;
  (** Height of the block, from the genesis block. *)
  proto_level: int ; (* uint8 *)
  (** Number of protocol changes since genesis modulo 256. *)
  predecessor: Block_hash.t ;
  (** Hash of the preceding block. *)
  timestamp: Time.Protocol.t ;
  (** Timestamp at which the block is claimed to have been created. *)
  validation_passes: int ; (* uint8 *)
  (** Number of validation passes (also number of lists of operations). *)
  operations_hash: Operation_list_list_hash.t ;
  (** Hash of the list of lists (actually root hashes of merkle trees)
      of operations included in the block. There is one list of
      operations per validation pass. *)
  fitness: Fitness.t ;
  (** A sequence of sequences of unsigned bytes, ordered by length and
      then lexicographically. It represents the claimed fitness of the
      chain ending in this block. *)
  context: Context_hash.t ;
  (** Hash of the state of the context after application of this block. *)
}

val shell_header_encoding: shell_header Data_encoding.t

type t = {
  shell: shell_header ;
  protocol_data: MBytes.t ;
}

include S.HASHABLE with type t := t
                    and type hash := Block_hash.t
val of_bytes_exn: MBytes.t -> t

val bounded_encoding: ?max_size:int -> unit -> t Data_encoding.t

val get_forced_protocol_upgrade: level:Int32.t -> Protocol_hash.t option
