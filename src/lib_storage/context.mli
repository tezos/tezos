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

(** Tezos - Versioned, block indexed (key x value) store *)

(** A block-indexed (key x value) store directory.  *)
type index

(** A (key x value) store for a given block. *)
type t
type context = t

(** Open or initialize a versioned store at a given path. *)
val init:
  ?patch_context:(context -> context Lwt.t) ->
  ?mapsize:int64 ->
  ?readonly:bool ->
  string ->
  index Lwt.t

val compute_testchain_chain_id:
  Block_hash.t -> Chain_id.t

val compute_testchain_genesis:
  Block_hash.t -> Block_hash.t

val commit_genesis:
  index ->
  chain_id:Chain_id.t ->
  time:Time.t ->
  protocol:Protocol_hash.t ->
  Context_hash.t Lwt.t

val commit_test_chain_genesis:
  context ->
  Block_header.t ->
  Block_header.t Lwt.t


(** {2 Generic interface} ****************************************************)

type key = string list
(** [key] indicates a path in a context. *)

type value = MBytes.t

val mem: context -> key -> bool Lwt.t
val dir_mem: context -> key -> bool Lwt.t
val get: context -> key -> value option Lwt.t
val set: context -> key -> value -> t Lwt.t
val del: context -> key -> t Lwt.t
val remove_rec: context -> key -> t Lwt.t

(** [copy] returns None if the [from] key is not bound *)
val copy: context -> from:key -> to_:key -> context option Lwt.t

(** [fold] iterates over elements under a path (not recursive). Iteration order
    is undeterministic. *)
val fold:
  context -> key -> init:'a ->
  f:([ `Key of key | `Dir of key ] -> 'a -> 'a Lwt.t) ->
  'a Lwt.t

(** {2 Accessing and Updating Versions} **************************************)

val exists: index -> Context_hash.t -> bool Lwt.t
val checkout: index -> Context_hash.t -> context option Lwt.t
val checkout_exn: index -> Context_hash.t -> context Lwt.t
val hash:   time:Time.t ->
  ?message:string -> t -> Context_hash.t Lwt.t
val commit:
  time:Time.t ->
  ?message:string ->
  context ->
  Context_hash.t Lwt.t
val set_head: index -> Chain_id.t -> Context_hash.t -> unit Lwt.t
val set_master: index -> Context_hash.t -> unit Lwt.t


(** {2 Predefined Fields} ****************************************************)

val get_protocol: context -> Protocol_hash.t Lwt.t
val set_protocol: context -> Protocol_hash.t -> context Lwt.t

val get_test_chain: context -> Test_chain_status.t Lwt.t
val set_test_chain: context -> Test_chain_status.t -> context Lwt.t

val del_test_chain: context -> context Lwt.t

val fork_test_chain:
  context -> protocol:Protocol_hash.t -> expiration:Time.t -> context Lwt.t
val clear_test_chain: index -> Chain_id.t -> unit Lwt.t

(** {2 Context dumping} ******************************************************)

module Pruned_block : sig

  type t = {
    block_header : Block_header.t ;
    operations : ( int * Operation.t list ) list ;
    operation_hashes : (int * Operation_hash.t list) list ;
  }

  val encoding : t Data_encoding.t

  val to_bytes : t -> MBytes.t
  val of_bytes : MBytes.t -> t option
end

module Block_data : sig

  type t = {
    block_header : Block_header.t ;
    operations : Operation.t list list ;
  }

  val to_bytes : t -> MBytes.t
  val of_bytes : MBytes.t -> t option
  val empty : t
  val encoding : t Data_encoding.t
end

module Protocol_data : sig

  type t = Int32.t * data

  and info = {
    author : string ;
    message : string ;
    timestamp : Time.t ;
  }

  and data = {
    info : info ;
    protocol_hash : Protocol_hash.t ;
    test_chain_status : Test_chain_status.t ;
    data_key : Context_hash.t ;
    parents : Context_hash.t list ;
  }

  val to_bytes : t -> MBytes.t
  val of_bytes : MBytes.t -> t option
  val empty : t
  val encoding : t Data_encoding.t

end

val get_protocol_data_from_header :
  index -> Block_header.t -> Protocol_data.t Lwt.t

val dump_contexts :
  index ->
  (Block_header.t * Block_data.t *
   (Block_header.t -> (Pruned_block.t option * Protocol_data.t option) tzresult Lwt.t) *
   Block_header.t) list ->
  filename:string ->
  unit tzresult Lwt.t

val dump_contexts_fd :
  index ->
  (Block_header.t * Block_data.t *
   (Block_header.t -> (Pruned_block.t option * Protocol_data.t option) tzresult Lwt.t) *
   Block_header.t) list ->
  fd:Lwt_unix.file_descr ->
  unit tzresult Lwt.t
