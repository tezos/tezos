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

open Store_sigs

type t
type global_store = t

(** [init ~mapsize path] returns an initialized store at [path] of
    maximum capacity [mapsize] bytes. *)
val init: ?mapsize:int64 -> string -> t tzresult Lwt.t
val close : t -> unit


(** {2 Chain store} *)

module Chain : sig

  val list: global_store -> Chain_id.t list Lwt.t
  val destroy: global_store -> Chain_id.t -> unit Lwt.t

  type store
  val get: global_store -> Chain_id.t -> store

  module Genesis_hash : SINGLE_STORE
    with type t := store
     and type value := Block_hash.t

  module Genesis_time : SINGLE_STORE
    with type t := store
     and type value := Time.Protocol.t

  module Genesis_protocol : SINGLE_STORE
    with type t := store
     and type value := Protocol_hash.t

  module Genesis_test_protocol : SINGLE_STORE
    with type t := store
     and type value := Protocol_hash.t

  module Expiration : SINGLE_STORE
    with type t := store
     and type value := Time.Protocol.t

  module Allow_forked_chain : SET_STORE
    with type t := t
     and type elt := Chain_id.t

end


(** {2 Mutable chain data} *)

module Chain_data : sig

  type store
  val get: Chain.store -> store

  module Current_head : SINGLE_STORE
    with type t := store
     and type value := Block_hash.t

  module Known_heads : BUFFERED_SET_STORE
    with type t := store
     and type elt := Block_hash.t
     and module Set := Block_hash.Set

  module In_main_branch : SINGLE_STORE
    with type t = store * Block_hash.t
     and type value := Block_hash.t (* successor *)

  module Checkpoint : SINGLE_STORE
    with type t := store
     and type value := Int32.t * Block_hash.t

end


(** {2 Block header store} *)

module Block : sig

  type store
  val get: Chain.store -> store

  type contents = {
    message: string option ;
    max_operations_ttl: int ;
    last_allowed_fork_level: Int32.t ;
    context: Context_hash.t ;
    metadata: MBytes.t ;
  }

  module Header : SINGLE_STORE
    with type t = store * Block_hash.t
     and type value := Block_header.t

  module Contents : SINGLE_STORE
    with type t = store * Block_hash.t
     and type value := contents

  module Operation_hashes : MAP_STORE
    with type t = store * Block_hash.t
     and type key = int
     and type value = Operation_hash.t list

  module Operation_path : MAP_STORE
    with type t = store * Block_hash.t
     and type key = int
     and type value = Operation_list_list_hash.path

  module Operations : MAP_STORE
    with type t = store * Block_hash.t
     and type key = int
     and type value = Operation.t list

  module Operations_metadata : MAP_STORE
    with type t = store * Block_hash.t
     and type key = int
     and type value = MBytes.t list

  type invalid_block = {
    level: int32 ;
    errors: Error_monad.error list ;
  }

  module Invalid_block : MAP_STORE
    with type t = store
     and type key = Block_hash.t
     and type value = invalid_block

  (**
     Block predecessors under
     [/blocks/<block_id>/predecessors/<distance>/<block_id>].
     Used to compute block predecessors in [lib_node_shell/state.ml].
  *)
  module Predecessors : MAP_STORE
    with type t = store * Block_hash.t
     and type key = int
     and type value = Block_hash.t

end


(** {2 Protocol store} *)

module Protocol : sig

  type store
  val get: global_store -> store

  module Contents : MAP_STORE
    with type t := store
     and type key := Protocol_hash.t
     and type value := Protocol.t

  module RawContents : SINGLE_STORE
    with type t = store * Protocol_hash.t
     and type value := MBytes.t

end

(** {2 Temporary test chain forking block store} *)

module Forking_block_hash : MAP_STORE
  with type t = global_store
   and type key := Chain_id.t
   and type value := Block_hash.t
