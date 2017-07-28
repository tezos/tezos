(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Store_sigs
open Tezos_data

type t
type global_store = t

(** Open or initialize a store at a given path. *)
val init: string -> t tzresult Lwt.t
val close : t -> unit


(** {2 Net store} ************************************************************)

module Net : sig

  val list: global_store -> Net_id.t list Lwt.t
  val destroy: global_store -> Net_id.t -> unit Lwt.t

  type store
  val get: global_store -> Net_id.t -> store

  module Genesis_hash : SINGLE_STORE
    with type t := store
     and type value := Block_hash.t

  module Genesis_time : SINGLE_STORE
    with type t := store
     and type value := Time.t

  module Genesis_protocol : SINGLE_STORE
    with type t := store
     and type value := Protocol_hash.t

  module Genesis_test_protocol : SINGLE_STORE
    with type t := store
     and type value := Protocol_hash.t

  module Expiration : SINGLE_STORE
    with type t := store
     and type value := Time.t

  module Allow_forked_network : SET_STORE
    with type t := t
     and type elt := Net_id.t

end


(** {2 Chain data} ***********************************************************)

module Chain : sig

  type store
  val get: Net.store -> store

  module Current_head : SINGLE_STORE
    with type t := store
     and type value := Block_hash.t

  module Known_heads : BUFFERED_SET_STORE
    with type t := store
     and type elt := Block_hash.t
     and module Set := Block_hash.Set

  module In_chain : SINGLE_STORE
    with type t = store * Block_hash.t
     and type value := Block_hash.t (* successor *)

end


(** {2 Block header store} **************************************************)

module Block : sig

  type store
  val get: Net.store -> store

  type contents = {
    header: Block_header.t ;
    message: string ;
    operation_list_count: int ;
    max_operations_ttl: int ;
  }

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

  type invalid_block = {
    level: int32 ;
    (* errors: Error_monad.error list ; *)
  }

  module Invalid_block : MAP_STORE
    with type t = store
     and type key = Block_hash.t
     and type value = invalid_block

end


(** {2 Protocol store} ******************************************************)

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
