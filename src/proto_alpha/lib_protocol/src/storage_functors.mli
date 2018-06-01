(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(** Tezos Protocol Implementation - Typed storage builders. *)

open Storage_sigs

module Make_subcontext (C : Raw_context.T) (N : NAME)
  : Raw_context.T with type t = C.t

module Make_single_data_storage
    (C : Raw_context.T) (N : NAME) (V : VALUE)
  : Single_data_storage with type t = C.t
                         and type value = V.t

module Make_carbonated_value (V : VALUE) : CARBONATED_VALUE with type t = V.t

module type INDEX = sig
  type t
  val path_length: int
  val to_path: t -> string list -> string list
  val of_path: string list -> t option
  type 'a ipath
  val args: ('a, t, 'a ipath) Storage_description.args
end

module Pair(I1 : INDEX)(I2 : INDEX) : INDEX with type t = I1.t * I2.t

module Make_data_set_storage (C : Raw_context.T) (I : INDEX)
  : Data_set_storage with type t = C.t and type elt = I.t

module Make_indexed_data_storage
    (C : Raw_context.T) (I : INDEX) (V : VALUE)
  : Indexed_data_storage with type t = C.t
                          and type key = I.t
                          and type value = V.t

module Make_indexed_carbonated_data_storage
    (C : Raw_context.T) (I : INDEX) (V : CARBONATED_VALUE)
  : Non_iterable_indexed_carbonated_data_storage with type t = C.t
                                                  and type key = I.t
                                                  and type value = V.t

module Make_indexed_data_snapshotable_storage (C : Raw_context.T)
    (Snapshot : INDEX) (I : INDEX) (V : VALUE)
  : Indexed_data_snapshotable_storage with type t = C.t
                                       and type snapshot = Snapshot.t
                                       and type key = I.t
                                       and type value = V.t

module Make_indexed_subcontext (C : Raw_context.T) (I : INDEX)
  : Indexed_raw_context with type t = C.t
                         and type key = I.t
                         and type 'a ipath = 'a I.ipath

module Wrap_indexed_data_storage
    (C : Indexed_data_storage)
    (K : sig
       type t
       val wrap: t -> C.key
       val unwrap: C.key -> t option
     end)
  : Indexed_data_storage with type t = C.t
                          and type key = K.t
                          and type value = C.value
