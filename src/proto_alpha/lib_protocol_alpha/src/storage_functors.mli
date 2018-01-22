(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(** Tezos Protocol Implementation - Typed storage builders. *)

open Storage_sigs

module type ENCODED_VALUE = sig
  type t
  val encoding: t Data_encoding.t
end

module Make_value (V : ENCODED_VALUE) : VALUE with type t = V.t

module Raw_value : VALUE with type t = MBytes.t

module Make_subcontext (C : Raw_context.T) (N : NAME)
  : Raw_context.T with type t = C.t

module Make_single_data_storage (C : Raw_context.T) (N : NAME) (V : VALUE)
  : Single_data_storage with type t = C.t
                         and type value = V.t

module type INDEX = sig
  type t
  val path_length: int
  val to_path: t -> string list -> string list
  val of_path: string list -> t option
end

module Pair(I1 : INDEX)(I2 : INDEX) : INDEX with type t = I1.t * I2.t

module Make_data_set_storage (C : Raw_context.T) (I : INDEX)
  : Data_set_storage with type t = C.t and type elt = I.t

module Make_indexed_data_storage (C : Raw_context.T) (I : INDEX) (V : VALUE)
  : Indexed_data_storage with type t = C.t
                          and type key = I.t
                          and type value = V.t

module Make_indexed_subcontext (C : Raw_context.T) (I : INDEX)
  : Indexed_raw_context with type t = C.t
                         and type key = I.t
