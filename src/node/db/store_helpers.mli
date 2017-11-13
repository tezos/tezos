(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Store_sigs

module Make_value (V : ENCODED_VALUE) : VALUE with type t = V.t

module Raw_value : VALUE with type t = MBytes.t

module Make_single_store (S : STORE) (N : NAME) (V : VALUE)
  : SINGLE_STORE with type t = S.t
                  and type value = V.t

module Make_substore (S : STORE) (N : NAME)
  : STORE with type t = S.t

module Make_set (S : STORE) (I : INDEX)
  : SET_STORE with type t = S.t and type elt = I.t

module Make_buffered_set
    (S : STORE) (I : INDEX) (Set : Set.S with type elt = I.t)
  : BUFFERED_SET_STORE with type t = S.t
                        and type elt = I.t
                        and module Set = Set

module Make_map
    (S : STORE) (I : INDEX) (V : VALUE)
  : MAP_STORE with type t = S.t
               and type key = I.t
               and type value = V.t

module Make_buffered_map
    (S : STORE) (I : INDEX) (V : VALUE) (Map : Map.S with type key = I.t)
  : BUFFERED_MAP_STORE with type t = S.t
                        and type key = I.t
                        and type value = V.t
                        and module Map = Map

module Make_indexed_substore (S : STORE) (I : INDEX)
  : INDEXED_STORE with type t = S.t
                   and type key = I.t

module Integer_index : INDEX with type t = int
