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
