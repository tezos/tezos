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

module type NAME = sig
  val name : string list
end

module type VALUE = sig
  type t
  val of_bytes: MBytes.t -> t tzresult
  val to_bytes: t -> MBytes.t
end

module type ENCODED_VALUE = sig
  type t
  val encoding: t Data_encoding.t
end

module type INDEX = sig
  type t
  val path_length: int
  val to_path: t -> string list -> string list
  val of_path: string list -> t option
end

module type SINGLE_STORE = sig
  type t
  type value
  val known: t -> bool Lwt.t
  val read: t -> value tzresult Lwt.t
  val read_opt: t -> value option Lwt.t
  val store: t -> value -> unit Lwt.t
  val remove: t -> unit Lwt.t
end

module type STORE = sig

  type t
  type key = string list
  type value = MBytes.t

  val known: t -> key -> bool Lwt.t
  val read: t -> key -> value tzresult Lwt.t
  val read_opt: t -> key -> value option Lwt.t
  val store: t -> key -> value -> unit Lwt.t
  val remove: t -> key -> unit Lwt.t

  val known_dir: t -> key -> bool Lwt.t
  val remove_dir: t -> key -> unit Lwt.t

  val fold:
    t -> key -> init:'a ->
    f:([ `Key of key | `Dir of key ] -> 'a -> 'a Lwt.t) ->
    'a Lwt.t

  val keys: t -> key -> key list Lwt.t
  val fold_keys: t -> key -> init:'a -> f:(key -> 'a -> 'a Lwt.t) -> 'a Lwt.t

end

module type SET_STORE = sig
  type t
  type elt
  val known: t -> elt -> bool Lwt.t
  val store: t -> elt -> unit Lwt.t
  val remove: t -> elt -> unit Lwt.t
  val elements: t -> elt list Lwt.t
  val remove_all: t -> unit Lwt.t
  val iter: t -> f:(elt -> unit Lwt.t) -> unit Lwt.t
  val fold: t -> init:'a -> f:(elt -> 'a -> 'a Lwt.t) -> 'a Lwt.t
end

module type BUFFERED_SET_STORE = sig
  include SET_STORE
  module Set : Set.S with type elt = elt
  val read_all: t -> Set.t Lwt.t
  val store_all: t -> Set.t -> unit Lwt.t
end

module type MAP_STORE = sig
  type t
  type key
  type value
  val known: t -> key -> bool Lwt.t
  val read: t -> key -> value tzresult Lwt.t
  val read_opt: t -> key -> value option Lwt.t
  val store: t -> key -> value -> unit Lwt.t
  val remove: t -> key -> unit Lwt.t
  val keys: t -> key list Lwt.t
  val bindings: t -> (key * value) list Lwt.t
  val remove_all: t -> unit Lwt.t
  val iter: t -> f:(key -> value -> unit Lwt.t) -> unit Lwt.t
  val iter_keys: t -> f:(key -> unit Lwt.t) -> unit Lwt.t
  val fold: t -> init:'a -> f:(key -> value -> 'a -> 'a Lwt.t) -> 'a Lwt.t
  val fold_keys: t -> init:'a -> f:(key -> 'a -> 'a Lwt.t) -> 'a Lwt.t
end

module type BUFFERED_MAP_STORE = sig
  include MAP_STORE
  module Map : Map.S with type key = key
  val read_all: t -> value Map.t Lwt.t
  val store_all: t -> value Map.t -> unit Lwt.t
end

module type INDEXED_STORE = sig

  type t
  type key

  module Store : STORE with type t = t * key

  val remove_all: t -> key -> unit Lwt.t

  val fold_indexes: t -> init:'a -> f:(key -> 'a -> 'a Lwt.t) -> 'a Lwt.t
  val indexes: t -> key list Lwt.t

  val resolve_index: t -> string list -> key list Lwt.t

  module Make_set (N : NAME)
    : SET_STORE with type t = t
                 and type elt = key

  module Make_buffered_set (N : NAME) (Set : Set.S with type elt = key)
    : BUFFERED_SET_STORE with type t = t
                          and type elt = key
                          and module Set = Set

  module Make_map (N : NAME) (V : VALUE)
    : MAP_STORE with type t = t
                 and type key = key
                 and type value = V.t

  module Make_buffered_map
      (N : NAME) (V : VALUE) (Map : Map.S with type key = key)
    : BUFFERED_MAP_STORE with type t = t
                          and type key = key
                          and type value = V.t
                          and module Map = Map

end
