(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(**  Tezos - Persistent structures on top of {!Context} *)

open Lwt


(** Keys in (kex x value) database implementations *)
type key = string list

(** Values in (kex x value) database implementations *)
type value = MBytes.t

(** Low level view over a (key x value) database implementation. *)
module type STORE = sig
  type t
  val mem: t -> key -> bool Lwt.t
  val get: t -> key -> value option Lwt.t
  val set: t -> key -> value -> t Lwt.t
  val del: t -> key -> t Lwt.t
  val list: t -> key list -> key list Lwt.t
  val remove_rec: t -> key -> t Lwt.t

  val keys : t -> key list Lwt.t
end

(** Projection of OCaml keys of some abstract type to concrete storage
    keys. For practical reasons, all such keys must fall under a same
    {!prefix} and have the same relative {!length}. Functions
    {!to_path} and {!of_path} only take the relative part into account
    (the prefix is added and removed when needed). *)
module type KEY = sig
  type t
  val prefix: key
  val length: int
  val to_path: t -> key
  val of_path: key -> t
  val compare: t -> t -> int
end

(** A KEY instance for using raw implementation paths as keys *)
module RawKey : KEY with type t = key

module type BYTES_STORE = sig
  type t
  type key
  val mem: t -> key -> bool Lwt.t
  val get: t -> key -> value option Lwt.t
  val set: t -> key -> value -> t Lwt.t
  val del: t -> key -> t Lwt.t
  val list: t -> key list -> key list Lwt.t
  val remove_rec: t -> key -> t Lwt.t

  val keys : t -> key list Lwt.t
end

module MakeBytesStore (S : STORE) (K : KEY) :
  BYTES_STORE with type t = S.t and type key = K.t

(** {2 Typed Store Overlays} *************************************************)

(** Projection of OCaml values of some abstract type to concrete
    storage data. *)
module type VALUE = sig
  type t
  val of_bytes: value -> t option
  val to_bytes: t -> value
end

(** A VALUE instance for using the raw bytes values *)
module RawValue : VALUE with type t = value

(** Signature of a typed store as returned by {!MakeTypedStore} *)
module type TYPED_STORE = sig
  type t
  type key
  type value
  val mem: t -> key -> bool Lwt.t
  val get: t -> key -> value option Lwt.t
  val set: t -> key -> value -> t Lwt.t
  val del: t -> key -> t Lwt.t

  val keys: t -> key list Lwt.t (** Not always relevant, BEWARE! *)
end

(** Gives a typed view of a store (values of a given type stored under
    keys of a given type). The view is also restricted to a prefix,
    (which can be empty). For all primitives to work as expected, all
    keys under this prefix must be homogeneously typed. *)
module MakeTypedStore (S : STORE) (K : KEY) (C : VALUE) :
  TYPED_STORE with type t = S.t and type key = K.t and type value = C.t


(** {2 Persistent Sets} ******************************************************)

(** Signature of a set as returned by {!MakePersistentSet} *)
module type PERSISTENT_SET = sig
  type t and key
  val mem : t -> key -> bool Lwt.t
  val set : t -> key -> t Lwt.t
  val del : t -> key -> t Lwt.t
  val elements : t -> key list Lwt.t
  val clear : t -> t Lwt.t
  val iter : t -> f:(key -> unit Lwt.t) -> unit Lwt.t
  val fold : t -> 'a -> f:(key -> 'a -> 'a Lwt.t) -> 'a Lwt.t
end

(** Signature of a buffered set as returned by {!MakeBufferedPersistentSet} *)
module type BUFFERED_PERSISTENT_SET = sig
  include PERSISTENT_SET
  module Set : Set.S with type elt = key
  val read : t -> Set.t Lwt.t
  val write : t -> Set.t -> t Lwt.t
end

(** Build a set in the (key x value) storage by encoding elements as
    keys and using the association of (any) data to these keys as
    membership. For this to work, the prefix passed must be reserved
    for the set (every key under it is considered a member). *)
module MakePersistentSet (S : STORE) (K : KEY)
  : PERSISTENT_SET with type t := S.t and type key := K.t

(** Same as {!MakePersistentSet} but also provides a way to use an
    OCaml set as an explicitly synchronized in-memory buffer. *)
module MakeBufferedPersistentSet
    (S : STORE) (K : KEY) (Set : Set.S with type elt = K.t)
  : BUFFERED_PERSISTENT_SET
    with type t := S.t
     and type key := K.t
     and module Set := Set

(** {2 Persistent Maps} ******************************************************)

(** Signature of a map as returned by {!MakePersistentMap} *)
module type PERSISTENT_MAP = sig
  type t and key and value
  val mem : t -> key -> bool Lwt.t
  val get : t -> key -> value option Lwt.t
  val set : t -> key -> value -> t Lwt.t
  val del : t -> key -> t Lwt.t
  val bindings : t -> (key * value) list Lwt.t
  val clear : t -> t Lwt.t
  val iter : t -> f:(key -> value -> unit Lwt.t) -> unit Lwt.t
  val fold : t -> 'a -> f:(key -> value -> 'a -> 'a Lwt.t) -> 'a Lwt.t
end

(** Signature of a buffered map as returned by {!MakeBufferedPersistentMap} *)
module type BUFFERED_PERSISTENT_MAP = sig
  include PERSISTENT_MAP
  module Map : Map.S with type key = key
  val read : t -> value Map.t Lwt.t
  val write : t -> value Map.t -> t Lwt.t
end

(** Build a map in the (key x value) storage. For this to work, the
    prefix passed must be reserved for the map (every key under it is
    considered the key of a binding). *)
module MakePersistentMap (S : STORE) (K : KEY) (C : VALUE)
  : PERSISTENT_MAP
    with type t := S.t and type key := K.t and type value := C.t

(** Same as {!MakePersistentMap} but also provides a way to use an
    OCaml map as an explicitly synchronized in-memory buffer. *)
module MakeBufferedPersistentMap
    (S : STORE) (K : KEY) (C : VALUE) (Map : Map.S with type key = K.t)
 : BUFFERED_PERSISTENT_MAP
   with type t := S.t
    and type key := K.t
    and type value := C.t
    and module Map := Map

(** {2 Imperative overlays} **************************************************)

type 'a shared_ref
val share : 'a -> 'a shared_ref
val update : 'a shared_ref -> ('a -> 'a option Lwt.t) -> bool Lwt.t
val update_with_res :
  'a shared_ref -> ('a -> ('a option * 'b) Lwt.t) -> (bool * 'b) Lwt.t
val use : 'a shared_ref -> ('a -> 'b Lwt.t) -> 'b Lwt.t

module type IMPERATIVE_PROXY = sig
  module Store : TYPED_STORE

  type t
  type rdata
  type state
  val create: state -> Store.t shared_ref -> t
  val known: t -> Store.key -> bool Lwt.t
  val read: t -> Store.key -> Store.value option Lwt.t
  val store: t -> Store.key -> Store.value -> bool Lwt.t
  val update: t -> Store.key -> Store.value -> bool Lwt.t
  val remove: t -> Store.key -> bool Lwt.t
  val prefetch: t -> rdata -> Store.key -> unit
  val fetch: t -> rdata -> Store.key -> Store.value Lwt.t
  val pending: t -> Store.key -> bool
  val shutdown: t -> unit Lwt.t

  val keys: t -> Store.key list Lwt.t
end

module type IMPERATIVE_PROXY_SCHEDULER = sig
  module Store : TYPED_STORE
  type state
  type rdata
  type data

  val name : string
  val init_request :
    state -> Store.key -> data Lwt.t
  val request :
    state ->
    get:(rdata -> Store.key -> Store.value Lwt.t) ->
    set:(Store.key -> Store.value -> unit Lwt.t) ->
    (Store.key * data * rdata) list -> float
end

module MakeImperativeProxy
    (Store : TYPED_STORE)
    (Table : Hashtbl.S with type key = Store.key)
    (Scheduler : IMPERATIVE_PROXY_SCHEDULER with module Store := Store)
  : IMPERATIVE_PROXY with module Store := Store and type state = Scheduler.state
                                                and type rdata = Scheduler.rdata

(** {2 Predefined Instances} *************************************************)

module MakePersistentBytesMap (S : STORE) (K : KEY)
  : PERSISTENT_MAP
  with type t := S.t and type key := K.t and type value := MBytes.t

module MakeBufferedPersistentBytesMap
    (S : STORE) (K : KEY) (Map : Map.S with type key = K.t)
  : BUFFERED_PERSISTENT_MAP
    with type t := S.t
     and type key := K.t
     and type value := MBytes.t
     and module Map := Map

module type TYPED_VALUE_REPR = sig
  type value
  val encoding: value Data_encoding.t
end

module MakePersistentTypedMap (S : STORE) (K : KEY) (T : TYPED_VALUE_REPR)
  : PERSISTENT_MAP
    with type t := S.t and type key := K.t and type value := T.value

module MakeBufferedPersistentTypedMap
    (S : STORE) (K : KEY) (T : TYPED_VALUE_REPR) (Map : Map.S with type key = K.t)
  : BUFFERED_PERSISTENT_MAP
    with type t := S.t
     and type key := K.t
     and type value := T.value
     and module Map := Map
