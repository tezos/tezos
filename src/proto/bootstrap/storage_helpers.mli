(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(** Tezos Protocol Implementation - Typed storage accessor builders

    This module hides the hierarchical (key x value) database under
    three kinds of typed data accessors (single typed data, homgeneous
    indexed data and homgeneous data set). *)

type context = Context.t * Constants.constants


(** {1 Errors} ****************************************************************)

(** An internal storage error that should not happen *)
type error += Storage_error of string

(** {1 Data Accessor Signatures} *********************************************)

(** The generic signature of a single data accessor (a single value
    bound to a specific key in the hierarchical (key x value)
    database). *)
module type Minimal_single_data_storage = sig

  (** The type of the value *)
  type value

  (** Retrieve the value from the storage bucket ; returns a
      {!Storage_error} if the key is not set or if the deserialisation
      fails *)
  val get : context -> value tzresult Lwt.t

  (** Tells if the data is already defined *)
  val mem : context -> bool Lwt.t

  (** Updates the content of the bucket ; returns a {!Storage_Error}
      if the value does not exists *)
  val set : context -> value -> context tzresult Lwt.t

end

module type Single_data_storage = sig

  include Minimal_single_data_storage

  (** Retrieves the value from the storage bucket ; returns [None] if
      the data is not initialized, or {!Storage_helpers.Storage_error}
      if the deserialisation fails *)
  val get_option : context -> value option tzresult Lwt.t


  (** Allocates the storage bucket and initializes it ; returns a
      {!Storage_error} if the bucket exists *)
  val init : context -> value -> context tzresult Lwt.t

  (** Delete the storage bucket ; returns a {!Storage_error} if the
      bucket does not exists *)
  val delete : context -> context tzresult Lwt.t

  (** Allocates the data and initializes it with a value ; just
      updates it if the bucket exists *)
  val init_set : context -> value -> context tzresult Lwt.t

  (** Removes the storage bucket and its contents ; does nothing if the
      bucket does not exists *)
  val remove : context -> context Lwt.t

end

module type Minimal_indexed_data_storage = sig

  (** An abstract type for keys *)
  type key

  (** The type of values *)
  type value

  (** Retrieve a value from the storage bucket at a given key ;
      returns a {!Storage_error} if the key is not set or if the
      deserialisation fails *)
  val get : context -> key -> value tzresult Lwt.t

  (** Tells if a given key is already bound to a storage bucket *)
  val mem : context -> key -> bool Lwt.t

  (** Updates the content of a bucket ; returns A {!Storage_Error} if
      the value does not exists *)
  val set : context -> key -> value -> context tzresult Lwt.t

end

(** The generic signature of indexed data accessors (a set of values
    of the same type indexed by keys of the same form in the
    hierarchical (key x value) database). *)
module type Indexed_data_storage = sig

  include Minimal_indexed_data_storage

  (** Retrieve a value from the storage bucket at a given key ;
      returns [None] if the value is not set an error if the
      deserialisation fails *)
  val get_option : context -> key -> value option tzresult Lwt.t

  (** Allocates a storage bucket at the given key and initializes it ;
      returns a {!Storage_error} if the bucket exists *)
  val init : context -> key -> value -> context tzresult Lwt.t

  (** Delete a storage bucket and its contents ; returns a
      {!Storage_error} if the bucket does not exists *)
  val delete : context -> key -> context tzresult Lwt.t

  (** Allocates a storage bucket at the given key and initializes it
      with a value ; just updates it if the bucket exists *)
  val init_set : context -> key -> value -> context tzresult Lwt.t

  (** Removes a storage bucket and its contents ; does nothing if the
      bucket does not exists *)
  val remove : context -> key -> context Lwt.t

end

(** An extension of [Indexed_data_storage] that allows iterations
    over the element of the database *)
module type Iterable_data_storage = sig

  include Indexed_data_storage

  (** Iter over all elements in the storage *)
  val iter : context -> f:(key -> value -> unit Lwt.t) -> unit Lwt.t
  val fold : context -> 'a -> f:(key -> value -> 'a -> 'a Lwt.t) -> 'a Lwt.t

  (** Removes all elements in the storage *)
  val clear : context -> context Lwt.t

end

(** The generic signature of a data set accessor (a set of values
    bound to a specific key prefix in the hierarchical (key x value)
    database). Values are equal if their serializations are. *)
module type Data_set_storage = sig

  (** The type of values *)
  type value

  (** Tells if a value is a member of the set *)
  val mem : context -> value -> bool tzresult Lwt.t

  (** Adds a value is a member of the set *)
  val add : context -> value -> context tzresult Lwt.t

  (** Removes a value of the set ; does nothing if not a member *)
  val del : context -> value -> context tzresult Lwt.t

  (** Returns the elements of the set, deserialized in a list in no
      particular order ; returns a {!Storage_error} if a
      deserialization error occurs *)
  val elements : context -> value list tzresult Lwt.t

  (** Removes all elements in the set *)
  val clear : context -> context tzresult Lwt.t
end

(** {1 Data Accessor Parameters} *********************************************)

(** Description of a single data typed accessor. *)
module type Single_data_description = sig
  (** The OCaml type of value contents *)
  type value

  (** The concrete key in the hierarchical database *)
  val key : string list

  (** A name (only used for error messages) *)
  val name : string

  (** The serialization format *)
  val encoding : value Data_encoding.t
end

(** Describes how to map abstract OCaml types for some (key x value)
    pair to the concrete path in the hierarchical database structure
    and the serialization format. *)
module type Indexed_data_description = sig

  (** The OCaml type for keys *)
  type key

  (** The OCaml type of value contents *)
  type value

  (** A name (only used for error messages) *)
  val name : string

  (** How to produce a concrete key from an abstract one  *)
  val key : key -> string list

  (** The serialization format *)
  val encoding : value Data_encoding.t
end

(** {1 Data Accessor Builders} ***********************************************)

(** Single data typed accessor builder *)
module Make_single_data_storage (P : Single_data_description) :
  Single_data_storage with type value = P.value

module Make_single_option_data_storage (P : Single_data_description) :
  Minimal_single_data_storage with type value = P.value option

(** Indexed data accessor builder *)
module Make_indexed_data_storage (P : Indexed_data_description) :
  Indexed_data_storage with type key = P. key and type value = P.value

module Make_indexed_option_data_storage (P : Indexed_data_description) :
  Minimal_indexed_data_storage with type key = P. key
                                and type value = P.value option

(** Data set builder (set of homogeneous data under a key prefix) *)
module Make_data_set_storage (P : Single_data_description) :
  Data_set_storage with type value = P.value

module Make_iterable_data_storage (H : HASH) (P: Single_data_description) :
  Iterable_data_storage with type key = H.t and type value = P.value

module Make_hash_resolver
    (K: sig val prefix: string list end)
    (H: Hash.HASH) : sig
  val register : Store.t -> unit
end

