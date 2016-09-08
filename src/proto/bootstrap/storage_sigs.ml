(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(** {1 Entity Accessor Signatures} ****************************************)

module type Single_optional_data_storage = sig
  type context
  type value
  val get : context -> value option tzresult Lwt.t
  val mem : context -> bool Lwt.t
  val set : context -> value option -> context tzresult Lwt.t
end

(** The generic signature of a single data accessor (a single value
    bound to a specific key in the hierarchical (key x value)
    database). *)
module type Single_data_storage = sig

  type context

  (** The type of the value *)
  type value

  (** Retrieve the value from the storage bucket ; returns a
      {!Storage_error} if the key is not set or if the deserialisation
      fails *)
  val get : context -> value tzresult Lwt.t

  (** Retrieves the value from the storage bucket ; returns [None] if
      the data is not initialized, or {!Storage_helpers.Storage_error}
      if the deserialisation fails *)
  val get_option : context -> value option tzresult Lwt.t

  (** Tells if the data is already defined *)
  val mem : context -> bool Lwt.t

  (** Updates the content of the bucket ; returns a {!Storage_Error}
      if the value does not exists *)
  val set : context -> value -> context tzresult Lwt.t

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

module type Indexed_optional_data_storage = sig
  type context
  type key
  type value
  val get : context -> key -> value option tzresult Lwt.t
  val mem : context -> key -> bool Lwt.t
  val set : context -> key -> value option -> context tzresult Lwt.t
end

(** The generic signature of indexed data accessors (a set of values
    of the same type indexed by keys of the same form in the
    hierarchical (key x value) database). *)
module type Indexed_data_storage = sig

  type context

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

  (** Retrieve a value from the storage bucket at a given key ;
      returns [None] if the value is not set an error if the
      deserialisation fails *)
  val get_option : context -> key -> value option tzresult Lwt.t

  (** Updates the content of a bucket ; returns A {!Storage_Error} if
      the value does not exists *)
  val set : context -> key -> value -> context tzresult Lwt.t

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

module type Iterable_data_storage = sig
  include Indexed_data_storage
  val iter : context -> f:(key -> value -> unit Lwt.t) -> unit Lwt.t
  val fold : context -> 'a -> f:(key -> value -> 'a -> 'a Lwt.t) -> 'a Lwt.t
  val clear : context -> context Lwt.t
end

(** The generic signature of a data set accessor (a set of values
    bound to a specific key prefix in the hierarchical (key x value)
    database). *)
module type Data_set_storage = sig

  type context

  (** The type of values *)
  type value

  (** Tells if a value is a member of the set *)
  val mem : context -> value -> bool tzresult Lwt.t

  (** Adds a value is a member of the set *)
  val add : context -> value -> context tzresult Lwt.t

  (** Removes a value of the set ; does nothing if not a member *)
  val del : context -> value -> context tzresult Lwt.t

  (** Returns the elements of the set, deserialized in a list in no
      particular order ; returns a {!Storage_helpers.Storage_error} if
      a deserialization error occurs *)
  val elements : context -> value list tzresult Lwt.t

  val fold :
    context -> 'a -> f:(value -> 'a -> 'a Lwt.t) -> 'a tzresult Lwt.t

  (** Removes all elements in the set *)
  val clear : context -> context tzresult Lwt.t
end
