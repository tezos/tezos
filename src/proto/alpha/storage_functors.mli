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


type context = {
  context: Context.t ;
  constants: Constants_repr.constants ;
  first_level: Raw_level_repr.t ;
  level: Level_repr.t ;
  timestamp: Time.t ;
  fitness: Int64.t ;
}

open Storage_sigs

(** {1 Errors} ****************************************************************)

(** An internal storage error that should not happen *)
type error += Storage_error of string

(** {1 Data Accessor Parameters} *********************************************)

(** Description of a single data typed accessor. *)
module type Data_description = sig
  (** The OCaml type of value contents *)
  type value

  (** A name (only used for error messages) *)
  val name : string

  (** The serialization format *)
  val encoding : value Data_encoding.t
end

module type Single_data_description = sig

  (** The concrete key in the hierarchical database *)
  val key : string list

  include Data_description

end

(** Describes how to map abstract OCaml types for some (key x value)
    pair to the concrete path in the hierarchical database structure
    and the serialization format. *)
module type Indexed_data_description = sig

  (** The OCaml type for keys *)
  type key

  (** How to produce a concrete key from an abstract one  *)
  val key : key -> string list

  include Data_description

end

(** {1 Data Accessor Builders} ***********************************************)

(** Single data typed accessor builder *)
module Make_single_data_storage (P : Single_data_description) :
  Single_data_storage with type value = P.value
                       and type context := context

module Make_single_optional_data_storage (P : Single_data_description) :
  Single_optional_data_storage with type value = P.value
                                and type context := context

(** Indexed data accessor builder *)
module Make_indexed_data_storage (P : Indexed_data_description) :
  Indexed_data_storage with type key = P. key
                        and type value = P.value
                        and type context := context

module Make_indexed_optional_data_storage (P : Indexed_data_description) :
  Indexed_optional_data_storage with type key = P. key
                                 and type value = P.value
                                 and type context := context

(** Data set builder (set of homogeneous data under a key prefix) *)
module Make_data_set_storage (P : Single_data_description) :
  Data_set_storage with type value = P.value
                    and type context := context

module Make_iterable_data_storage (H : HASH) (P: Single_data_description) :
  Iterable_data_storage with type key = H.t
                         and type value = P.value
                         and type context := context

module Raw_make_iterable_data_storage (K: Persist.KEY) (P: Data_description) :
  Iterable_data_storage with type key = K.t
                         and type value = P.value
                         and type context := context

val register_resolvers: (module Hash.HASH) -> string list list -> unit

