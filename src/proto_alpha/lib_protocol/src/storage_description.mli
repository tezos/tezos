(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(** Typed description of the key-value context. *)
type 'key t

(** Trivial display of the key-value context layout. *)
val pp: Format.formatter -> 'key t -> unit

(** Export an RPC hierarchy for querying the context. There is one service
    by possible path in the context. Services for "directory" are able to
    aggregate in one JSON object the whole subtree. *)
val build_directory: 'key t -> 'key RPC_directory.t

(** Create a empty context description,
    keys will be registred by side effects. *)
val create: unit -> 'key t

(** Register a single key accessor at a given path. *)
val register_value:
  'key t ->
  get:('key -> 'a option tzresult Lwt.t) ->
  'a Data_encoding.t -> unit

(** Return a description for a prefixed fragment of the given context.
    All keys registred in the subcontext will be shared by the external
    context *)
val register_named_subcontext: 'key t -> string list -> 'key t

(** Description of an index as a sequence of `RPC_arg.t`. *)
type (_, _, _) args =
  | One : { rpc_arg: 'a RPC_arg.t ;
            encoding: 'a Data_encoding.t ;
            compare: 'a -> 'a -> int } -> ('key, 'a, 'key * 'a) args
  | Pair : ('key, 'a, 'inter_key) args *
           ('inter_key, 'b, 'sub_key) args -> ('key, 'a * 'b, 'sub_key) args

(** Return a description for a indexed sub-context.
    All keys registred in the subcontext will be shared by the external
    context. One should provide a function to list all the registred
    index in the context. *)
val register_indexed_subcontext:
  'key t ->
  list:('key -> 'arg list tzresult Lwt.t) ->
  ('key, 'arg, 'sub_key) args -> 'sub_key t

(** Helpers for manipulating and defining indexes. *)

val pack: ('key, 'a, 'sub_key) args -> 'key -> 'a -> 'sub_key
val unpack: ('key, 'a, 'sub_key) args -> 'sub_key -> 'key * 'a

module type INDEX = sig
  type t
  val path_length: int
  val to_path: t -> string list -> string list
  val of_path: string list -> t option
  val rpc_arg: t RPC_arg.t
  val encoding: t Data_encoding.t
  val compare: t -> t -> int
end
