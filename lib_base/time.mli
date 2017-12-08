(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type t

val min_value : t
val epoch : t
val max_value : t

val add : t -> int64 -> t
val diff : t -> t -> int64

val equal : t -> t -> bool
val compare : t -> t -> int

val (=) : t -> t -> bool
val (<>) : t -> t -> bool
val (<) : t -> t -> bool
val (<=) : t -> t -> bool
val (>=) : t -> t -> bool
val (>) : t -> t -> bool
val min : t -> t -> t
val max : t -> t -> t

val of_seconds : int64 -> t
val to_seconds : t -> int64

val of_notation : string -> t option
val of_notation_exn : string -> t
val to_notation : t -> string

val now : unit -> t

val encoding : t Data_encoding.t
val rfc_encoding : t Data_encoding.t

val rpc_arg : t RPC_arg.t

val pp_hum : Format.formatter -> t -> unit

type 'a timed_data = {
  data: 'a ;
  time: t ;
}

val make_timed : 'a -> 'a timed_data

val timed_encoding : 'a Data_encoding.t -> 'a timed_data Data_encoding.t

module Set : Set.S with type elt = t
module Map : Map.S with type key = t
module Table : Hashtbl.S with type key = t
