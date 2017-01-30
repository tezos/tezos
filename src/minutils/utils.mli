(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

module StringMap : Map.S with type key = string

(** Splits a string on slashes, grouping multiple slashes, and
    ignoring slashes at the beginning and end of string. *)
val split_path: string -> string list

(** Splits a string on a delimier character, grouping multiple
    delimiters, and ignoring delimiters at the beginning and end of
    string, if [limit] is passed, stops after [limit] split(s). *)
val split: char -> ?dup:bool -> ?limit: int -> string -> string list

val map_option: f:('a -> 'b) -> 'a option -> 'b option
val apply_option: f:('a -> 'b option) -> 'a option -> 'b option
val iter_option: f:('a -> unit) -> 'a option -> unit
val unopt: default:'a -> 'a option -> 'a
val unopt_map: f:('a -> 'b) -> default:'b -> 'a option -> 'b
val unopt_list: 'a option list -> 'a list
val first_some: 'a option -> 'a option -> 'a option

val display_paragraph: Format.formatter -> string -> unit

(** [remove nb list] remove the first [nb] elements from the list [list]. *)
val remove_elem_from_list: int -> 'a list -> 'a list

val remove_prefix: prefix:string -> string -> string option

val filter_map: ('a -> 'b option) -> 'a list -> 'b list

(** [list_rev_sub l n] is (List.rev l) capped to max n elements *)
val list_rev_sub : 'a list -> int -> 'a list
(** [list_sub l n] is l capped to max n elements *)
val list_sub: 'a list -> int -> 'a list
val list_hd_opt: 'a list -> 'a option

val finalize: (unit -> 'a) -> (unit -> unit) -> 'a

val read_file: ?bin:bool -> string -> string
val write_file: ?bin:bool -> string -> string -> unit

(** Compose functions from right to left. *)
val (<<) : ('b -> 'c) -> ('a -> 'b) -> 'a -> 'c

(** Sequence: [i--j] is the sequence [i;i+1;...;j-1;j] *)
val (--) : int -> int -> int list

val repeat: int -> 'a -> 'a list

(** [take_n n l] returns the [n] first elements of [n]. When [compare]
    is provided, it returns the [n] greatest element of [l]. *)
val take_n: ?compare:('a -> 'a -> int) -> int -> 'a list -> 'a list

(** Bounded sequence: keep only the [n] greatest elements. *)
module Bounded(E: Set.OrderedType) : sig
  type t
  val create: int -> t
  val insert: E.t -> t -> unit
  val get: t -> E.t list
end

val select: int -> 'a list -> 'a * 'a list

(** [split_url_port uri] is (node, service) where [node] is the DNS or
    IP and service is the optional port number or service name. *)
val parse_addr_port: string -> string * string
