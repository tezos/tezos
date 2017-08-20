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

(** [Some (f x)] if input is [Some x], or [None] if it's [None] **)
val map_option: f:('a -> 'b) -> 'a option -> 'b option

(** [(f x)] if input is [Some x], or [None] if it's [None] **)
val apply_option: f:('a -> 'b option) -> 'a option -> 'b option

(** Call [(f x)] if input is [Some x], noop if it's [None] **)
val iter_option: f:('a -> unit) -> 'a option -> unit

(** [x] if input is [Some x], default if it's [None] **)
val unopt: default:'a -> 'a option -> 'a

(** [unopt_map f d x] is [y] if [x] is [Some y], [d] if [x] is [None] **)
val unopt_map: f:('a -> 'b) -> default:'b -> 'a option -> 'b

(** [x] for elements [Some x] in input. [None]s are dropped. **)
val unopt_list: 'a option list -> 'a list

(** First input of form [Some x], or [None] if none **)
val first_some: 'a option -> 'a option -> 'a option

(** Print a paragraph in a box **)
val display_paragraph: Format.formatter -> string -> unit

(** [remove nb list] remove the first [nb] elements from the list [list]. *)
val remove_elem_from_list: int -> 'a list -> 'a list

(** Return pair (list of first n items, remaining items) **)
val split_list_at: int -> 'a list -> 'a list * 'a list

(** [true] if input has prefix **)
val has_prefix: prefix:string -> string -> bool

(** Some (input with [prefix] removed), if string has [prefix], else [None] **)
val remove_prefix: prefix:string -> string -> string option

(** Length of common prefix of input strings *)
val common_prefix: string -> string -> int

(** [filter_map f l] is [[y for x in l where (f x) = Some y]] **)
val filter_map: ('a -> 'b option) -> 'a list -> 'b list

(** [list_rev_sub l n] is [List.rev l] capped to max [n] elements *)
val list_rev_sub : 'a list -> int -> 'a list
(** [list_sub l n] is [l] capped to max [n] elements *)
val list_sub: 'a list -> int -> 'a list

(** Like [List.hd], but [Some hd] or [None] if empty **)
val list_hd_opt: 'a list -> 'a option

(** Last elt of list, or raise Not_found if empty **)
val list_last_exn: 'a list -> 'a

(** [merge_filter_list2 ~compare ~f l1 l2] merges two lists ordered by [compare]
    and whose items can be merged with [f]. Item is discarded or kept whether
    [f] returns [Some] or [None] *)
val merge_filter_list2 :
  ?finalize:('a list -> 'a list) ->
  ?compare:('a -> 'a -> int) ->
  ?f:('a option -> 'a option -> 'a option) ->
  'a list -> 'a list ->
  'a list

(** [merge_list2 ~compare ~f l1 l2] merges two lists ordered by [compare] and
    whose items can be merged with [f] *)
val merge_list2 :
  ?finalize:('a list -> 'a list) ->
  ?compare:('a -> 'a -> int) ->
  ?f:('a -> 'a -> 'a) ->
  'a list -> 'a list ->
  'a list

(** [finalize f g ] ensures g() called after f(), even if exception raised **)
val finalize: (unit -> 'a) -> (unit -> unit) -> 'a

(** Return contents of file at given filename. **)
val read_file: ?bin:bool -> string -> string

(** [write_file p c] writes c to file at path p **)
val write_file: ?bin:bool -> string -> string -> unit

(** Compose functions from right to left. *)
val (<<) : ('b -> 'c) -> ('a -> 'b) -> 'a -> 'c

(** Sequence: [i--j] is the sequence [i;i+1;...;j-1;j] *)
val (--) : int -> int -> int list

(** [repeat n x] is a list of [n] [x]'s **)
val repeat: int -> 'a -> 'a list

(** [take_n n l] returns the [n] first elements of [l]. When [compare]
    is provided, it returns the [n] greatest element of [l]. *)
val take_n: ?compare:('a -> 'a -> int) -> int -> 'a list -> 'a list

(** Bounded sequence: keep only the [n] greatest elements. *)
module Bounded(E: Set.OrderedType) : sig
  type t
  val create: int -> t
  val insert: E.t -> t -> unit
  val get: t -> E.t list
end

(** [select n l] is ([n]th element of [l], [l] without that element) **)
val select: int -> 'a list -> 'a * 'a list

(** [parse_addr_port uri] is (node, service) where [node] is the DNS or
    IP and service is the optional port number or service name. *)
val parse_addr_port: string -> string * string
