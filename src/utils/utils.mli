(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

val remove_dir: string -> unit Lwt.t

val create_dir: ?perm:int -> string -> unit Lwt.t
val create_file: ?perm:int -> string -> string -> unit Lwt.t

val termination_thread: int Lwt.t
val exit: int -> 'a

module StringMap : Map.S with type key = string

(** Splits a string on slashes, grouping multiple slashes, and
    ignoring slashes at the beginning and end of string. *)
val split_path: string -> string list

(** Splits a string on a delimier character, grouping multiple
    delimiters, and ignoring delimiters at the beginning and end of
    string, if [limit] is passed, stops after [limit] split(s). *)
val split: char -> ?limit: int -> string -> string list

val map_option: f:('a -> 'b) -> 'a option -> 'b option
val apply_option: f:('a -> 'b option) -> 'a option -> 'b option
val iter_option: f:('a -> unit) -> 'a option -> unit
val unopt: 'a -> 'a option -> 'a
val unopt_list: 'a option list -> 'a list

val display_paragraph: Format.formatter -> string -> unit

(** [remove nb list] remove the first [nb] elements from the list [list]. *)
val remove_elem_from_list: int -> 'a list -> 'a list

val remove_prefix: prefix:string -> string -> string option

val filter_map: ('a -> 'b option) -> 'a list -> 'b list

val finalize: (unit -> 'a) -> (unit -> unit) -> 'a
val read_file: ?bin:bool -> string -> string
val write_file: ?bin:bool -> string -> string -> unit
