(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

module Infix : sig

  (** Compose functions from right to left. *)
  val (<<) : ('b -> 'c) -> ('a -> 'b) -> 'a -> 'c

  (** Sequence: [i--j] is the sequence [i;i+1;...;j-1;j] *)
  val (--) : int -> int -> int list

end

(** Print a paragraph in a box **)
val display_paragraph: Format.formatter -> string -> unit

(** [finalize f g ] ensures g() called after f(), even if exception raised **)
val finalize: (unit -> 'a) -> (unit -> unit) -> 'a

(** Return contents of file at given filename. **)
val read_file: ?bin:bool -> string -> string

(** [write_file p c] writes c to file at path p **)
val write_file: ?bin:bool -> string -> string -> unit

val mkdir: string -> unit
