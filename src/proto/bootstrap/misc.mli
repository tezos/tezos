(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(** {2 Stuff} ****************************************************************)

type 'a lazyt = unit -> 'a
type 'a lazy_list_t = LCons of 'a * ('a lazy_list_t tzresult Lwt.t lazyt)
type 'a lazy_list = 'a lazy_list_t tzresult Lwt.t

(** Include lower bound, exclude upper bound *)
val (-->) : int -> int -> int list
val (--->) : Int32.t -> Int32.t -> Int32.t list

val pp_print_paragraph : Format.formatter -> string -> unit

val take: int -> 'a list -> ('a list * 'a list) option
