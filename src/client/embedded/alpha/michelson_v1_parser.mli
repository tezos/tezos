(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(** The result of parsing and expanding a Michelson V1 script or data. *)
type parsed =
  { source :
      (** The original source code. *)
      string ;
    unexpanded :
      (** Original expression with macros. *)
      string Micheline.canonical ;
    expanded :
      (** Expression with macros fully expanded. *)
      Script.expr ;
    expansion_table :
      (** Associates unexpanded nodes to their parsing locations and
          the nodes expanded from it in the expanded expression. *)
      (int * (Micheline_parser.location * int list)) list ;
    unexpansion_table :
      (** Associates an expanded node to its source in the unexpanded
          expression. *)
      (int * int) list }

val parse_toplevel : ?check:bool -> string -> parsed tzresult
val parse_expression : ?check:bool -> string -> parsed tzresult
