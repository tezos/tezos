(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Tezos_micheline

(** The result of parsing and expanding a Michelson V1 script or data. *)
type parsed =
  {
    source : string ;
    (** The original source code. *)
    unexpanded : string Micheline.canonical ;
    (** Original expression with macros. *)
    expanded : Script.expr ;
    (** Expression with macros fully expanded. *)
    expansion_table :
      (int * (Micheline_parser.location * int list)) list ;
    (** Associates unexpanded nodes to their parsing locations and
        the nodes expanded from it in the expanded expression. *)
    unexpansion_table : (int * int) list ;
    (** Associates an expanded node to its source in the unexpanded
        expression. *)
  }

val parse_toplevel : ?check:bool -> string -> parsed Micheline_parser.parsing_result
val parse_expression : ?check:bool -> string -> parsed Micheline_parser.parsing_result
