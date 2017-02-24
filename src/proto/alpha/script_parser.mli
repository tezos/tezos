(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Contract_repr

(** The node position in the AST in prefix order *)
type location = int
val location_encoding : location Data_encoding.t

(** The three possible parsing contexts *)
type parse_context = Type | Constant | Instr

(** The primitive on which an error happened *)
type parse_symbol = parse_context * string

(** Errors (only used as result in the error monad) *)
exception Invalid_arity of location * parse_symbol * int * int
exception Unknown_primitive of location * parse_symbol
exception Comparable_type_expected of location
exception Invalid_push of location
exception Invalid_node of location * parse_context
exception Invalid_constant of location * string

(** An association from script AST nodes to original expression nodes.
   Nodes are represented by their indexes in expression trees and
   script AST in prefix order, the root being 0. In expression nodes,
   all constants count for 1, as do a primitive applcation or a
   sequence. In AST nodes, all constructors count for 1, except for
   all type contructors and the seq instruction constructor which are
   omitted. *)
type code_map = (int * int) list

(** parse a script expression as code, may return a [Parse_error]  *)
val parse_code: Script_ir.node -> (script_instr * code_map) result

(** parse a script expression as typed data, may return a [Parse_error]  *)
val parse_data: Script_ir.node -> (script_data_ty * script_data * code_map) result

(** parse a script expression as a type, may return a [Parse_error]  *)
val parse_data_type: Script_ir.node -> (script_data_ty * code_map) result
