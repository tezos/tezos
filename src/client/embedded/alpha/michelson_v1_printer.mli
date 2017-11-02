(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

val print_expr :
  Format.formatter -> Script_repr.expr -> unit

val print_expr_unwrapped :
  Format.formatter -> Script_repr.expr -> unit

(** Insert the type map returned by the typechecker as comments in a
    printable Micheline AST. *)
val inject_types :
  Script_ir_translator.type_map ->
  Michelson_v1_parser.parsed ->
  Micheline_printer.node

(** Unexpand the macros and produce the result of parsing an
    intermediate pretty printed source. Useful when working with
    contracts extracted from the blockchain and not local files. *)
val unparse_toplevel : ?type_map: Script_ir_translator.type_map -> Script.expr -> Michelson_v1_parser.parsed
val unparse_expression : Script.expr -> Michelson_v1_parser.parsed
