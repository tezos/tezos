(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type point =
  int * int

type location =
  point * point

type node =
  | Int of location * string
  | String of location * string
  | Prim of location * string * node list
  | Seq of location * node list

let node_location = function
  | Int (loc, _)
  | String (loc, _)
  | Prim (loc, _, _)
  | Seq (loc, _) -> loc

(*-- Located errors ---------------------------------------------------------*)

(* Lexer error *)
exception Illegal_character of location * char
exception Illegal_escape of location * string
exception Invalid_indentation of location
exception Invalid_indentation_after_opener of location * char
exception Invalid_indentation_in_block of location * char * location
exception Newline_in_string of location
exception Unaligned_closer of location * char * char * location
exception Unclosed of location * char * location
exception Unopened of location * char
exception Unterminated_comment of location * location
exception Unterminated_string of location
exception Unterminated_string_in_comment of location * location * location

(* Parser error *)
exception Invalid_application of location
exception Sequence_in_parens of location
exception Missing_program_field of string

(*-- Converters between IR and Located IR -----------------------------------*)

let strip_locations root =
  let id = let id = ref (-1) in fun () -> incr id ; !id in
  let rec strip_locations l =
    let id = id () in
    match l with
    | Int (_, v) ->
        Script.Int (id, v)
    | String (_, v) ->
        Script.String (id, v)
    | Seq (_, seq) ->
        Script.Seq (id, List.map strip_locations seq)
    | Prim (_, name, seq) ->
        Script.Prim (id, name, List.map strip_locations seq) in
  strip_locations root
