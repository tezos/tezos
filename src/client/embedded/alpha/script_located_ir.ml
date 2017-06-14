(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type point =
  { point : int ;
    byte : int ;
    line : int ;
    column : int }

let point_zero =
  { point = 0 ;
    byte = 0 ;
    line = 0 ;
    column = 0 }

type location =
  { start : point ;
    stop : point }

let location_encoding =
  let open Data_encoding in
  let point_encoding =
    conv
      (fun { line ; column ; point ; byte } -> (line, column, point, byte))
      (fun (line, column, point, byte) -> { line ; column ; point ; byte })
      (obj4
         (req "line" uint16)
         (req "column" uint16)
         (req "point" uint16)
         (req "byte" uint16)) in
  conv
    (fun { start ; stop } -> (start, stop))
    (fun (start, stop) -> { start ; stop })
    (obj2
       (req "start" point_encoding)
       (req "stop" point_encoding))

type node =
  | Int of location * string
  | String of location * string
  | Prim of location * string * node list * string option
  | Seq of location * node list * string option

let node_location = function
  | Int (loc, _)
  | String (loc, _)
  | Prim (loc, _, _, _)
  | Seq (loc, _, _) -> loc

let strip_locations root =
  let id = let id = ref (-1) in fun () -> incr id ; !id in
  let loc_table = ref [] in
  let rec strip_locations l =
    let id = id () in
    match l with
    | Int (loc, v) ->
        loc_table := (id, loc) :: !loc_table ;
        Script.Int (id, v)
    | String (loc, v) ->
        loc_table := (id, loc) :: !loc_table ;
        Script.String (id, v)
    | Seq (loc, seq, annot) ->
        loc_table := (id, loc) :: !loc_table ;
        Script.Seq (id, List.map strip_locations seq, annot)
    | Prim (loc, name, seq, annot) ->
        loc_table := (id, loc) :: !loc_table ;
        Script.Prim (id, name, List.map strip_locations seq, annot) in
  let stripped = strip_locations root in
  stripped, List.rev !loc_table

exception Missing_program_field of string
