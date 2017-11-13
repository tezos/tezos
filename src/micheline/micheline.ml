(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type ('l, 'p) node =
  | Int of 'l * string
  | String of 'l * string
  | Prim of 'l * 'p * ('l, 'p) node list * string option
  | Seq of 'l * ('l, 'p) node list * string option

type canonical_location = int

type 'p canonical = Canonical of (canonical_location, 'p) node

let canonical_location_encoding =
  let open Data_encoding in
  def
    "canonicalExpressionLocation" @@
  describe
    ~title:
      "Canonical location in a Micheline expression"
    ~description:
      "The location of a node in a Micheline expression tree \
       in prefix order, with zero being the root and adding one \
       for every basic node, sequence and primitive application." @@
  int31

let location = function
  | Int (loc, _) -> loc
  | String (loc, _) -> loc
  | Seq (loc, _, _) -> loc
  | Prim (loc, _, _, _) -> loc

let annotation = function
  | Int (_, _) -> None
  | String (_, _) -> None
  | Seq (_, _, annot) -> annot
  | Prim (_, _, _, annot) -> annot


let root (Canonical expr) = expr

let strip_locations root =
  let id = let id = ref (-1) in fun () -> incr id ; !id in
  let rec strip_locations l =
    let id = id () in
    match l with
    | Int (_, v) ->
        Int (id, v)
    | String (_, v) ->
        String (id, v)
    | Seq (_, seq, annot) ->
        Seq (id, List.map strip_locations seq, annot)
    | Prim (_, name, seq, annot) ->
        Prim (id, name, List.map strip_locations seq, annot) in
  Canonical (strip_locations root)

let extract_locations root =
  let id = let id = ref (-1) in fun () -> incr id ; !id in
  let loc_table = ref [] in
  let rec strip_locations l =
    let id = id () in
    match l with
    | Int (loc, v) ->
        loc_table := (id, loc) :: !loc_table ;
        Int (id, v)
    | String (loc, v) ->
        loc_table := (id, loc) :: !loc_table ;
        String (id, v)
    | Seq (loc, seq, annot) ->
        loc_table := (id, loc) :: !loc_table ;
        Seq (id, List.map strip_locations seq, annot)
    | Prim (loc, name, seq, annot) ->
        loc_table := (id, loc) :: !loc_table ;
        Prim (id, name, List.map strip_locations seq, annot) in
  let stripped = strip_locations root in
  Canonical stripped, List.rev !loc_table

let inject_locations lookup (Canonical root) =
  let rec inject_locations l =
    match l with
    | Int (loc, v) ->
        Int (lookup loc, v)
    | String (loc, v) ->
        String (lookup loc, v)
    | Seq (loc, seq, annot) ->
        Seq (lookup loc, List.map inject_locations seq, annot)
    | Prim (loc, name, seq, annot) ->
        Prim (lookup loc, name, List.map inject_locations seq, annot) in
  inject_locations root

let map f (Canonical expr) =
  let rec map_node f = function
    | Int _ | String _ as node -> node
    | Seq (loc, seq, annot) ->
        Seq (loc, List.map (map_node f) seq, annot)
    | Prim (loc, name, seq, annot) ->
        Prim (loc, f name, List.map (map_node f) seq, annot) in
  Canonical (map_node f expr)

let rec map_node fl fp = function
  | Int (loc, v) ->
      Int (fl loc, v)
  | String (loc, v) ->
      String (fl loc, v)
  | Seq (loc, seq, annot) ->
      Seq (fl loc, List.map (map_node fl fp) seq, annot)
  | Prim (loc, name, seq, annot) ->
      Prim (fl loc, fp name, List.map (map_node fl fp) seq, annot)

let canonical_encoding prim_encoding =
  let open Data_encoding in
  let int_encoding =
    obj1 (req "int" string) in
  let string_encoding =
    obj1 (req "string" string) in
  let application_encoding expr_encoding =
    obj3 (req "prim" prim_encoding) (req "args" (list expr_encoding)) (opt "annot" string) in
  let seq_encoding expr_encoding =
    list expr_encoding in
  let node_encoding = mu "tezosScriptExpression" (fun expr_encoding ->
      describe
        ~title: "Script expression (data, type or code)" @@
      union ~tag_size:`Uint8
        [ case ~tag:0 int_encoding
            (function Int (_, v) -> Some v | _ -> None)
            (fun v -> Int (0, v)) ;
          case ~tag:1 string_encoding
            (function String (_, v) -> Some v | _ -> None)
            (fun v -> String (0, v)) ;
          case ~tag:2 (application_encoding expr_encoding)
            (function
              | Prim (_, v, args, annot) -> Some (v, args, annot)
              | _ -> None)
            (function (prim, args, annot) -> Prim (0, prim, args, annot)) ;
          case ~tag:3 (seq_encoding expr_encoding)
            (function Seq (_, v, _annot) -> Some v | _ -> None)
            (fun args -> Seq (0, args, None)) ]) in
  conv
    (function Canonical node -> node)
    (fun node -> strip_locations node)
    node_encoding

let table_encoding location_encoding prim_encoding =
  let open Data_encoding in
  conv
    (fun node ->
       let canon, assoc = extract_locations node in
       let _, table = List.split assoc in
       (canon, table))
    (fun (canon, table) ->
       let table = Array.of_list table in
       inject_locations (fun i -> table.(i)) canon)
    (obj2
       (req "expression" (canonical_encoding prim_encoding))
       (req "locations" (list location_encoding)))

let erased_encoding default_location prim_encoding =
  let open Data_encoding in
  conv
    (fun node -> strip_locations node)
    (fun canon -> inject_locations (fun _ -> default_location) canon)
    (canonical_encoding prim_encoding)
