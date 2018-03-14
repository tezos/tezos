(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
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
    "micheline.location" @@
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

let canonical_encoding ~variant prim_encoding =
  let open Data_encoding in
  let int_encoding =
    obj1 (req "int" string) in
  let string_encoding =
    obj1 (req "string" string) in
  let int_encoding tag =
    case tag int_encoding
      (function Int (_, v) -> Some v | _ -> None)
      (fun v -> Int (0, v)) in
  let string_encoding tag =
    case tag string_encoding
      (function String (_, v) -> Some v | _ -> None)
      (fun v -> String (0, v)) in
  let seq_encoding tag expr_encoding =
    case tag (list expr_encoding)
      (function Seq (_, v, _annot) -> Some v | _ -> None)
      (fun args -> Seq (0, args, None)) in
  let application_encoding tag expr_encoding =
    case tag
      (obj3 (req "prim" prim_encoding)
         (req "args" (list expr_encoding))
         (opt "annot" string))
      (function Prim (_, prim, args, annot) -> Some (prim, args, annot)
              | _ -> None)
      (fun (prim, args, annot) -> Prim (0, prim, args, annot)) in
  let node_encoding = mu ("micheline." ^ variant ^ ".expression") (fun expr_encoding ->
      describe
        ~title: ("Micheline expression (" ^ variant ^ " variant)") @@
      splitted
        ~json:(union ~tag_size:`Uint8
                 [ int_encoding Json_only;
                   string_encoding Json_only ;
                   seq_encoding Json_only expr_encoding ;
                   application_encoding Json_only expr_encoding ])
        ~binary:(union ~tag_size:`Uint8
                   [ int_encoding (Tag 0) ;
                     string_encoding (Tag 1) ;
                     seq_encoding (Tag 2) expr_encoding ;
                     (* No args, no annot *)
                     case (Tag 3)
                       (obj1 (req "prim" prim_encoding))
                       (function Prim (_, v, [], None) -> Some v
                               | _ -> None)
                       (fun v -> Prim (0, v, [], None)) ;
                     (* No args, with annot *)
                     case (Tag 4)
                       (obj2 (req "prim" prim_encoding)
                          (req "annot" string))
                       (function
                         | Prim (_, v, [], Some annot) -> Some (v, annot)
                         | _ -> None)
                       (function (prim, annot) -> Prim (0, prim, [], Some annot)) ;
                     (* Single arg, no annot *)
                     case (Tag 5)
                       (obj2 (req "prim" prim_encoding)
                          (req "arg" expr_encoding))
                       (function
                         | Prim (_, v, [ arg ], None) -> Some (v, arg)
                         | _ -> None)
                       (function (prim, arg) -> Prim (0, prim, [ arg ], None)) ;
                     (* Single arg, with annot *)
                     case (Tag 6)
                       (obj3 (req "prim" prim_encoding)
                          (req "arg" expr_encoding)
                          (req "annot" string))
                       (function
                         | Prim (_, prim, [ arg ], Some annot) -> Some (prim, arg, annot)
                         | _ -> None)
                       (fun (prim, arg, annot) -> Prim (0, prim, [ arg ], Some annot)) ;
                     (* Two args, no annot *)
                     case (Tag 7)
                       (obj3 (req "prim" prim_encoding)
                          (req "arg1" expr_encoding)
                          (req "arg2" expr_encoding))
                       (function
                         | Prim (_, prim, [ arg1 ; arg2 ], None) -> Some (prim, arg1, arg2)
                         | _ -> None)
                       (fun (prim, arg1, arg2) -> Prim (0, prim, [ arg1 ; arg2 ], None)) ;
                     (* Two args, with annot *)
                     case (Tag 8)
                       (obj4 (req "prim" prim_encoding)
                          (req "arg1" expr_encoding)
                          (req "arg2" expr_encoding)
                          (req "annot" string))
                       (function
                         | Prim (_, prim, [ arg1 ; arg2 ], Some annot) -> Some (prim, arg1, arg2, annot)
                         | _ -> None)
                       (fun (prim, arg1, arg2, annot) -> Prim (0, prim, [ arg1 ; arg2 ], Some annot)) ;
                     (* General case *)
                     application_encoding (Tag 9) expr_encoding ]))
  in
  conv
    (function Canonical node -> node)
    (fun node -> strip_locations node)
    node_encoding

let table_encoding ~variant location_encoding prim_encoding =
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
       (req "expression" (canonical_encoding ~variant prim_encoding))
       (req "locations" (list location_encoding)))

let erased_encoding ~variant default_location prim_encoding =
  let open Data_encoding in
  conv
    (fun node -> strip_locations node)
    (fun canon -> inject_locations (fun _ -> default_location) canon)
    (canonical_encoding ~variant prim_encoding)
