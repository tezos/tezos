
%token DEDENT
%token EOF
%token INDENT
%token LBRACE
%token LPAREN
%token NEWLINE
%token RBRACE
%token RPAREN
%token SEMICOLON

%token <string> INT
%token <string> PRIM
%token <string> STRING

%left PRIM INT LPAREN LBRACE STRING
%left apply

%start <Script_located_ir.node list> tree

%{

open Script_located_ir

let expand_caddadr original =
  match original with
  | Prim (loc, str, []) ->
     let len = String.length str in
     if len > 3
        && String.get str 0 = 'C'
        && String.get str (len - 1) = 'R' then
       let rec parse i acc =
         if i = 0 then
           Some (Seq (loc, acc))
         else
           match String.get str i with
           | 'A' -> parse (i - 1) (Prim (loc, "CAR", []) :: acc)
           | 'D' -> parse (i - 1) (Prim (loc, "CDR", []) :: acc)
           | _ -> None in
       parse (len - 2) []
     else
       None
  | _ -> None

exception Not_a_roman

let decimal_of_roman roman =
  (* http://rosettacode.org/wiki/Roman_numerals/Decode#OCaml *)
  let arabic = ref 0 in
  let lastval = ref 0 in
  for i = (String.length roman) - 1 downto 0 do
    let n =
      match roman.[i] with
      | 'M' -> 1000
      | 'D' -> 500
      | 'C' -> 100
      | 'L' -> 50
      | 'X' -> 10
      | 'V' -> 5
      | 'I' -> 1
      | _ -> raise Not_a_roman
    in
    if Compare.Int.(n < !lastval)
    then arabic := !arabic - n
    else arabic := !arabic + n;
    lastval := n
  done;
  !arabic

let expand_dxiiivp original =
  match original with
  | Prim (loc, str, [ arg ]) ->
     let len = String.length str in
     if len > 3
        && String.get str 0 = 'D'
        && String.get str (len - 1) = 'P' then
       try
         let depth = decimal_of_roman (String.sub str 1 (len - 2)) in
         let rec make i acc =
           if i = 0 then
             acc
           else
             make (i - 1)
               (Seq (loc, [ Prim (loc, "DIP", [ acc ]) ])) in
         Some (make depth arg)
       with Not_a_roman -> None
     else None
  | _ -> None

exception Not_a_pair

let expand_paaiair original =
  match original with
  | Prim (loc, str, []) ->
     let len = String.length str in
     if len > 4
        && String.get str 0 = 'P'
        && String.get str (len - 1) = 'R' then
       try
         let rec parse i acc =
           if i = 0 then
             acc
           else if String.get str i = 'I'
              && String.get str (i - 1) = 'A' then
             parse (i - 2) (Prim (loc, "PAIR", []) :: acc)
           else if String.get str i = 'A' then
             match acc with
             | [] ->
                raise Not_a_pair
             | acc :: accs ->
                parse (i - 1) (Prim (loc, "DIP", [ acc ]) :: accs)
           else
             raise Not_a_pair in
         Some (Seq (loc, parse (len - 2) []))
       with Not_a_pair -> None
     else
       None
  | _ -> None

exception Not_a_dup

let expand_duuuuup original =
  match original with
  | Prim (loc, str, []) ->
     let len = String.length str in
     if len > 3
        && String.get str 0 = 'D'
        && String.get str 1 = 'U'
        && String.get str (len - 1) = 'P' then
       try
         let rec parse i acc =
           if i = 1 then acc
           else if String.get str i = 'U' then
             parse (i - 1)
                   (Seq (loc, [ Prim (loc, "DIP", [ acc ]) ;
                                Prim (loc, "SWAP", []) ]))
           else
             raise Not_a_dup in
         Some (parse (len - 2) (Seq (loc, [ Prim (loc, "DUP", []) ])))
       with Not_a_dup -> None
     else
       None
  | _ -> None


let expand_compare original =
  match original with
  | Prim (loc, "CMPEQ", []) ->
     Some (Seq (loc, [ Prim (loc, "COMPARE", []) ;
                       Prim (loc, "EQ", []) ]))
  | Prim (loc, "CMPNEQ", []) ->
     Some (Seq (loc, [ Prim (loc, "COMPARE", []) ;
                       Prim (loc, "NEQ", []) ]))
  | Prim (loc, "CMPLT", []) ->
     Some (Seq (loc, [ Prim (loc, "COMPARE", []) ;
                       Prim (loc, "LT", []) ]))
  | Prim (loc, "CMPGT", []) ->
     Some (Seq (loc, [ Prim (loc, "COMPARE", []) ;
                       Prim (loc, "GT", []) ]))
  | Prim (loc, "CMPLE", []) ->
     Some (Seq (loc, [ Prim (loc, "COMPARE", []) ;
                       Prim (loc, "LE", []) ]))
  | Prim (loc, "CMPGE", []) ->
     Some (Seq (loc, [ Prim (loc, "COMPARE", []) ;
                       Prim (loc, "GE", []) ]))
  | Prim (loc, "IFCMPEQ", args) ->
     Some (Seq (loc, [ Prim (loc, "COMPARE", []) ;
                       Prim (loc, "EQ", []) ;
                       Prim (loc, "IF", args) ]))
  | Prim (loc, "IFCMPNEQ", args) ->
     Some (Seq (loc, [ Prim (loc, "COMPARE", []) ;
                       Prim (loc, "NEQ", []) ;
                       Prim (loc, "IF", args) ]))
  | Prim (loc, "IFCMPLT", args) ->
     Some (Seq (loc, [ Prim (loc, "COMPARE", []) ;
                       Prim (loc, "LT", []) ;
                       Prim (loc, "IF", args) ]))
  | Prim (loc, "IFCMPGT", args) ->
     Some (Seq (loc, [ Prim (loc, "COMPARE", []) ;
                       Prim (loc, "GT", []) ;
                       Prim (loc, "IF", args) ]))
  | Prim (loc, "IFCMPLE", args) ->
     Some (Seq (loc, [ Prim (loc, "COMPARE", []) ;
                       Prim (loc, "LE", []) ;
                       Prim (loc, "IF", args) ]))
  | Prim (loc, "IFCMPGE", args) ->
     Some (Seq (loc, [ Prim (loc, "COMPARE", []) ;
                       Prim (loc, "GE", []) ;
                       Prim (loc, "IF", args) ]))
  | Prim (loc, "IFEQ", args) ->
     Some (Seq (loc, [ Prim (loc, "EQ", []) ;
                       Prim (loc, "IF", args) ]))
  | Prim (loc, "IFNEQ", args) ->
     Some (Seq (loc, [ Prim (loc, "NEQ", []) ;
                       Prim (loc, "IF", args) ]))
  | Prim (loc, "IFLT", args) ->
     Some (Seq (loc, [ Prim (loc, "LT", []) ;
                       Prim (loc, "IF", args) ]))
  | Prim (loc, "IFGT", args) ->
     Some (Seq (loc, [ Prim (loc, "GT", []) ;
                       Prim (loc, "IF", args) ]))
  | Prim (loc, "IFLE", args) ->
     Some (Seq (loc, [ Prim (loc, "LE", []) ;
                       Prim (loc, "IF", args) ]))
  | Prim (loc, "IFGE", args) ->
     Some (Seq (loc, [ Prim (loc, "GE", []) ;
                       Prim (loc, "IF", args) ]))
  | _ -> None

let expand original =
  let try_expansions expanders =
    match
      List.fold_left
        (fun acc f ->
          match acc with
          | None -> f original
          | Some rewritten -> Some rewritten)
        None expanders with
    | None -> original
    | Some rewritten -> rewritten in
  try_expansions
    [ expand_dxiiivp ;
      expand_paaiair ;
      expand_caddadr ;
      expand_duuuuup ;
      expand_compare ]

let loc = function
  | Prim (loc, _, _) | Int (loc, _) | String (loc, _) | Seq (loc, _) -> loc

let apply node arg =
  match node with
  | Prim ((sloc, _), n, args) ->
     Prim ((sloc, snd (loc arg)), n, args @ [arg])
  | Int _ | String _ | Seq _ as _node ->
      raise (Invalid_application (node_location arg))

let rec apply_seq node = function
  | [] -> node
  | n1 :: n2 -> apply_seq (apply node n1) n2

let pos p1 p2 =
  ({ line = p1.Lexing.pos_lnum ;
    column = p1.Lexing.pos_cnum - p1.Lexing.pos_bol ;
    point = p1.Lexing.pos_cnum },
   { line = p2.Lexing.pos_lnum ;
     column = p2.Lexing.pos_cnum - p2.Lexing.pos_bol ;
     point = p2.Lexing.pos_cnum })

%}

%%

%public tree:
| node = nodes EOF { node }
| INDENT node = nodes DEDENT EOF { node }

nodes:
| { [] }
| n1 = node { [n1] }
| n1 = node SEMICOLON n2 = nodes { n1 :: n2 }
| n1 = node SEMICOLON NEWLINE n2 = nodes {  n1 :: n2 }
| n1 = node NEWLINE n2 = nodes {  n1 :: n2 }

node:
| node = line_node { expand node }
|        line_node error
    (* Un seul elt par bloc de '(' ... ')' (pas de NEWLINE ou de ';' *)
  { raise (Sequence_in_parens (pos $startpos $endpos)) }
| node = line_node INDENT nodes = nodes DEDENT { expand (apply_seq node nodes) }

line_node:
| n1 = line_node n2 = line_node %prec apply { apply n1 n2 }
| LPAREN node = node RPAREN { node }
| LBRACE nodes = nodes RBRACE { Seq (pos $startpos $endpos, nodes) }
| prim = PRIM { Prim (pos $startpos $endpos, prim, []) }
| i = INT { Int (pos $startpos $endpos, i) }
| s = STRING { String (pos $startpos $endpos, s) }

%%
