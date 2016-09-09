
%token DEDENT
%token EOF
%token INDENT
%token LBRACE
%token LPAREN
%token NEWLINE
%token RBRACE
%token RPAREN
%token SEMICOLON

%token <string> FLOAT
%token <string> INT
%token <string> PRIM
%token <string> STRING

%left PRIM INT FLOAT LPAREN LBRACE STRING
%left apply

%start <Script_located_ir.node list> tree

%{

open Script_located_ir

let expand_caddadr loc str =
  let len = String.length str in
  if len > 3
  && String.get str 0 = 'c'
  && String.get str (len - 1) = 'r' then
    let rec parse i acc =
      if i = 0 then
        Some (Seq (loc, acc))
      else
        match String.get str i with
        | 'a' -> parse (i - 1) (Prim (loc, "car", []) :: acc)
        | 'd' -> parse (i - 1) (Prim (loc, "cdr", []) :: acc)
        | _ -> None in
    parse (len - 2) []
  else
    None

exception Not_a_roman

let decimal_of_roman roman =
  (* http://rosettacode.org/wiki/Roman_numerals/Decode#OCaml *)
  let arabic = ref 0 in
  let lastval = ref 0 in
  for i = (String.length roman) - 1 downto 0 do
    let n =
      match roman.[i] with
      | 'm' -> 1000
      | 'd' -> 500
      | 'c' -> 100
      | 'l' -> 50
      | 'x' -> 10
      | 'v' -> 5
      | 'i' -> 1
      | _ -> raise Not_a_roman
    in
    if Compare.Int.(n < !lastval)
    then arabic := !arabic - n
    else arabic := !arabic + n;
    lastval := n
  done;
  !arabic

let expand_dxiiivp loc str arg =
  let len = String.length str in
  if len > 3
  && String.get str 0 = 'd'
  && String.get str (len - 1) = 'p' then
    try
      let depth = decimal_of_roman (String.sub str 1 (len - 2)) in
      let rec make i =
        if i = 0 then
          arg
        else
          let sub = make (i - 1) in
          Prim (loc, "dip", [ sub ]) in
      Some (make depth)
    with Not_a_roman -> None
  else None

exception Not_a_pair

let expand_paaiair loc str =
  let len = String.length str in
  if len > 4
  && String.get str 0 = 'p'
  && String.get str (len - 1) = 'r' then
    try
      let rec parse i acc =
        if String.get str i = 'i'
        && String.get str (i - 1) = 'a' then
          parse (i - 2) (Prim (loc, "pair", []) :: acc)
        else if String.get str i = 'a' then
          match acc with
          | [] ->
              raise Not_a_pair
          | acc :: accs ->
              parse (i - 1) (Prim (loc, "dip", [ acc ]) :: accs)
        else
          raise Not_a_pair in
      Some (Seq (loc, parse (len - 2) []))
    with Not_a_pair -> None
  else
    None

let expand = function
  | Prim (loc, name, [ arg ]) as original ->
      begin match expand_dxiiivp loc name arg with
        | None -> original
        | Some rewritten -> rewritten
      end
  | Prim (loc, name, []) as original ->
      begin match expand_paaiair loc name with
        | None ->
            begin match expand_caddadr loc name with
              | None -> original
              | Some rewritten -> rewritten
            end
        | Some rewritten -> rewritten
      end
  | original -> original

let apply node arg =
  match node with
  | Prim (loc, n, args) -> Prim (loc, n, args @ [arg])
  | Int _ | Float _ | String _ | Seq _ as _node ->
      raise (Invalid_application (node_location arg))

let rec apply_seq node = function
  | [] -> node
  | n1 :: n2 -> apply_seq (apply node n1) n2

let pos p1 p2 =
  Lexing.((p1.pos_lnum, p1.pos_cnum - p1.pos_bol),
          (p2.pos_lnum, p2.pos_cnum - p2.pos_bol))

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
| f = FLOAT { Float (pos $startpos $endpos, f) }
| s = STRING { String (pos $startpos $endpos, s) }

%%
