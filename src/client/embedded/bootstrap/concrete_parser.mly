
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
| node = line_node { node }
|        line_node error
    (* Un seul elt par bloc de '(' ... ')' (pas de NEWLINE ou de ';' *)
  { raise (Sequence_in_parens (pos $startpos $endpos)) }
| node = line_node INDENT nodes = nodes DEDENT { apply_seq node nodes }

line_node:
| n1 = line_node n2 = line_node %prec apply { apply n1 n2 }
| LPAREN node = node RPAREN { node }
| LBRACE nodes = nodes RBRACE { Seq (pos $startpos $endpos, nodes) }
| prim = PRIM { Prim (pos $startpos $endpos, prim, []) }
| i = INT { Int (pos $startpos $endpos, i) }
| f = FLOAT { Float (pos $startpos $endpos, f) }
| s = STRING { String (pos $startpos $endpos, s) }

%%
