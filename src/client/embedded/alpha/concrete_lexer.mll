
{

open Concrete_parser

open Script_located_ir

let count_nl s =
  let c = ref 0 in
  for i = 0 to String.length s - 1 do
    if Compare.Char.(s.[i] = '\010') then
      incr c
  done;
  !c

let update_loc lexbuf nl indent =
  let open Lexing in
  let lcp = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <- { lcp with
                         pos_lnum = lcp.pos_lnum + nl;
                         pos_bol = lcp.pos_cnum - indent;
                       }

let may_update_loc lexbuf nl indent =
  if Compare.Int.(nl <> 0) then update_loc lexbuf nl indent

let start_offset lexbuf =
  let open Lexing in
  let lsp = lexbuf.lex_start_p in
  lsp.pos_cnum - lsp.pos_bol

let end_offset lexbuf =
  let open Lexing in
  let lcp = lexbuf.lex_curr_p in
  lcp.pos_cnum - lcp.pos_bol

let curr_location lexbuf =
  lexbuf.Lexing.lex_start_p, lexbuf.Lexing.lex_curr_p

let pos pos =
  Lexing.(pos.pos_lnum, pos.pos_cnum - pos.pos_bol)

let pos2 (start, stop) =
  pos start, pos stop

(* To translate escape sequences *)

let char_for_backslash = function
  | 'n' -> '\010'
  | 'r' -> '\013'
  | 'b' -> '\008'
  | 't' -> '\009'
  | c   -> c

let char_for_decimal_code lexbuf i =
  let c = 100 * (int_of_char(Lexing.lexeme_char lexbuf i) - 48) +
           10 * (int_of_char(Lexing.lexeme_char lexbuf (i+1)) - 48) +
                (int_of_char(Lexing.lexeme_char lexbuf (i+2)) - 48) in
  if Compare.Int.(c < 0 || c > 255) then
    raise @@ Illegal_escape (pos2 (curr_location lexbuf), Lexing.lexeme lexbuf)
  else char_of_int c

let char_for_hexadecimal_code lexbuf i =
  let d1 = int_of_char (Lexing.lexeme_char lexbuf i) in
  let val1 = if Compare.Int.(d1 >= 97) then d1 - 87
             else if Compare.Int.(d1 >= 65) then d1 - 55
             else d1 - 48
  in
  let d2 = int_of_char (Lexing.lexeme_char lexbuf (i+1)) in
  let val2 = if Compare.Int.(d2 >= 97) then d2 - 87
             else if Compare.Int.(d2 >= 65) then d2 - 55
             else d2 - 48
  in
  char_of_int (val1 * 16 + val2)

(** Lexer state *)

type state = {
  mutable indent_stack:
    (int * [`Indent | `Open of (char * (Lexing.position * Lexing.position)) ]) list;
  mutable buffer: Concrete_parser.token list;
  mutable string_buff: bytes;
  mutable string_index: int;
  mutable string_start_loc: Lexing.position * Lexing.position;
  mutable comment_start_loc: (Lexing.position * Lexing.position) list;
}

let init_state () = {
  indent_stack = [];
  buffer = [];
  string_index = 0;
  string_buff = Bytes.create 256;
  string_start_loc = Lexing.dummy_pos, Lexing.dummy_pos;
  comment_start_loc = [];
}


(** String helpers *)

let reset_string_buffer st =
  st.string_buff <- Bytes.create 256;
  st.string_index <- 0

let store_string_char st c =
  if st.string_index >= Bytes.length st.string_buff then begin
    let new_buff = Bytes.create (Bytes.length (st.string_buff) * 2) in
    Bytes.blit st.string_buff 0 new_buff 0 (Bytes.length st.string_buff);
    st.string_buff <- new_buff
  end;
  Bytes.set st.string_buff st.string_index c;
  st.string_index <- st.string_index + 1

let store_string st s =
  for i = 0 to String.length s - 1 do
    store_string_char st s.[i];
  done

let store_lexeme st lexbuf =
  store_string st (Lexing.lexeme lexbuf)

let get_stored_string st =
  let s = Bytes.sub st.string_buff 0 st.string_index in
  st.string_buff <- Bytes.create 256;
  Bytes.to_string s


(** Indentation helpers *)

let first_token st =
  match st.indent_stack with
  | [] -> true
  | _ :: _ -> false

let starting_offset (start, _) =
  let open Lexing in
  start.pos_cnum - start.pos_bol

let rec pop_indent st loc xs i =
  match xs with
  | [] -> assert false
  | ((x, _) :: _) as xs when Compare.Int.(x = i) ->
      st.indent_stack <- xs;
      [NEWLINE]
  | (x, `Indent) :: xs ->
      if Compare.Int.(x > i) then
        DEDENT :: pop_indent st loc xs i
      else
        raise @@ Invalid_indentation (pos2 loc)
  | (_, `Open (c, opener_loc)) :: _ ->
      let opener_offset = starting_offset opener_loc in
      if Compare.Int.(i > opener_offset) then
        raise @@ Invalid_indentation_in_block (pos2 loc, c, pos2 opener_loc)
      else
        raise @@ Unclosed (pos2 loc, c, pos2 opener_loc)

let indent_token st loc =
  let i = starting_offset loc in
  match st.indent_stack with
  | (x, `Indent) :: xs when Compare.Int.(x > i) ->
      DEDENT :: pop_indent st loc xs i;
  | (x, `Open (c, opener_loc)) :: _ when Compare.Int.(x > i) ->
      let opener_offset = starting_offset opener_loc in
      if Compare.Int.(i > opener_offset) then
        raise @@ Invalid_indentation_in_block (pos2 loc, c, pos2 opener_loc)
      else
        raise @@ Unclosed (pos2 loc, c, pos2 opener_loc)
  | (x, _) :: _ when Compare.Int.(x = i) ->
      [NEWLINE]
  | [] | (_, _) :: _ (* when Compare.Int.(x < i) *) ->
      st.indent_stack <- (i, `Indent) :: st.indent_stack;
      [INDENT]

let open_block st opener opener_loc token_offset =
  let opener_offset = starting_offset opener_loc in
  if Compare.Int.(token_offset <= opener_offset) then
    raise @@ Invalid_indentation_after_opener (pos2 opener_loc, opener) ;
  st.indent_stack <-
    (token_offset, `Open (opener, opener_loc)) :: st.indent_stack;
  match opener with
  | '{' -> [LBRACE]
  | '(' -> [LPAREN]
  |  _  -> assert false

let close_block st bol closer closer_loc =
  let closer_offset = starting_offset closer_loc in
  let rec pop xs =
    match xs with
    | [] -> raise @@ Unopened (pos2 closer_loc, closer)
    | (_, `Indent) :: xs -> DEDENT :: pop xs
    | (_, `Open (opener, opener_loc)) :: xs ->
        let opener_offset = starting_offset opener_loc in
        if bol && Compare.Int.(opener_offset <> closer_offset) then
          raise @@
          Unaligned_closer (pos2 closer_loc, opener, closer, pos2 opener_loc) ;
        st.indent_stack <- xs;
        [ match opener, closer with
          | '{', '}' -> RBRACE
          | '(', ')' -> RPAREN
          |  _       ->
              raise @@ Unclosed (pos2 closer_loc, opener, pos2 opener_loc) ]
  in
  pop st.indent_stack

}

let eol_comment = '#' [^ '\010'] *
let newline = eol_comment ? ('\010' | "\013\010" )
let space = [' ']
let firstidentchar = ['A'-'Z' 'a'-'z' '_']
let identchar = ['A'-'Z' 'a'-'z' '_' '\'' '0'-'9']
let decimal_literal =
  ['0'-'9'] ['0'-'9' '_']*
let hex_literal =
  '0' ['x' 'X'] ['0'-'9' 'A'-'F' 'a'-'f']['0'-'9' 'A'-'F' 'a'-'f' '_']*
let oct_literal =
  '0' ['o' 'O'] ['0'-'7'] ['0'-'7' '_']*
let bin_literal =
  '0' ['b' 'B'] ['0'-'1'] ['0'-'1' '_']*
let int_literal =
  '-' ? ( decimal_literal | hex_literal | oct_literal | bin_literal)

rule indent_tokens st nl = parse

| space   { indent_tokens st nl lexbuf }
| newline { Lexing.new_line lexbuf; indent_tokens st (nl + 1) lexbuf }

| ""
    { let bol = nl <> 0 || first_token st in
      if bol then indent_token st (curr_location lexbuf) else [] }

| "/*"
    { st.comment_start_loc <- [curr_location lexbuf];
      comment st nl lexbuf }

| ('{' | '(' as opener)
    { let opener_loc = curr_location lexbuf in
      let token_offset = next_token_indent st lexbuf in
      let bol = nl <> 0 || first_token st in
      let prefix =
        if bol then indent_token st opener_loc else [] in
      prefix @ open_block st opener opener_loc token_offset }

| ('}' | ')' as closer)
    { let closer_loc = curr_location lexbuf in
      let bol = Compare.Int.(nl <> 0) in
      close_block st bol closer closer_loc }

| eof
    { List.map
        (function
          | (_, `Indent) -> DEDENT
          | (_, `Open (c, loc)) ->
              raise @@ Unclosed (pos2 (curr_location lexbuf), c, pos2 loc))
        st.indent_stack
      @ [EOF]
    }

and comment st nl = parse

| "/*"    { st.comment_start_loc <-
              curr_location lexbuf :: st.comment_start_loc;
            comment st nl lexbuf }

| "*/"    { match st.comment_start_loc with
            | [] -> assert false
            | [_] -> indent_tokens st nl lexbuf
            | _ :: xs -> st.comment_start_loc <- xs; comment st nl lexbuf }

| "\""    { st.string_start_loc <- curr_location lexbuf;
            let nl =
              try string st nl lexbuf
              with Unterminated_string str_start ->
                match st.comment_start_loc with
                | [] -> assert false
                | loc :: _ ->
                    let start = List.hd (List.rev st.comment_start_loc) in
                    raise @@
                    Unterminated_string_in_comment (pos2 loc, pos2 start, str_start)
            in
            comment st nl lexbuf }

| newline { Lexing.new_line lexbuf; comment st (nl+1) lexbuf }

| eof     { match st.comment_start_loc with
            | [] -> assert false
            | loc :: _ ->
                let start = List.hd (List.rev st.comment_start_loc) in
                raise @@ Unterminated_comment (pos2 loc, pos2 start) }

| _       { comment st nl lexbuf }


(** Eat spacings and return the next token offset. *)
and next_token_indent st = parse

| space   { next_token_indent st lexbuf }

| newline { Lexing.new_line lexbuf; next_token_indent st lexbuf }

| ""      { end_offset lexbuf }

(** The lexer for non-indentation tokens.
    It should not care about 'space', 'newline', '{}()' nor comments. *)
and raw_token st = parse

| ";" { SEMICOLON }

| firstidentchar identchar *
    { PRIM (Lexing.lexeme lexbuf) }

| int_literal
    { INT (Lexing.lexeme lexbuf) }

| "\""
    { reset_string_buffer st;
      let string_start = lexbuf.Lexing.lex_start_p in
      st.string_start_loc <- curr_location lexbuf;
      ignore (string st 0 lexbuf);
      lexbuf.Lexing.lex_start_p <- string_start;
      STRING (get_stored_string st) }

| _
    { raise (Illegal_character (pos2 (curr_location lexbuf),
                                Lexing.lexeme_char lexbuf 0))
    }

and string st nl = parse
    '"'
      { nl }
  | '\\' newline ([' ' '\t'] * as space)
      { update_loc lexbuf 1 (String.length space);
        string st nl lexbuf
      }
  | '\\' ['\\' '\'' '"' 'n' 't' 'b' 'r' ' ']
      { store_string_char st (char_for_backslash(Lexing.lexeme_char lexbuf 1));
        string st nl lexbuf }
  | '\\' ['0'-'9'] ['0'-'9'] ['0'-'9']
      { store_string_char st (char_for_decimal_code lexbuf 1);
        string st nl lexbuf }
  | '\\' 'x' ['0'-'9' 'a'-'f' 'A'-'F'] ['0'-'9' 'a'-'f' 'A'-'F']
      { store_string_char st (char_for_hexadecimal_code lexbuf 2);
        string st nl lexbuf }
  | newline
      { match st.comment_start_loc with
        | [] -> raise @@ Newline_in_string (pos2 (curr_location lexbuf))
        | _  -> Lexing.new_line lexbuf; string st (nl+1) lexbuf }
  | eof
      { raise @@ Unterminated_string (pos2 st.string_start_loc) }
  | _
      { store_string_char st (Lexing.lexeme_char lexbuf 0);
        string st nl lexbuf }


{

  let rec token st lexbuf =
    match st.buffer with
    | tok :: tokens ->
        st.buffer <- tokens;
        tok
    | [] ->
        match indent_tokens st 0 lexbuf with
        | [] -> raw_token st lexbuf
        | _ :: _ as tokens -> st.buffer <- tokens; token st lexbuf

}
