(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Script_located_ir

exception Invalid_utf8_sequence of point * string
exception Unexpected_character of point * string
exception Undefined_escape_character of point * string
exception Missing_break_after_number of point
exception Unterminated_string of location
exception Unterminated_integer of location
exception Unterminated_comment of location

type token_value =
  | String of string
  | Int of string
  | Ident of string
  | Annot of string
  | Comment of string
  | Eol_comment of string
  | Semi
  | Open_paren | Close_paren
  | Open_brace | Close_brace

type token =
  { token : token_value ;
    loc : location }

let tokenize source =
  let decoder = Uutf.decoder ~encoding:`UTF_8 (`String source) in
  let here () =
    { point = Uutf.decoder_count decoder ;
      byte = Uutf.decoder_byte_count decoder ;
      line = Uutf.decoder_line decoder ;
      column = Uutf.decoder_col decoder } in
  let tok start stop token =
    { loc = { start ; stop } ; token } in
  let stack = ref [] in
  let next () =
    match !stack with
    | charloc :: charlocs ->
        stack := charlocs ;
        charloc
    | [] ->
        let loc = here () in
        match Uutf.decode decoder with
        | `Await -> assert false
        | `Malformed s -> raise (Invalid_utf8_sequence (loc, s))
        | `Uchar _ | `End as other -> other, loc in
  let back charloc =
    stack := charloc :: !stack in
  let uchar_to_char c =
    if Uchar.is_char c then
      Some (Uchar.to_char c)
    else
      None in
  let rec skip acc =
    match next () with
    | `End, _ -> List.rev acc
    | `Uchar c, start ->
        begin match uchar_to_char c with
          | Some ('a'..'z' | 'A'..'Z') -> ident acc start (fun s -> Ident s)
          | Some '@' -> ident acc start (fun s -> Annot s)
          | Some '-' ->
              begin match next () with
                | `End, stop ->
                    raise (Unterminated_integer { start ; stop })
                | `Uchar c, stop ->
                    begin match uchar_to_char c with
                      | Some '0' -> base acc start
                      | Some ('1'..'9') -> integer `dec acc start false
                      | Some _ | None ->
                          raise (Unterminated_integer { start ; stop })
                    end
              end
          | Some '0' -> base acc start
          | Some ('1'..'9') -> integer `dec acc start false
          | Some (' ' | '\n') -> skip acc
          | Some ';' -> skip (tok start (here ()) Semi :: acc)
          | Some '{' -> skip (tok start (here ()) Open_brace :: acc)
          | Some '}' -> skip (tok start (here ()) Close_brace :: acc)
          | Some '(' -> skip (tok start (here ()) Open_paren :: acc)
          | Some ')' -> skip (tok start (here ()) Close_paren :: acc)
          | Some '"' -> string acc [] start
          | Some '#' -> eol_comment acc start
          | Some '/' ->
              begin match next () with
                | `Uchar c, _ when Uchar.equal c (Uchar.of_char '*') ->
                    comment acc start 0
                | (`Uchar _ | `End), _ ->
                    raise (Unexpected_character (start, "/"))
              end
          | Some _ | None ->
              let byte = Uutf.decoder_byte_count decoder in
              let s = String.sub source start.byte (byte - start.byte) in
              raise (Unexpected_character (start, s))
        end
  and base acc start =
    match next () with
    | (`Uchar c, stop) as charloc ->
        begin match uchar_to_char c with
          | Some ('0'.. '9') -> integer `dec acc start false
          | Some 'x' -> integer `hex acc start true
          | Some 'b' -> integer `bin acc start true
          | Some ('a' | 'c'..'w' | 'y' | 'z' | 'A'..'Z') ->
              raise (Missing_break_after_number stop)
          | Some _ | None ->
              back charloc ;
              skip (tok start (here ()) (Int "0") :: acc)
        end
    | (_, stop) as other ->
        back other ;
        skip (tok start stop (Int "0") :: acc)
  and integer base acc start first =
    let tok stop =
      let value =
        String.sub source start.byte (stop.byte - start.byte) in
      tok start stop (Int value) in
    match next () with
    | (`Uchar c, stop) as charloc ->
        begin match base, Uchar.to_char c with
          | `dec, ('0'.. '9') ->
              integer `dec acc start false
          | `dec, ('a'..'z' | 'A'..'Z') ->
              raise (Missing_break_after_number stop)
          | `hex, ('0'..'9' | 'a'..'f' | 'A'..'F') ->
              integer `hex acc start false
          | `hex, ('g'..'z' | 'G'..'Z') ->
              raise (Missing_break_after_number stop)
          | `bin, ('0' | '1') ->
              integer `bin acc start false
          | `bin, ('2'..'9' | 'a'..'z' | 'A'..'Z') ->
              raise (Missing_break_after_number stop)
          | (`bin | `hex), _ when first ->
              raise (Unterminated_integer { start ; stop })
          | _ ->
              back charloc ;
              skip (tok stop :: acc)
        end
    | (`End, stop) as other ->
        if first && base = `bin || base = `hex then
          raise (Unterminated_integer { start ; stop }) ;
        back other ;
        skip (tok stop :: acc)
  and string acc sacc start =
    let tok () =
      tok start (here ()) (String (String.concat "" (List.rev sacc))) in
    match next () with
    | `End, stop -> raise (Unterminated_string { start ; stop })
    | `Uchar c, stop ->
        match uchar_to_char c with
        | Some '"' -> skip (tok () :: acc)
        | Some '\n' -> raise (Unterminated_string { start ; stop })
        | Some '\\' ->
            begin match next () with
              | `End, stop -> raise (Unterminated_string { start ; stop })
              | `Uchar c, loc ->
                  match uchar_to_char c with
                  | Some '"' -> string acc ("\"" :: sacc) start
                  | Some 'r' -> string acc ("\r" :: sacc) start
                  | Some 'n' -> string acc ("\n" :: sacc) start
                  | Some 't' -> string acc ("\t" :: sacc) start
                  | Some 'b' -> string acc ("\b" :: sacc) start
                  | Some '\\' -> string acc ("\\" :: sacc) start
                  | Some _ | None ->
                      let byte = Uutf.decoder_byte_count decoder in
                      let s = String.sub source loc.byte (byte - loc.byte) in
                      raise (Undefined_escape_character (loc, s))
            end
        | Some _ | None ->
            let byte = Uutf.decoder_byte_count decoder in
            let s = String.sub source stop.byte (byte - stop.byte) in
            string acc (s :: sacc) start
  and ident acc start ret =
    let tok stop =
      let name =
        String.sub source start.byte (stop.byte - start.byte) in
      tok start stop (ret name) in
    match next () with
    | (`Uchar c, stop) as charloc ->
        begin match uchar_to_char c with
          | Some ('a'..'z' | 'A'..'Z' | '_' | '0'..'9') ->
              ident acc start ret
          | Some _ | None ->
              back charloc ;
              skip (tok stop :: acc)
        end
    | (_, stop) as other ->
        back other ;
        skip (tok stop :: acc)
  and comment acc start lvl =
    match next () with
    | `End, stop -> raise (Unterminated_comment { start ; stop })
    | `Uchar c, _ ->
        begin match uchar_to_char c with
          | Some '*' ->
              begin match next () with
                | `Uchar c, _ when Uchar.equal c (Uchar.of_char '/') ->
                    if lvl = 0 then
                      let stop = here () in
                      let text =
                        String.sub source start.byte (stop.byte - start.byte) in
                      skip (tok start stop (Comment text) :: acc)
                    else
                      comment acc start (lvl - 1)
                | other ->
                    back other ;
                    comment acc start lvl
              end
          | Some '/' ->
              begin match next () with
                | `Uchar c, _ when Uchar.equal c (Uchar.of_char '*') ->
                    comment acc start (lvl + 1)
                | other ->
                    back other ;
                    comment acc start lvl
              end
          | Some _ | None -> comment acc start lvl
        end
  and eol_comment acc start =
    let tok stop =
      let text = String.sub source start.byte (stop.byte - start.byte) in
      tok start stop (Eol_comment text) in
    match next () with
    | `Uchar c, stop ->
        begin match uchar_to_char c with
          | Some '\n' -> skip (tok stop :: acc)
          | Some _ | None -> eol_comment acc start
        end
    | (_, stop) as other ->
        back other ;
        skip (tok stop :: acc) in
  skip []

(* Beginning of a sequence of consecutive primitives *)
let min_point : node list -> point = function
  | [] -> point_zero
  | Int ({ start }, _) :: _
  | String ({ start }, _) :: _
  | Prim ({ start }, _, _, _) :: _
  | Seq ({ start }, _, _) :: _ -> start

(* End of a sequence of consecutive primitives *)
let rec max_point : node list -> point  = function
  | [] -> point_zero
  | _ :: (_ :: _ as rest) -> max_point rest
  | Int ({ stop }, _) :: []
  | String ({ stop }, _) :: []
  | Prim ({ stop }, _, _, _) :: []
  | Seq ({ stop }, _, _) :: [] -> stop

(* An item in the parser's state stack.
   Not every value of type [mode list] is a valid parsing context.
   It must respect the following additional invariants.
   - a state stack always ends in [Toplevel _],
   - [Toplevel _] does not appear anywhere else,
   - [Unwrapped _] cannot appear directly on top of [Wrapped _],
   - [Wrapped _] cannot appear directly on top of [Sequence _],
   - [Wrapped _] cannot appear directly on top of [Sequence _]. *)
type mode =
  | Toplevel of node list
  | Expression of node option
  | Sequence of token * node list * string option
  | Unwrapped of location * string * node list * string option
  | Wrapped of token * string * node list * string option

(* Enter a new parsing state. *)
let push_mode mode stack =
  mode :: stack

(* Leave a parsing state. *)
let pop_mode = function
  | [] -> assert false
  | _ :: rest -> rest

(* Usually after a [pop_mode], jump back into the previous parsing
   state, injecting the current reduction (insert the just parsed item
   of a sequence or argument of a primitive application). *)
let fill_mode result = function
  | [] -> assert false
  | Expression _ :: _ :: _ -> assert false
  | Expression (Some _) :: [] -> assert false
  | Toplevel _ :: _ :: _ -> assert false
  | Expression None :: []  ->
      Expression (Some result) :: []
  | Toplevel exprs :: [] ->
      Toplevel (result :: exprs) :: []
  | Sequence (token, exprs, annot) :: rest ->
      Sequence (token, result :: exprs, annot) :: rest
  | Wrapped (token, name, exprs, annot) :: rest ->
      Wrapped (token, name, result :: exprs, annot) :: rest
  | Unwrapped (start, name, exprs, annot) :: rest ->
      Unwrapped (start, name, result :: exprs, annot) :: rest

exception Unclosed of token
exception Unexpected of token
exception Extra of token
exception Misaligned of node
exception Empty

let rec parse
    ?expand:(do_expand = true)
    ?check:(do_check = true)
    tokens stack =
  (* Two steps:
     - 1. parse without checking indentation [parse]
     - 2. check indentation [check] (inlined in 1)
     - 3. expand macros (inlined in 1, after 2) *)
  match stack, tokens with
  (* Start by preventing all absurd cases, so now the pattern
     matching exhaustivity can tell us that we treater all
     possible tokens for all possible valid states. *)
  | [], _
  | [ Wrapped _ ], _
  | [ Unwrapped _ ], _
  | Unwrapped _ :: Unwrapped _ :: _, _
  | Unwrapped _ :: Wrapped _ :: _, _
  | Toplevel _ :: _ :: _, _
  | Expression _ :: _ :: _, _ ->
      assert false
  (* Return *)
  | Expression (Some result) :: _, [] ->
      [ result ]
  | Expression (Some _) :: _, token :: _ ->
      raise (Unexpected token)
  | Expression None :: _, [] ->
      raise Empty
  | Toplevel [ Seq (_, exprs, _) as expr ] :: [],
    [] ->
      if do_check then check ~toplevel: false expr ;
      let exprs =
        if do_expand then
          List.map Michelson_macros.expand exprs
        else exprs in
      exprs
  | Toplevel exprs :: [],
    [] ->
      let exprs = List.rev exprs in
      let loc = { start = min_point exprs ; stop = max_point exprs } in
      let expr = Seq (loc, exprs, None) in
      if do_check then check ~toplevel: true expr ;
      let exprs =
        if do_expand then
          List.map Michelson_macros.expand exprs
        else exprs in
      exprs
  (* Ignore comments *)
  | _,
    { token = Eol_comment _ | Comment _ } :: rest ->
      parse rest stack
  | (Wrapped _ | Unwrapped _) :: _,
    ({ token = Open_paren } as token)
    :: { token = Eol_comment _ | Comment _ } :: rest ->
      parse (token :: rest) stack
  (* Erroneous states *)
  | (Wrapped _ | Unwrapped _) :: _ ,
    ({ token = Open_paren } as token)
    :: { token = Open_paren | Open_brace } :: _
  | Unwrapped _ :: Expression _ :: _ ,
    ({ token = Semi | Close_brace | Close_paren } as token) :: _
  | Expression None :: _ ,
    ({ token = Semi | Close_brace | Close_paren | Open_brace | Open_paren } as token) :: _ ->
      raise (Unexpected token)
  | (Sequence _ | Toplevel _) :: _ ,
    { token = Semi } :: ({ token = Semi } as token) :: _ ->
      raise (Extra token)
  | (Wrapped _ | Unwrapped _) :: _ ,
    { token = Open_paren }
    :: ({ token = Int _ | String _ | Annot _ | Close_paren } as token)  :: _
  | Unwrapped (_, _, _, _) :: Toplevel _ :: _,
    ({ token = Close_brace } as token) :: _
  | Unwrapped (_, _, _, _) :: _,
    ({ token = Close_paren } as token) :: _
  | Toplevel _ :: [],
    ({ token = Close_paren } as token) :: _
  | Toplevel _ :: [],
    ({ token = Close_brace } as token) :: _
  | _,
    ({ token = Annot _ } as token) :: _ ->
      raise (Unexpected token)
  | Wrapped (token, _, _, _) :: _,
    ({ token = Close_brace | Semi } :: _ | [])
  | (Sequence _ | Toplevel _) :: _,
    ({ token = Open_paren } as token) :: _
  | (Wrapped _ | Unwrapped _) :: _,
    ({ token = Open_paren } as token) :: ({ token = Close_brace | Semi } :: _ | [])
  | (Sequence (token, _, _) :: _ | Unwrapped _ :: Sequence (token, _, _) :: _),
    ({ token = Close_paren } :: _ | [])->
      raise (Unclosed token)
  (* Valid states *)
  | (Toplevel _ | Sequence (_, _, _)) :: _ ,
    { token = Ident name ; loc } :: { token = Annot annot } :: rest ->
      let mode = Unwrapped (loc, name, [], Some annot) in
      parse rest (push_mode mode stack)
  | (Expression None | Toplevel _ | Sequence (_, _, _)) :: _ ,
    { token = Ident name ; loc } :: rest ->
      let mode = Unwrapped (loc, name, [], None) in
      parse rest (push_mode mode stack)
  | (Expression None | Sequence _ | Toplevel _ | Unwrapped _ | Wrapped _) :: _,
    { token = Int value ; loc } :: rest ->
      let expr : node = Int (loc, value) in
      if do_check then check ~toplevel: false expr ;
      let expr =
        if do_expand then
          Michelson_macros.expand expr
        else expr in
      parse rest (fill_mode expr stack)
  | (Expression None | Sequence _ | Toplevel _ | Unwrapped _ | Wrapped _) :: _,
    { token = String contents ; loc } :: rest ->
      let expr : node = String (loc, contents) in
      if do_check then check ~toplevel: false expr ;
      let expr =
        if do_expand then
          Michelson_macros.expand expr
        else expr in
      parse rest (fill_mode expr stack)
  | Sequence ({ loc = { start } }, exprs, annot) :: _ ,
    { token = Close_brace ; loc = { stop } } :: rest ->
      let exprs = List.rev exprs in
      let expr = Seq ({ start ; stop }, exprs, annot) in
      if do_check then check ~toplevel: false expr ;
      let expr =
        if do_expand then
          Michelson_macros.expand expr
        else expr in
      parse rest (fill_mode expr (pop_mode stack))
  | (Sequence _ | Toplevel _) :: _ ,
    { token = Semi } :: rest ->
      parse rest stack
  | Unwrapped ({ start ; stop }, name, exprs, annot) :: Expression _ :: _,
    ([] as rest)
  | Unwrapped ({ start ; stop }, name, exprs, annot) :: Toplevel _ :: _,
    ({ token = Semi } :: _ | [] as rest)
  | Unwrapped ({ start ; stop }, name, exprs, annot) :: Sequence _ :: _ ,
    ({ token = Close_brace | Semi } :: _ as rest)
  | Wrapped ({ loc = { start ; stop } }, name, exprs, annot) :: _ ,
    { token = Close_paren } :: rest ->
      let exprs = List.rev exprs in
      let stop = if exprs = [] then stop else max_point exprs in
      let expr = Prim ({ start ; stop }, name, exprs, annot) in
      if do_check then check ~toplevel: false expr ;
      let expr =
        if do_expand then
          Michelson_macros.expand expr
        else expr in
      parse rest (fill_mode expr (pop_mode stack))
  | (Wrapped _ | Unwrapped _) :: _ ,
    ({ token = Open_paren } as token) :: { token = Ident name } :: { token = Annot annot } :: rest ->
      let mode = Wrapped (token, name, [], Some annot) in
      parse rest (push_mode mode stack)
  | (Wrapped _ | Unwrapped _) :: _ ,
    ({ token = Open_paren } as token) :: { token = Ident name } :: rest ->
      let mode = Wrapped (token, name, [], None) in
      parse rest (push_mode mode stack)
  | (Wrapped _ | Unwrapped _) :: _ ,
    { token = Ident name ; loc } :: rest ->
      let expr = Prim (loc, name, [], None) in
      if do_check then check ~toplevel: false expr ;
      let expr =
        if do_expand then
          Michelson_macros.expand expr
        else expr in
      parse rest (fill_mode expr stack)
  | (Wrapped _ | Unwrapped _ | Toplevel _ | Sequence _) :: _ ,
    ({ token = Open_brace } as token) :: { token = Annot annot } :: rest ->
      let mode = Sequence (token, [], Some annot) in
      parse rest (push_mode mode stack)
  | (Wrapped _ | Unwrapped _ | Toplevel _ | Sequence _) :: _ ,
    ({ token = Open_brace } as token) :: rest ->
      let mode = Sequence (token, [], None) in
      parse rest (push_mode mode stack)
(* indentation checker *)
and check ?(toplevel = false) = function
  | Seq ({ start ; stop }, [], _) as expr ->
      if start.column >= stop.column then
        raise (Misaligned expr)
  | Prim ({ start ; stop }, _, first :: rest, _)
  | Seq ({ start ; stop }, first :: rest, _) as expr ->
      let { column = first_column ; line = first_line } =
        min_point [ first ] in
      if start.column >= stop.column then
        raise (Misaligned expr) ;
      if not toplevel && start.column >= first_column then
        raise (Misaligned expr) ;
      (* In a sequence or in the arguments of a primitive, we
         require all items to be aligned, but we relax the rule to
         allow consecutive items to be writtem on the same line. *)
      let rec in_line_or_aligned prev_start_line = function
        | [] -> ()
        | expr :: rest ->
            let { column ; line = start_line } = min_point [ expr ] in
            let { line = stop_line } = max_point [ expr ] in
            if stop_line <> prev_start_line
            && column <> first_column then
              raise (Misaligned expr) ;
            in_line_or_aligned start_line rest in
      in_line_or_aligned first_line rest
  | Prim (_, _, [], _) | String _ | Int _ -> ()

let parse_expression ?expand ?check tokens =
  let result = match tokens with
    | ({ token = Open_paren } as token) :: { token = Ident name } :: { token = Annot annot } :: rest ->
        let mode = Wrapped (token, name, [], Some annot) in
        parse ?expand ?check rest [ mode ; Expression None ]
    | ({ token = Open_paren } as token) :: { token = Ident name } :: rest ->
        let mode = Wrapped (token, name, [], None) in
        parse ?expand ?check rest [ mode ; Expression None ]
    | _ ->
        parse ?expand ?check tokens [ Expression None ] in
  match result with
  | [ single ] -> single
  | _ -> assert false

let parse_toplevel ?expand ?check tokens =
  parse ?expand ?check tokens [ Toplevel [] ]
