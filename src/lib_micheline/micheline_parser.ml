(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2018 Nomadic Labs. <nomadic@tezcore.com>                    *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

open Error_monad
open Micheline

type 'a parsing_result = 'a * error list

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

let point_encoding =
  let open Data_encoding in
  conv
    (fun { line ; column ; point ; byte } -> (line, column, point, byte))
    (fun (line, column, point, byte) -> { line ; column ; point ; byte })
    (obj4
       (req "line" uint16)
       (req "column" uint16)
       (req "point" uint16)
       (req "byte" uint16))

type location =
  { start : point ;
    stop : point }

let location_zero =
  { start = point_zero ;
    stop = point_zero }

let location_encoding =
  let open Data_encoding in
  conv
    (fun { start ; stop } -> (start, stop))
    (fun (start, stop) -> { start ; stop })
    (obj2
       (req "start" point_encoding)
       (req "stop" point_encoding))

type token_value =
  | String of string
  | Bytes of string
  | Int of string
  | Ident of string
  | Annot of string
  | Comment of string
  | Eol_comment of string
  | Semi
  | Open_paren | Close_paren
  | Open_brace | Close_brace

let token_value_encoding =
  let open Data_encoding in
  union
    [ case (Tag 0)
        ~title:"String"
        (obj1 (req "string" string))
        (function String s -> Some s | _ -> None)
        (fun s -> String s) ;
      case (Tag 1)
        ~title:"Int"
        (obj1 (req "int" string))
        (function Int s -> Some s | _ -> None)
        (fun s -> Int s) ;
      case (Tag 2)
        ~title:"Annot"
        (obj1 (req "annot" string))
        (function Annot s -> Some s | _ -> None)
        (fun s -> Annot s) ;
      case (Tag 3)
        ~title:"Comment"
        (obj2 (req "comment" string) (dft "end_of_line" bool false))
        (function
          | Comment s -> Some (s, false)
          | Eol_comment s -> Some (s, true) | _ -> None)
        (function
          | (s, false) -> Comment s
          | (s, true) -> Eol_comment s) ;
      case (Tag 4)
        ~title:"Punctuation"
        (obj1 (req "punctuation" (string_enum [
             "(", Open_paren ;
             ")", Close_paren ;
             "{", Open_brace ;
             "}", Close_brace ;
             ";", Semi ])))
        (fun t -> Some t) (fun t -> t) ;
      case (Tag 5)
        ~title:"Bytes"
        (obj1 (req "bytes" string))
        (function Bytes s -> Some s | _ -> None)
        (fun s -> Bytes s) ]

type token =
  { token : token_value ;
    loc : location }

let max_annot_length = 255

type error += Invalid_utf8_sequence of point * string
type error += Unexpected_character of point * string
type error += Undefined_escape_sequence of point * string
type error += Missing_break_after_number of point
type error += Unterminated_string of location
type error += Unterminated_integer of location
type error += Odd_lengthed_bytes of location
type error += Unterminated_comment of location
type error += Annotation_length of location

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
  let errors = ref [] in
  let rec next () =
    match !stack with
    | charloc :: charlocs ->
        stack := charlocs ;
        charloc
    | [] ->
        let loc = here () in
        match Uutf.decode decoder with
        | `Await -> assert false
        | `Malformed s ->
            errors := Invalid_utf8_sequence (loc, s) :: !errors ;
            next ()
        | `Uchar _ | `End as other -> other, loc in
  let back charloc =
    stack := charloc :: !stack in
  let uchar_to_char c =
    if Uchar.is_char c then
      Some (Uchar.to_char c)
    else
      None in
  let allowed_ident_char c =
    match uchar_to_char c with
    | Some ('a'..'z' | 'A'..'Z' | '_' | '0'..'9') -> true
    | Some _ | None -> false in
  let allowed_annot_char c =
    match uchar_to_char c with
    | Some ('a'..'z' | 'A'..'Z' | '_' | '.' | '%' | '@' | '0'..'9') -> true
    | Some _ | None -> false in
  let rec skip acc =
    match next () with
    | `End, _ -> List.rev acc
    | `Uchar c, start ->
        begin match uchar_to_char c with
          | Some ('a'..'z' | 'A'..'Z') -> ident acc start (fun s _ -> Ident s)
          | Some ('@' | ':' | '$' | '&' | '%' | '!' | '?') ->
              annot acc start
                (fun str stop ->
                   if String.length str > max_annot_length
                   then errors := (Annotation_length { start ; stop }) :: !errors ;
                   Annot str)
          | Some '-' ->
              begin match next () with
                | `End, stop ->
                    errors := Unterminated_integer { start ; stop } :: !errors ;
                    List.rev acc
                | `Uchar c, stop as first ->
                    begin match uchar_to_char c with
                      | Some '0' -> base acc start
                      | Some ('1'..'9') -> integer acc start
                      | Some _ | None ->
                          errors := Unterminated_integer { start ; stop } :: !errors ;
                          back first ;
                          skip acc
                    end
              end
          | Some '0' -> base acc start
          | Some ('1'..'9') -> integer acc start
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
                | (`Uchar _ | `End), _ as charloc ->
                    errors := Unexpected_character (start, "/") :: !errors ;
                    back charloc ;
                    skip acc
              end
          | Some _ | None ->
              let byte = Uutf.decoder_byte_count decoder in
              let s = String.sub source start.byte (byte - start.byte) in
              errors := Unexpected_character (start, s) :: !errors ;
              skip acc
        end
  and base acc start =
    match next () with
    | (`Uchar c, stop) as charloc ->
        begin match uchar_to_char c with
          | Some ('0'.. '9') -> integer acc start
          | Some 'x' -> bytes acc start
          | Some ('a'..'w' | 'y' | 'z' | 'A'..'Z') ->
              errors := Missing_break_after_number stop :: !errors ;
              back charloc ;
              skip (tok start stop (Int "0") :: acc)
          | Some _ | None ->
              back charloc ;
              skip (tok start stop (Int "0") :: acc)
        end
    | (_, stop) as other ->
        back other ;
        skip (tok start stop (Int "0") :: acc)
  and integer acc start =
    let tok stop =
      let value =
        String.sub source start.byte (stop.byte - start.byte) in
      tok start stop (Int value) in
    match next () with
    | (`Uchar c, stop) as charloc ->
        let missing_break () =
          errors := Missing_break_after_number stop :: !errors ;
          back charloc ;
          skip (tok stop :: acc) in
        begin match Uchar.to_char c with
          | ('0'.. '9') ->
              integer acc start
          | ('a'..'z' | 'A'..'Z') ->
              missing_break ()
          | _ ->
              back charloc ;
              skip (tok stop :: acc)
        end
    | (`End, stop) as other ->
        back other ;
        skip (tok stop :: acc)
  and bytes acc start =
    let tok stop =
      let value =
        String.sub source start.byte (stop.byte - start.byte) in
      tok start stop (Bytes value) in
    match next () with
    | (`Uchar c, stop) as charloc ->
        let missing_break () =
          errors := Missing_break_after_number stop :: !errors ;
          back charloc ;
          skip (tok stop :: acc) in
        begin match Uchar.to_char c with
          | ('0'..'9' | 'a'..'f' | 'A'..'F') ->
              bytes acc start
          | ('g'..'z' | 'G'..'Z') ->
              missing_break ()
          | _ ->
              back charloc ;
              skip (tok stop :: acc)
        end
    | (`End, stop) as other ->
        back other ;
        skip (tok stop :: acc)
  and string acc sacc start =
    let tok () =
      tok start (here ()) (String (String.concat "" (List.rev sacc))) in
    match next () with
    | `End, stop ->
        errors := Unterminated_string { start ; stop } :: !errors ;
        skip (tok () :: acc)
    | `Uchar c, stop ->
        match uchar_to_char c with
        | Some '"' -> skip (tok () :: acc)
        | Some ('\n' | '\r') ->
            errors := Unterminated_string { start ; stop } :: !errors ;
            skip (tok () :: acc)
        | Some '\\' ->
            begin match next () with
              | `End, stop ->
                  errors := Unterminated_string { start ; stop } :: !errors ;
                  skip (tok () :: acc)
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
                      errors := Undefined_escape_sequence (loc, s) :: !errors ;
                      string acc sacc start
            end
        | Some _ | None ->
            let byte = Uutf.decoder_byte_count decoder in
            let s = String.sub source stop.byte (byte - stop.byte) in
            string acc (s :: sacc) start
  and generic_ident allow_char acc start (ret : string -> point -> token_value) =
    let tok stop =
      let name =
        String.sub source start.byte (stop.byte - start.byte) in
      tok start stop (ret name stop) in
    match next () with
    | (`Uchar c, stop) as charloc ->
        if allow_char c then
          generic_ident allow_char acc start ret
        else begin
          back charloc ;
          skip (tok stop :: acc)
        end
    | (_, stop) as other ->
        back other ;
        skip (tok stop :: acc)
  and ident acc start ret = generic_ident allowed_ident_char acc start ret
  and annot acc start ret = generic_ident allowed_annot_char acc start ret
  and comment acc start lvl =
    match next () with
    | `End, stop ->
        errors := Unterminated_comment { start ; stop } :: !errors ;
        let text = String.sub source start.byte (stop.byte - start.byte) in
        skip (tok start stop (Comment text) :: acc)
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
  let tokens = skip [] in
  tokens, List.rev !errors

type node = (location, string) Micheline.node

let node_encoding =
  Micheline.table_encoding ~variant:"generic" location_encoding Data_encoding.string

(* Beginning of a sequence of consecutive primitives *)
let min_point : node list -> point = function
  | [] -> point_zero
  | Int ({ start ; _ }, _) :: _
  | String ({ start ; _ }, _) :: _
  | Bytes ({ start ; _ }, _) :: _
  | Prim ({ start ; _ }, _, _, _) :: _
  | Seq ({ start ; _ }, _) :: _ -> start

(* End of a sequence of consecutive primitives *)
let rec max_point : node list -> point  = function
  | [] -> point_zero
  | _ :: (_ :: _ as rest) -> max_point rest
  | Int ({ stop ; _ }, _) :: []
  | String ({ stop ; _ }, _) :: []
  | Bytes ({ stop ; _ }, _) :: []
  | Prim ({ stop ; _ }, _, _, _) :: []
  | Seq ({ stop ; _ }, _) :: [] -> stop

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
  | Sequence of token * node list
  | Unwrapped of location * string * node list * string list
  | Wrapped of token * string * node list * string list

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
  | Sequence (token, exprs) :: rest ->
      Sequence (token, result :: exprs) :: rest
  | Wrapped (token, name, exprs, annot) :: rest ->
      Wrapped (token, name, result :: exprs, annot) :: rest
  | Unwrapped (start, name, exprs, annot) :: rest ->
      Unwrapped (start, name, result :: exprs, annot) :: rest

type error += Unclosed of token
type error += Unexpected of token
type error += Extra of token
type error += Misaligned of node
type error += Empty

let rec annots = function
  | { token = Annot annot ; _ } :: rest ->
      let annots, rest = annots rest in
      annot :: annots, rest
  | rest -> [], rest

let rec parse ?(check = true) errors tokens stack =
  (* Two steps:
     - 1. parse without checking indentation [parse]
     - 2. check indentation [check] (inlined in 1) *)
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
      [ result ], List.rev errors
  | Expression (Some _) :: _, token :: rem ->
      let errors = Unexpected token :: errors in
      parse ~check errors rem (* skip *) stack
  | Expression None :: _, [] ->
      let errors = Empty :: errors in
      let ghost = { start = point_zero ; stop = point_zero} in
      [ Seq (ghost, []) ], List.rev errors
  | Toplevel [ Seq (_, exprs) as expr ] :: [],
    [] ->
      let errors = if check then do_check ~toplevel: false errors expr else errors in
      exprs, List.rev errors
  | Toplevel exprs :: [],
    [] ->
      let exprs = List.rev exprs in
      let loc = { start = min_point exprs ; stop = max_point exprs } in
      let expr = Seq (loc, exprs) in
      let errors = if check then do_check ~toplevel: true errors expr else errors in
      exprs, List.rev errors
  (* Ignore comments *)
  | _,
    { token = Eol_comment _ | Comment _ ; _ } :: rest ->
      parse ~check errors rest stack
  | (Expression None | Sequence _ | Toplevel _) :: _,
    ({ token = Int _ | String _ | Bytes _ ; _ } as token):: { token = Eol_comment _ | Comment _ ; _ } :: rest
  | (Wrapped _ | Unwrapped _) :: _,
    ({ token = Open_paren ; _ } as token)
    :: { token = Eol_comment _ | Comment _ ; _ } :: rest ->
      parse ~check errors (token :: rest) stack
  (* Erroneous states *)
  | (Wrapped _ | Unwrapped _) :: _ ,
    ({ token = Open_paren ; _ } as token)
    :: { token = Open_paren | Open_brace ; _ } :: rem
  | Unwrapped _ :: Expression _ :: _ ,
    ({ token = Semi | Close_brace | Close_paren ; _ } as token) :: rem
  | Expression None :: _ ,
    ({ token = Semi | Close_brace | Close_paren | Open_paren ; _ } as token) :: rem ->
      let errors = Unexpected token :: errors in
      parse ~check errors rem (* skip *) stack
  | (Sequence _ | Toplevel _) :: _ ,
    ({ token = Semi ; _ } as valid) :: ({ token = Semi ; _ } as token) :: rem ->
      let errors = Extra token :: errors in
      parse ~check errors (valid (* skip *) :: rem) stack
  | (Wrapped _ | Unwrapped _) :: _ ,
    { token = Open_paren ; _ }
    :: ({ token = Int _ | String _ | Bytes _ | Annot _ | Close_paren ; _ } as token) :: rem
  | (Expression None | Sequence _ | Toplevel _) :: _,
    { token = Int _ | String _ | Bytes _ ; _ } :: ({ token = Ident _ | Int _ | String _ | Bytes _ | Annot _ | Close_paren | Open_paren | Open_brace ; _ } as token) :: rem
  | Unwrapped (_, _, _, _) :: Toplevel _ :: _,
    ({ token = Close_brace ; _ } as token) :: rem
  | Unwrapped (_, _, _, _) :: _,
    ({ token = Close_paren ; _ } as token) :: rem
  | Toplevel _ :: [],
    ({ token = Close_paren ; _ } as token) :: rem
  | Toplevel _ :: [],
    ({ token = Open_paren ; _ } as token) :: rem
  | Toplevel _ :: [],
    ({ token = Close_brace ; _ } as token) :: rem
  | Sequence _ :: _,
    ({ token = Open_paren ; _ } as token) :: rem
  | Sequence _ :: _,
    ({ token = Close_paren ; _ } as token :: rem)
  | (Wrapped _ | Unwrapped _) :: _,
    ({ token = Open_paren ; _ } as token) :: ({ token = Close_brace | Semi ; _ } :: _ | [] as rem)
  | _,
    ({ token = Annot _ ; _ } as token) :: rem ->
      let errors = Unexpected token :: errors in
      parse ~check errors rem (* skip *) stack
  | Wrapped (token, _, _, _) :: _, ([] | { token = Close_brace | Semi ; _ } :: _) ->
      let errors = Unclosed token :: errors in
      let fake = { token with token = Close_paren } in
      let tokens = (* insert *) fake :: tokens  in
      parse ~check errors tokens stack
  | (Sequence (token, _) :: _ | Unwrapped _ :: Sequence (token, _) :: _), [] ->
      let errors = Unclosed token :: errors in
      let fake = { token with token = Close_brace } in
      let tokens = (* insert *) fake :: tokens  in
      parse ~check errors tokens stack
  (* Valid states *)
  | (Toplevel _ | Sequence (_, _)) :: _ ,
    { token = Ident name ; loc } :: ({ token = Annot _ ; _ } :: _ as rest) ->
      let annots, rest = annots rest in
      let mode = Unwrapped (loc, name, [], annots) in
      parse ~check errors rest (push_mode mode stack)
  | (Expression None | Toplevel _ | Sequence (_, _)) :: _ ,
    { token = Ident name ; loc } :: rest ->
      let mode = Unwrapped (loc, name, [], []) in
      parse ~check errors rest (push_mode mode stack)
  | (Unwrapped _ | Wrapped _) :: _,
    { token = Int value ; loc } :: rest
  | (Expression None | Sequence _ | Toplevel _) :: _,
    { token = Int value ; loc } :: ([] | { token = Semi | Close_brace; _ } :: _ as rest) ->
      let expr : node = Int (loc, Z.of_string value) in
      let errors = if check then do_check ~toplevel: false errors expr else errors in
      parse ~check errors rest (fill_mode expr stack)
  | (Unwrapped _ | Wrapped _) :: _,
    { token = String contents ; loc } :: rest
  | (Expression None | Sequence _ | Toplevel _) :: _,
    { token = String contents ; loc } :: ([] | { token = Semi | Close_brace; _ } :: _ as rest) ->
      let expr : node = String (loc, contents) in
      let errors = if check then do_check ~toplevel: false errors expr else errors in
      parse ~check errors rest (fill_mode expr stack)
  | (Unwrapped _ | Wrapped _) :: _,
    { token = Bytes contents ; loc } :: rest
  | (Expression None | Sequence _ | Toplevel _) :: _,
    { token = Bytes contents ; loc } :: ([] | { token = Semi | Close_brace; _ } :: _ as rest) ->
      let errors, contents = if String.length contents mod 2 <> 0 then
          Odd_lengthed_bytes loc :: errors, contents ^ "0"
        else errors, contents in
      let bytes =
        MBytes.of_hex (`Hex (String.sub contents 2 (String.length contents - 2))) in
      let expr : node = Bytes (loc, bytes) in
      let errors = if check then do_check ~toplevel: false errors expr else errors in
      parse ~check errors rest (fill_mode expr stack)
  | Sequence ({ loc = { start ; _ } ; _ }, exprs) :: _ ,
    { token = Close_brace ; loc = { stop ; _ } } :: rest ->
      let exprs = List.rev exprs in
      let expr = Micheline.Seq ({ start ; stop }, exprs) in
      let errors = if check then do_check ~toplevel: false errors expr else errors in
      parse ~check errors rest (fill_mode expr (pop_mode stack))
  | (Sequence _ | Toplevel _) :: _ ,
    { token = Semi ; _ } :: rest ->
      parse ~check errors rest stack
  | Unwrapped ({ start ; stop }, name, exprs, annot) :: Expression _ :: _,
    ([] as rest)
  | Unwrapped ({ start ; stop }, name, exprs, annot) :: Toplevel _ :: _,
    ({ token = Semi ; _ } :: _ | [] as rest)
  | Unwrapped ({ start ; stop }, name, exprs, annot) :: Sequence _ :: _ ,
    ({ token = Close_brace | Semi ; _ } :: _ as rest)
  | Wrapped ({ loc = { start ; stop } ; _ }, name, exprs, annot) :: _ ,
    { token = Close_paren ; _ } :: rest ->
      let exprs = List.rev exprs in
      let stop = if exprs = [] then stop else max_point exprs in
      let expr = Micheline.Prim ({ start ; stop }, name, exprs, annot) in
      let errors = if check then do_check ~toplevel: false errors expr else errors in
      parse ~check errors rest (fill_mode expr (pop_mode stack))
  | (Wrapped _ | Unwrapped _) :: _ ,
    ({ token = Open_paren ; _ } as token) :: { token = Ident name ; _ } :: ({ token = Annot _ ; _ } :: _ as rest) ->
      let annots, rest = annots rest in
      let mode = Wrapped (token, name, [], annots) in
      parse ~check errors rest (push_mode mode stack)
  | (Wrapped _ | Unwrapped _) :: _ ,
    ({ token = Open_paren ; _ } as token) :: { token = Ident name ; _ } :: rest ->
      let mode = Wrapped (token, name, [], []) in
      parse ~check errors rest (push_mode mode stack)
  | (Wrapped _ | Unwrapped _) :: _ ,
    { token = Ident name ; loc } :: rest ->
      let expr = Micheline.Prim (loc, name, [], []) in
      let errors = if check then do_check ~toplevel: false errors expr else errors in
      parse ~check errors rest (fill_mode expr stack)
  | (Wrapped _ | Unwrapped _ | Toplevel _ | Sequence _ | Expression None) :: _ ,
    ({ token = Open_brace ; _ } as token) :: rest ->
      let mode = Sequence (token, []) in
      parse ~check errors rest (push_mode mode stack)
(* indentation checker *)
and do_check ?(toplevel = false) errors = function
  | Seq ({ start ; stop }, []) as expr ->
      if start.column >= stop.column then
        Misaligned expr :: errors
      else errors
  | Prim ({ start ; stop }, _, first :: rest, _)
  | Seq ({ start ; stop }, first :: rest) as expr ->
      let { column = first_column ; line = first_line ; _ } =
        min_point [ first ] in
      if start.column >= stop.column then
        Misaligned expr :: errors
      else if not toplevel && start.column >= first_column then
        Misaligned expr :: errors
      else
        (* In a sequence or in the arguments of a primitive, we
           require all items to be aligned, but we relax the rule to
           allow consecutive items to be writtem on the same line. *)
        let rec in_line_or_aligned prev_start_line errors = function
          | [] -> errors
          | expr :: rest ->
              let { column ; line = start_line ; _ } = min_point [ expr ] in
              let { line = stop_line ; _ } = max_point [ expr ] in
              let errors =
                if stop_line <> prev_start_line
                && column <> first_column then
                  Misaligned expr :: errors
                else
                  errors in
              in_line_or_aligned start_line errors rest in
        in_line_or_aligned first_line errors rest
  | Prim (_, _, [], _) | String _ | Int _ | Bytes _ -> errors

let parse_expression ?check tokens =
  let result = match tokens with
    | ({ token = Open_paren ; _ } as token) :: { token = Ident name ; _ } :: { token = Annot annot ; _ } :: rest ->
        let annots, rest = annots rest in
        let mode = Wrapped (token, name, [], annot :: annots) in
        parse ?check [] rest [ mode ; Expression None ]
    | ({ token = Open_paren ; _ } as token) :: { token = Ident name ; _ } :: rest ->
        let mode = Wrapped (token, name, [], []) in
        parse ?check [] rest [ mode ; Expression None ]
    | _ ->
        parse ?check [] tokens [ Expression None ] in
  match result with
  | [ single ], errors -> single, errors
  | _ -> assert false

let parse_toplevel ?check tokens =
  parse ?check [] tokens [ Toplevel [] ]

let print_point ppf { line ; column ; _ } =
  Format.fprintf ppf
    "At line %d character %d"
    line column

let print_token_kind ppf = function
  | Open_paren | Close_paren -> Format.fprintf ppf "parenthesis"
  | Open_brace | Close_brace -> Format.fprintf ppf "curly brace"
  | String _ -> Format.fprintf ppf "string constant"
  | Bytes _ -> Format.fprintf ppf "bytes constant"
  | Int _ -> Format.fprintf ppf "integer constant"
  | Ident _ -> Format.fprintf ppf "identifier"
  | Annot _ -> Format.fprintf ppf "annotation"
  | Comment _ | Eol_comment _ -> Format.fprintf ppf "comment"
  | Semi -> Format.fprintf ppf "semi colon"

let print_location ppf loc =
  if loc.start.line = loc.stop.line then
    if loc.start.column = loc.stop.column then
      Format.fprintf ppf
        "At line %d character %d"
        loc.start.line loc.start.column
    else
      Format.fprintf ppf
        "At line %d characters %d to %d"
        loc.start.line loc.start.column loc.stop.column
  else
    Format.fprintf ppf
      "From line %d character %d to line %d character %d"
      loc.start.line loc.start.column loc.stop.line loc.stop.column

let no_parsing_error (ast, errors) =
  match errors with
  | [] -> ok ast
  | errors -> Error errors

let () =
  register_error_kind `Permanent
    ~id: "micheline.parse_error.invalid_utf8_sequence"
    ~title: "Micheline parser error: invalid UTF-8 sequence"
    ~description: "While parsing a piece of Micheline source, \
                   a sequence of bytes that is not valid UTF-8 \
                   was encountered."
    ~pp:(fun ppf (point, str) -> Format.fprintf ppf "%a, invalid UTF-8 sequence %S" print_point point str)
    Data_encoding.(obj2 (req "point" point_encoding) (req "sequence" string))
    (function Invalid_utf8_sequence (point, str) -> Some (point, str) | _ -> None)
    (fun (point, str) -> Invalid_utf8_sequence (point, str)) ;
  register_error_kind `Permanent
    ~id: "micheline.parse_error.unexpected_character"
    ~title: "Micheline parser error: unexpected character"
    ~description: "While parsing a piece of Micheline source, \
                   an unexpected character was encountered."
    ~pp:(fun ppf (point, str) -> Format.fprintf ppf "%a, unexpected character %s" print_point point str)
    Data_encoding.(obj2 (req "point" point_encoding) (req "character" string))
    (function Unexpected_character (point, str) -> Some (point, str) | _ -> None)
    (fun (point, str) -> Unexpected_character (point, str)) ;
  register_error_kind `Permanent
    ~id: "micheline.parse_error.undefined_escape_sequence"
    ~title: "Micheline parser error: undefined escape sequence"
    ~description: "While parsing a piece of Micheline source, \
                   an unexpected escape sequence was encountered in a string."
    ~pp:(fun ppf (point, str) -> Format.fprintf ppf "%a, undefined escape sequence \"%s\"" print_point point str)
    Data_encoding.(obj2 (req "point" point_encoding) (req "sequence" string))
    (function Undefined_escape_sequence (point, str) -> Some (point, str) | _ -> None)
    (fun (point, str) -> Undefined_escape_sequence (point, str)) ;
  register_error_kind `Permanent
    ~id: "micheline.parse_error.missing_break_after_number"
    ~title: "Micheline parser error: missing break after number"
    ~description: "While parsing a piece of Micheline source, \
                   a number was not visually separated from \
                   its follower token, leading to misreadability."
    ~pp:(fun ppf point -> Format.fprintf ppf "%a, missing break after number" print_point point)
    Data_encoding.(obj1 (req "point" point_encoding))
    (function Missing_break_after_number point -> Some point | _ -> None)
    (fun point -> Missing_break_after_number point) ;
  register_error_kind `Permanent
    ~id: "micheline.parse_error.unterminated_string"
    ~title: "Micheline parser error: unterminated string"
    ~description: "While parsing a piece of Micheline source, \
                   a string was not terminated."
    ~pp:(fun ppf loc -> Format.fprintf ppf "%a, unterminated string" print_location loc)
    Data_encoding.(obj1 (req "location" location_encoding))
    (function Unterminated_string loc -> Some loc | _ -> None)
    (fun loc -> Unterminated_string loc) ;
  register_error_kind `Permanent
    ~id: "micheline.parse_error.unterminated_integer"
    ~title: "Micheline parser error: unterminated integer"
    ~description: "While parsing a piece of Micheline source, \
                   an integer was not terminated."
    ~pp:(fun ppf loc -> Format.fprintf ppf "%a, unterminated integer" print_location loc)
    Data_encoding.(obj1 (req "location" location_encoding))
    (function Unterminated_integer loc -> Some loc | _ -> None)
    (fun loc -> Unterminated_integer loc) ;
  register_error_kind `Permanent
    ~id: "micheline.parse_error.odd_lengthed_bytes"
    ~title: "Micheline parser error: odd lengthed bytes"
    ~description: "While parsing a piece of Micheline source, the \
                   length of a byte sequence (0x...) was not a \
                   multiple of two, leaving a trailing half byte."
    ~pp:(fun ppf loc -> Format.fprintf ppf "%a, odd_lengthed bytes" print_location loc)
    Data_encoding.(obj1 (req "location" location_encoding))
    (function Odd_lengthed_bytes loc -> Some loc | _ -> None)
    (fun loc -> Odd_lengthed_bytes loc) ;
  register_error_kind `Permanent
    ~id: "micheline.parse_error.unterminated_comment"
    ~title: "Micheline parser error: unterminated comment"
    ~description: "While parsing a piece of Micheline source, \
                   a commentX was not terminated."
    ~pp:(fun ppf loc -> Format.fprintf ppf "%a, unterminated comment" print_location loc)
    Data_encoding.(obj1 (req "location" location_encoding))
    (function Unterminated_comment loc -> Some loc | _ -> None)
    (fun loc -> Unterminated_comment loc) ;
  register_error_kind `Permanent
    ~id: "micheline.parse_error.annotation_exceeds_max_length"
    ~title: "Micheline parser error: annotation exceeds max length"
    ~description: (Format.sprintf
                     "While parsing a piece of Micheline source, \
                      an annotation exceeded the maximum length (%d)." max_annot_length)
    ~pp:(fun ppf loc -> Format.fprintf ppf "%a, annotation exceeded maximum length (%d chars)"
            print_location
            loc max_annot_length)
    Data_encoding.(obj1 (req "location" location_encoding))
    (function Annotation_length loc -> Some loc | _ -> None)
    (fun loc -> Annotation_length loc) ;
  register_error_kind `Permanent
    ~id: "micheline.parse_error.unclosed_token"
    ~title: "Micheline parser error: unclosed token"
    ~description: "While parsing a piece of Micheline source, \
                   a parenthesis or a brace was unclosed."
    ~pp:(fun ppf (loc, token) ->
        Format.fprintf ppf "%a, unclosed %a" print_location loc print_token_kind token)
    Data_encoding.(obj2
                     (req "location"location_encoding)
                     (req "token" token_value_encoding))
    (function Unclosed { loc ; token } -> Some (loc, token) | _ -> None)
    (fun (loc, token) -> Unclosed { loc ; token }) ;
  register_error_kind `Permanent
    ~id: "micheline.parse_error.unexpected_token"
    ~title: "Micheline parser error: unexpected token"
    ~description: "While parsing a piece of Micheline source, \
                   an unexpected token was encountered."
    ~pp:(fun ppf (loc, token) ->
        Format.fprintf ppf "%a, unexpected %a" print_location loc print_token_kind token)
    Data_encoding.(obj2
                     (req "location"location_encoding)
                     (req "token" token_value_encoding))
    (function Unexpected { loc ; token } -> Some (loc, token) | _ -> None)
    (fun (loc, token) -> Unexpected { loc ; token }) ;
  register_error_kind `Permanent
    ~id: "micheline.parse_error.extra_token"
    ~title: "Micheline parser error: extra token"
    ~description: "While parsing a piece of Micheline source, \
                   an extra semi colon or parenthesis was encountered."
    ~pp:(fun ppf (loc, token) ->
        Format.fprintf ppf "%a, extra %a" print_location loc print_token_kind token)
    Data_encoding.(obj2
                     (req "location"location_encoding)
                     (req "token" token_value_encoding))
    (function Extra { loc ; token } -> Some (loc, token) | _ -> None)
    (fun (loc, token) -> Extra { loc ; token }) ;
  register_error_kind `Permanent
    ~id: "micheline.parse_error.misaligned_node"
    ~title: "Micheline parser error: misaligned node"
    ~description: "While parsing a piece of Micheline source, \
                   an expression was not aligned with its \
                   siblings of the same mother application \
                   or sequence."
    ~pp:(fun ppf node ->
        Format.fprintf ppf "%a, misaligned expression" print_location (location node))
    Data_encoding.(obj1 (req "expression" node_encoding))
    (function Misaligned node -> Some node | _ -> None)
    (fun node -> Misaligned node) ;
  register_error_kind `Permanent
    ~id: "micheline.parse_error.empty_expression"
    ~title: "Micheline parser error: empty_expression"
    ~description: "Tried to interpret an empty piece or \
                   Micheline source as a single expression."
    ~pp:(fun ppf () -> Format.fprintf ppf "empty expression")
    Data_encoding.empty
    (function Empty -> Some () | _ -> None)
    (fun () -> Empty)
