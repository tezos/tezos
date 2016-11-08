(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(* Tezos Utility library - Hexadecimal encoding *)

(* From OCaml's stdlib. See [Digest.to_hex] *)
let gen_encode length get s =
  let n = length s in
  let result = Bytes.create (n*2) in
  for i = 0 to n-1 do
    Bytes.blit_string (Printf.sprintf "%02x" (get s i)) 0 result (2*i) 2;
  done;
  Bytes.unsafe_to_string result

let hex_of_bytes = gen_encode MBytes.length MBytes.get_uint8
let hex_encode = gen_encode String.length (fun s i -> int_of_char s.[i])

(* From OCaml's stdlib. See [Digest.from_hex]. *)
let gen_decode create set h =
  let n = String.length h in
  if n mod 2 <> 0 then invalid_arg ("hex_decode: " ^ h);
  let digit c =
    match c with
    | '0'..'9' -> int_of_char c - int_of_char '0'
    | 'A'..'F' -> int_of_char c - int_of_char 'A' + 10
    | 'a'..'f' -> int_of_char c - int_of_char 'a' + 10
    | _c -> invalid_arg ("hex_decode: " ^ h)
  in
  let byte i = digit h.[i] lsl 4 + digit h.[i+1] in
  let result = create (n / 2) in
  for i = 0 to n/2 - 1 do
    set result i (byte (2 * i));
  done;
  result

let hex_decode s =
  gen_decode Bytes.create (fun s i c -> Bytes.set s i (char_of_int c)) s |>
  Bytes.unsafe_to_string

let bytes_of_hex s =
  gen_decode MBytes.create MBytes.set_int8 s

(*
let hex_bytes =
  let open Data_encoding in
  let schema =
     let open Json_schema in
     create
       { title = None ;
         description = None ;
         default = None;
         enum = None;
         kind = String {
             pattern = Some "^[a-zA-Z0-9]+$";
             min_length = 0;
             max_length = None;
           };
         format = None ;
         id = None } in
  conv ~schema hex_of_bytes (Json.wrap_error bytes_of_hex) string

let sha256 =
  let open Data_encoding in
  let schema =
    let open Json_schema in
    create
      { title = None ;
        description = None ;
        default = None;
        enum = None;
        kind = String {
            pattern = Some "^[a-zA-Z0-9]+$";
            min_length = 64;
            max_length = Some 64;
          };
        format = Some "sha256" ;
        id = None } in
  conv ~schema hex_of_bytes (Json.wrap_error bytes_of_hex) string
*)
