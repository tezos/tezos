(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Utils

let (>>=) = Lwt.bind

let decode_alphabet alphabet =
  let str = Bytes.make 256 '\255' in
  for i = 0 to String.length alphabet - 1 do
    Bytes.set str (int_of_char alphabet.[i]) (char_of_int i) ;
  done ;
  Bytes.to_string str

let default_alphabet =
  "eE2NXaQvHPqDdTJxfF36jb7VRmp9tAyMgG4L5cS8CKrnksBh"

let default_decode_alphabet = decode_alphabet default_alphabet

let count_trailing_char s c =
  let len = String.length s in
  let rec loop i =
    if i < 0 then len
    else if String.get s i <> c then (len-i-1)
    else loop (i-1) in
  loop (len-1)

let of_char ?(alphabet=default_decode_alphabet) x =
  let pos = String.get alphabet (int_of_char x) in
  if pos = '\255' then failwith "Invalid data" ;
  int_of_char pos

let to_char ?(alphabet=default_alphabet) x =
  alphabet.[x]

let forty_eight = Z.of_int 48

let raw_encode ?alphabet s =
  let zero, alphabet =
    match alphabet with
    | None -> default_alphabet.[0], default_alphabet
    | Some alphabet ->
        if String.length alphabet <> 48 then invalid_arg "Base48.encode" ;
        alphabet.[0], decode_alphabet alphabet in
  let zeros = count_trailing_char s '\000' in
  let len = String.length s in
  let res_len = (len * 8 + 4) / 5 in
  let res = Bytes.make res_len '\000' in
  let s = Z.of_bits s in
  let rec loop s i =
    if s = Z.zero then i else
    let s, r = Z.div_rem s forty_eight in
    Bytes.set res i (to_char ~alphabet (Z.to_int r));
    loop s (i+1) in
  let i = loop s 0 in
  let res = Bytes.sub_string res 0 i in
  res ^ String.make zeros zero

let raw_decode ?alphabet s =
  let zero, alphabet =
    match alphabet with
    | None -> default_alphabet.[0], default_decode_alphabet
    | Some alphabet ->
        if String.length alphabet <> 48 then invalid_arg "Base48.decode" ;
        alphabet.[0], decode_alphabet alphabet in
  let zeros = count_trailing_char s zero in
  let len = String.length s in
  let rec loop res i =
    if i < 0 then res else
    let x = Z.of_int (of_char ~alphabet (String.get s i)) in
    let res = Z.(add x (mul res forty_eight)) in
    loop res (i-1)
  in
  let res = Z.to_bits @@ loop Z.zero (len - zeros - 1) in
  let res_tzeros = count_trailing_char res '\000' in
  String.sub res 0 (String.length res - res_tzeros) ^
  String.make zeros '\000'

let sha256 s =
  let hash = Cryptokit.Hash.sha256 () in
  hash#add_string s;
  let computed_hash = hash#result in hash#wipe;
  computed_hash

let safe_encode ?alphabet s =
  raw_encode ?alphabet (s ^ String.sub (sha256 (sha256 s)) 0 4)

let safe_decode ?alphabet s =
  let s = raw_decode ?alphabet s in
  let len = String.length s in
  let msg = String.sub s 0 (len-4)
  and msg_hash = String.sub s (len-4) 4 in
  if msg_hash <> String.sub (sha256 (sha256 msg)) 0 4 then
    invalid_arg "safe_decode" ;
  msg

type data = ..

type kind =
    Kind : { prefix: string;
             read: data -> string option ;
             build: string -> data ;
             mutable resolver: string -> string list Lwt.t ;
           } -> kind

let kinds = ref ([] : kind list)

exception Unknown_prefix

let decode ?alphabet s =
  let rec find s = function
    | [] -> raise Unknown_prefix
    | Kind { prefix ; build } :: kinds ->
        match remove_prefix ~prefix s with
        | None -> find s kinds
        | Some msg -> build msg in
  let s = safe_decode ?alphabet s in
  find s !kinds

exception Unregistred_kind

let encode ?alphabet s =
  let rec find s = function
    | [] -> raise Unregistred_kind
    | Kind { prefix ; read } :: kinds ->
        match read s with
        | None -> find s kinds
        | Some msg -> safe_encode ?alphabet (prefix ^ msg) in
  try find s !kinds
  with Not_found -> raise Unknown_prefix

let default_resolver _ = Lwt.return_nil

let register ~prefix ~read ~build =
  match List.find (fun (Kind {prefix=s}) -> remove_prefix s prefix <> None || remove_prefix prefix s <> None) !kinds with
  | exception Not_found ->
      let kind =
        Kind { prefix ; read ; build ; resolver = default_resolver } in
      kinds := kind :: !kinds ;
      kind
  | Kind { prefix = s } ->
      Format.kasprintf
        Pervasives.failwith
        "Base48.register: Conflicting prefixes: %S and %S." prefix s

let register_resolver (Kind k) resolver = k.resolver <- resolver

module Prefix = struct
  let block_hash = "\000"
  let operation_hash = "\001"
  let protocol_hash = "\002"
  let public_key_hash = "\003"
  let public_key = "\004"
  let secret_key = "\005"
  let signature = "\006"
  let protocol_prefix = "\015"
end

let decode_partial ?alphabet request =
  let n = String.length request in
  let s = raw_decode request ?alphabet in
  let partial = String.sub s 0 (n/2) in
  let rec find s = function
    | [] -> Lwt.return_nil
    | Kind { prefix ; build ; resolver } :: kinds ->
        match remove_prefix ~prefix s with
        | None -> find s kinds
        | Some msg ->
            resolver msg >>= fun msgs ->
            let candidates = List.map build msgs in
            Lwt.return @@
            List.filter
              (fun data ->
                 match Utils.remove_prefix ~prefix:request (encode data) with
                 | None -> false
                 | Some _ -> true)
              candidates in
  find partial !kinds
