(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Utils
open Lwt.Infix

let base = 58
let zbase = Z.of_int base

let log2 x = log x /. log 2.
let log2_base = log2 (float_of_int base)


module Alphabet = struct

  type t = { encode: string ; decode: string }

  let make alphabet =
    if String.length alphabet <> base then
      invalid_arg "Base58: invalid alphabet (length)" ;
    let str = Bytes.make 256 '\255' in
    for i = 0 to String.length alphabet - 1 do
      let char = int_of_char alphabet.[i] in
      if Bytes.get str char <> '\255' then
        Format.kasprintf invalid_arg
          "Base58: invalid alphabet (dup '%c' %d %d)"
        (char_of_int char) (int_of_char @@ Bytes.get str char) i ;
      Bytes.set str char (char_of_int i) ;
    done ;
    { encode = alphabet ; decode = Bytes.to_string str }

  let bitcoin =
    make "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"
  let ripple =
    make "rpshnaf39wBUDNEGHJKLM4PQRST7VWXYZ2bcdeCg65jkm8oFqi1tuvAxyz"
  let flickr =
    make "123456789abcdefghijkmnopqrstuvwxyzABCDEFGHJKLMNPQRSTUVWXYZ"

  let default = bitcoin

  let all_in_alphabet alphabet string =
    let ok = Array.make 256 false in
    String.iter (fun x -> ok.(Char.code x) <- true) alphabet.encode ;
    let res = ref true in
    for i = 0 to (String.length string) - 1 do
      res := !res && ok.(Char.code string.[i])
    done;
    !res

  let pp ppf { encode } = Format.fprintf ppf "%s" encode

end

let count_trailing_char s c =
  let len = String.length s in
  let rec loop i =
    if i < 0 then len
    else if String.get s i <> c then (len-i-1)
    else loop (i-1) in
  loop (len-1)

let count_leading_char s c =
  let len = String.length s in
  let rec loop i =
    if i = len then len
    else if String.get s i <> c then i
    else loop (i+1) in
  loop 0

let of_char ?(alphabet=Alphabet.default) x =
  let pos = String.get alphabet.decode (int_of_char x) in
  if pos = '\255' then failwith "Invalid data" ;
  int_of_char pos

let to_char ?(alphabet=Alphabet.default) x =
  alphabet.encode.[x]

let raw_encode ?(alphabet=Alphabet.default) s =
  let len = String.length s in
  let s = String.init len (fun i -> String.get s (len - i - 1)) in
  let zero = alphabet.encode.[0] in
  let zeros = count_trailing_char s '\000' in
  let res_len = (len * 8 + 4) / 5 in
  let res = Bytes.make res_len '\000' in
  let s = Z.of_bits s in
  let rec loop s =
    if s = Z.zero then 0 else
    let s, r = Z.div_rem s zbase in
    let i = loop s in
    Bytes.set res i (to_char ~alphabet (Z.to_int r)) ;
    i + 1 in
  let i = loop s in
  let res = Bytes.sub_string res 0 i in
  String.make zeros zero ^ res

let raw_decode ?(alphabet=Alphabet.default) s =
  let zero = alphabet.encode.[0] in
  let zeros = count_leading_char s zero in
  let len = String.length s in
  let rec loop res i =
    if i = len then res else
    let x = Z.of_int (of_char ~alphabet (String.get s i)) in
    let res = Z.(add x (mul res zbase)) in
    loop res (i+1)
  in
  let res = Z.to_bits @@ loop Z.zero zeros in
  let res_tzeros = count_trailing_char res '\000' in
  let len = String.length res - res_tzeros in
  String.make zeros '\000' ^
  String.init len (fun i -> String.get res (len - i - 1))

let checksum s =
  let hash =
    Nocrypto.Hash.digest `SHA256 @@
    Nocrypto.Hash.digest `SHA256 @@
    Cstruct.of_string s in
  let res = Bytes.make 4 '\000' in
  Cstruct.blit_to_bytes hash 0 res 0 4 ;
  Bytes.to_string res

(* Append a 4-bytes cryptographic checksum before encoding string s *)
let safe_encode ?alphabet s =
  raw_encode ?alphabet (s ^ checksum s)

let safe_decode ?alphabet s =
  let s = raw_decode ?alphabet s in
  let len = String.length s in
  let msg = String.sub s 0 (len-4)
  and msg_hash = String.sub s (len-4) 4 in
  if msg_hash <> checksum msg then
    invalid_arg "safe_decode" ;
  msg

type data = ..

type 'a encoding = {
  prefix: string ;
  length: int ;
  encoded_prefix: string ;
  encoded_length: int ;
  to_raw: 'a -> string ;
  of_raw: string -> 'a option ;
  wrap: 'a -> data ;
}

let simple_decode ?alphabet { prefix ; of_raw } s =
  safe_decode ?alphabet s |>
  remove_prefix ~prefix |>
  Utils.apply_option ~f:of_raw

let simple_encode ?alphabet { prefix ; to_raw } d =
  safe_encode ?alphabet (prefix ^ to_raw d)

type registred_encoding = Encoding : 'a encoding -> registred_encoding

module MakeEncodings(E: sig
    val encodings: registred_encoding list
  end) = struct

  let encodings = ref E.encodings

  let check_ambiguous_prefix prefix encodings =
    List.iter
      (fun (Encoding { encoded_prefix = s }) ->
         if remove_prefix s prefix <> None ||
            remove_prefix prefix s <> None then
           Format.ksprintf invalid_arg
             "Base58.register_encoding: duplicate prefix: %S, %S." s prefix)
      encodings

  let make_encoded_prefix prefix len =
    let zeros = safe_encode (prefix ^ String.make len '\000')
    and ones = safe_encode (prefix ^ String.make len '\255') in
    let len = String.length zeros in
    if String.length ones <> len then
      Format.ksprintf invalid_arg
        "Base58.registred_encoding: variable length encoding." ;
    let rec loop i =
      if i = len then len
      else if zeros.[i] = ones.[i] then loop (i+1)
      else i in
    let len = loop 0 in
    if len = 0 then
      invalid_arg
        "Base58.register_encoding: not a unique prefix." ;
    String.sub zeros 0 len, String.length zeros

  let register_encoding ~prefix ~length ~to_raw ~of_raw ~wrap =
    let to_raw x =
      let s = to_raw x in assert (String.length s = length) ; s in
    let of_raw s = assert (String.length s = length) ; of_raw s in
    let encoded_prefix, encoded_length = make_encoded_prefix prefix length in
    check_ambiguous_prefix encoded_prefix !encodings ;
    let encoding =
      { prefix ; length ; encoded_prefix ; encoded_length ;
        to_raw ; of_raw ; wrap } in
    encodings := Encoding encoding :: !encodings ;
    encoding

  let check_encoded_prefix enc p l =
    if enc.encoded_prefix <> p then
      Format.kasprintf failwith
        "Unexpected prefix %s (expected %s)"
        p enc.encoded_prefix ;
    if enc.encoded_length <> l then
      Format.kasprintf failwith
        "Unexpected encoded length %d for %s (expected %d)"
        l p enc.encoded_length

  let decode ?alphabet s =
    let rec find s = function
      | [] -> None
      | Encoding { prefix ; of_raw ; wrap } :: encodings ->
          match remove_prefix ~prefix s with
          | None -> find s encodings
          | Some msg -> of_raw msg |> Utils.map_option ~f:wrap in
    let s = safe_decode ?alphabet s in
    find s !encodings

end

type 'a resolver =
    Resolver : {
      encoding: 'h encoding ;
      resolver: 'a -> string -> 'h list Lwt.t ;
    } -> 'a resolver

module MakeResolvers(R: sig
    type context
    val encodings: registred_encoding list ref
  end) = struct

  let resolvers = ref []

  let register_resolver
      (type a)
      (encoding : a encoding)
      (resolver : R.context -> string -> a list Lwt.t) =
    resolvers := Resolver { encoding ; resolver } :: !resolvers

  let partial_decode ?(alphabet=Alphabet.default) request len =
    let zero = alphabet.encode.[0] in
    let last = alphabet.encode.[base-1] in
    let n = String.length request in
    let min = raw_decode ~alphabet (request ^ String.make (len - n) zero) in
    let max = raw_decode ~alphabet (request ^ String.make (len - n) last) in
    let prefix_len = Utils.common_prefix min max in
    String.sub min 0 prefix_len

  let complete ?alphabet context request =
    let rec find s = function
      | [] -> Lwt.return_nil
      | Resolver { encoding ; resolver } :: resolvers ->
          if not (has_prefix ~prefix:encoding.encoded_prefix s) then
            find s resolvers
          else
            let prefix =
              partial_decode ?alphabet request encoding.encoded_length in
            let len = String.length prefix in
            let ignored = String.length encoding.prefix in
            let msg =
              if len <= ignored then ""
              else begin
                assert (String.sub prefix 0 ignored  = encoding.prefix) ;
                String.sub prefix ignored (len - ignored)
              end in
            resolver context msg >|= fun msgs ->
            filter_map
              (fun msg ->
                 let res = simple_encode encoding ?alphabet msg in
                 Utils.remove_prefix ~prefix:request res |>
                 Utils.map_option ~f:(fun _ -> res))
              msgs in
    find request !resolvers

end

include MakeEncodings(struct let encodings = [] end)
include MakeResolvers(struct
    type context = unit
    let encodings = encodings
  end)

let register_resolver enc f = register_resolver enc (fun () s -> f s)
let complete ?alphabet s = complete ?alphabet () s

module Make(C: sig type context end) = struct
  include MakeEncodings(struct let encodings = !encodings end)
  include MakeResolvers(struct
      type context = C.context
      let encodings = encodings
    end)
end

module Prefix = struct

  (* 32 *)
  let block_hash = "\001\052" (* B(51) *)
  let operation_hash = "\005\116" (* o(51) *)
  let operation_list_hash = "\133\233" (* Lo(52) *)
  let operation_list_list_hash = "\029\159\109" (* LLo(53) *)
  let protocol_hash = "\002\170" (* P(51) *)

  (* 20 *)
  let ed25519_public_key_hash = "\006\161\159" (* tz1(36) *)

  (* 16 *)
  let cryptobox_public_key_hash = "\153\103" (* id(30) *)

  (* 32 *)
  let ed25519_public_key = "\013\015\037\217" (* edpk(54) *)

  (* 64 *)
  let ed25519_secret_key = "\043\246\078\007" (* edsk(98) *)
  let ed25519_signature = "\009\245\205\134\018" (* edsig(99) *)

  (* 4 *)
  let net_id = "\087\082\000" (* Net(15) *)

end
