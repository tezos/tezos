(*---------------------------------------------------------------------------
   Copyright (c) 2017 Vincent Bernardoff. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open StdLabels

let acceptable_num_words = [12 ; 15 ; 18 ; 21 ; 24]

type entropy = {
  bytes : Bigstring.t ;
  length : int ;
  digest_length : int ;
  num_words : int ;
}

let entropy_of_bytes bytes =
  match Bigstring.length bytes with
  | 16 -> Some { bytes ; length = 16 ; digest_length = 4 ; num_words = 12 }
  | 20 -> Some { bytes ; length = 20 ; digest_length = 5 ; num_words = 15 }
  | 24 -> Some { bytes ; length = 24 ; digest_length = 6 ; num_words = 18 }
  | 28 -> Some { bytes ; length = 28 ; digest_length = 7 ; num_words = 21 }
  | 32 -> Some { bytes ; length = 32 ; digest_length = 8 ; num_words = 24 }
  | _ -> None

type t = int list

let index_of_word word =
  let index = ref (-1) in
  try
    List.iteri English.words ~f:begin fun i w ->
      if String.compare word w = 0 then (index := i ; raise Exit)
    end ;
    None
  with Exit -> Some !index

let of_words words =
  try
    List.fold_right words ~init:(0, []) ~f:begin fun word (count, acc) ->
      match index_of_word word with
      | Some i -> (succ count, i :: acc)
      | _ -> raise Exit
    end |> fun (count, x) ->
    if List.(mem count ~set:acceptable_num_words) then Some x
    else None
  with Exit -> None

let of_indices idxs =
  try
    List.fold_right idxs ~init:(0, []) ~f:begin fun i (count, acc) ->
      if i < 0 || i > 2047 then raise Exit
      else (succ count, i :: acc)
    end |> fun (count, x) ->
    if List.(mem count ~set:acceptable_num_words) then Some x
    else None
  with Exit -> None

let to_words = List.map ~f:(List.nth English.words)
let to_indices t = t

let pp ppf t =
  let open Format in
  let words = to_words t in
  let pp_mnemonic =
    pp_print_list
      ~pp_sep:(fun fmt () -> fprintf fmt " ")
      pp_print_string in
  fprintf ppf "%a" pp_mnemonic words

let show t =
  Format.asprintf "%a" pp t

let int_of_bits bits =
  snd @@ List.fold_right bits ~init:(0, 0) ~f:begin fun b (i, res) ->
    succ i, if b then res lor (1 lsl i) else res
  end

let bits_of_char c =
  let b = Char.code c in
  let res = ref [] in
  for i = 0 to 7 do
    res := (b land (1 lsl i) <> 0) :: !res
  done ;
  !res

let bits_of_bytes bytes =
  let acc = ref [] in
  String.iter bytes ~f:begin fun c ->
    acc := List.rev_append (bits_of_char c) !acc
  end ;
  List.rev !acc

let list_sub l n =
  let rec inner acc n l =
    if n > 0 then match l with
      | h :: tl -> inner (h :: acc) (pred n) tl
      | _ -> invalid_arg "Bip39.list_sub"
    else List.rev acc
  in inner [] n l

let pack l pack_len =
  let rec inner (sub_acc_len, sub_acc, acc) = function
    | [] -> if sub_acc <> [] then List.rev sub_acc :: acc else acc
    | h :: tl ->
        if sub_acc_len = pack_len then
          inner (1, [h], List.rev sub_acc :: acc) tl
        else inner (succ sub_acc_len, h :: sub_acc, acc) tl
  in
  List.rev (inner (0, [], []) l)

let of_entropy entropy =
  match entropy_of_bytes entropy with
  | None -> invalid_arg "Bip39.of_entropy: wrong entropy length"
  | Some { bytes ; digest_length ; _ } ->
      let digest = Bigstring.get (Hacl.Hash.SHA256.digest entropy) 0 in
      let digest = list_sub (bits_of_char digest) digest_length in
      let entropy = bits_of_bytes (Bigstring.to_string bytes) @ digest in
      List.map (pack entropy 11) ~f:int_of_bits

let to_seed ?(passphrase=Bigstring.empty) t =
  let words = to_words t in
  let password = Bigstring.of_string (String.concat ~sep:" " words) in
  let salt = Bigstring.(concat "" [of_string "mnemonic" ; passphrase]) in
  Pbkdf.SHA512.pbkdf2 ~password ~salt ~count:2048 ~dk_len:64l

(*---------------------------------------------------------------------------
   Copyright (c) 2017 Vincent Bernardoff

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
