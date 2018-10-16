(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

(** Tezos - X25519/XSalsa20-Poly1305 cryptography *)

open Hacl

type secret_key = secret Box.key
type public_key = public Box.key
type channel_key = Box.combined Box.key
type nonce = Bigstring.t
type target = Z.t

module Secretbox = struct
  include Secretbox
  let box_noalloc key nonce msg =
    box ~key ~nonce ~msg ~cmsg:msg

  let box_open_noalloc key nonce cmsg =
    box_open ~key ~nonce ~cmsg ~msg:cmsg

  let box key msg nonce =
    let msglen = MBytes.length msg in
    let cmsg = MBytes.create (msglen + zerobytes) in
    MBytes.fill cmsg '\x00' ;
    MBytes.blit msg 0 cmsg zerobytes msglen ;
    box ~key ~nonce ~msg:cmsg ~cmsg ;
    MBytes.sub cmsg boxzerobytes (msglen + zerobytes - boxzerobytes)

  let box_open key cmsg nonce =
    let cmsglen = MBytes.length cmsg in
    let msg = MBytes.create (cmsglen + boxzerobytes) in
    MBytes.fill msg '\x00' ;
    MBytes.blit cmsg 0 msg boxzerobytes cmsglen ;
    match box_open ~key ~nonce ~cmsg:msg ~msg with
    | false -> None
    | true -> Some (MBytes.sub msg zerobytes (cmsglen - boxzerobytes))
end

module Public_key_hash = Blake2B.Make (Base58) (struct
    let name = "Crypto_box.Public_key_hash"
    let title = "A Cryptobox public key ID"
    let b58check_prefix = Base58.Prefix.cryptobox_public_key_hash
    let size = Some 16
  end)

let () =
  Base58.check_encoded_prefix Public_key_hash.b58check_encoding "id" 30

let hash pk =
  Public_key_hash.hash_bytes [Box.unsafe_to_bytes pk]

let zerobytes = Box.zerobytes
let boxzerobytes = Box.boxzerobytes

let random_keypair () =
  let pk, sk = Box.keypair () in
  sk, pk, hash pk

let zero_nonce = MBytes.make Nonce.bytes '\x00'
let random_nonce = Nonce.gen
let increment_nonce = Nonce.increment
let generate_nonce mbytes =
  let hash = Blake2B.hash_bytes mbytes in
  Nonce.of_bytes_exn @@ (Bigstring.sub (Blake2B.to_bytes hash) 0 Nonce.bytes)

let init_to_resp_seed = MBytes.of_string "Init -> Resp"
let resp_to_init_seed  = MBytes.of_string "Resp -> Init"
let generate_nonces ~incoming ~sent_msg ~recv_msg =
  let (init_msg, resp_msg, false)
    | (resp_msg, init_msg, true) = (sent_msg, recv_msg, incoming) in
  let nonce_init_to_resp =
    generate_nonce [ init_msg ; resp_msg ; init_to_resp_seed ] in
  let nonce_resp_to_init =
    generate_nonce [ init_msg ; resp_msg ; resp_to_init_seed ] in
  if incoming then
    (nonce_init_to_resp, nonce_resp_to_init)
  else
    (nonce_resp_to_init, nonce_init_to_resp)

let precompute sk pk = Box.dh pk sk

let fast_box_noalloc k nonce msg =
  Box.box ~k ~nonce ~msg ~cmsg:msg

let fast_box_open_noalloc k nonce cmsg =
  Box.box_open ~k ~nonce ~cmsg ~msg:cmsg

let fast_box k msg nonce =
  let msglen = MBytes.length msg in
  let cmsg = MBytes.create (msglen + zerobytes) in
  MBytes.fill cmsg '\x00' ;
  MBytes.blit msg 0 cmsg zerobytes msglen ;
  Box.box ~k ~nonce ~msg:cmsg ~cmsg ;
  cmsg

let fast_box_open k cmsg nonce =
  let cmsglen = MBytes.length cmsg in
  let msg = MBytes.create cmsglen in
  match Box.box_open ~k ~nonce ~cmsg ~msg with
  | false -> None
  | true -> Some (MBytes.sub msg zerobytes (cmsglen - zerobytes))

let compare_target hash target =
  let hash = Z.of_bits (Blake2B.to_string hash) in
  Z.compare hash target <= 0

let make_target f =
  if f < 0. || 256. < f then invalid_arg "Cryptobox.target_of_float" ;
  let frac, shift = modf f in
  let shift = int_of_float shift in
  let m =
    Z.of_int64 @@
    if frac = 0. then
      Int64.(pred (shift_left 1L 54))
    else
      Int64.of_float (2. ** (54. -. frac))
  in
  if shift < 202 then
    Z.logor
      (Z.shift_left m (202 - shift))
      (Z.pred @@ Z.shift_left Z.one (202 - shift))
  else
    Z.shift_right m (shift - 202)

let default_target = make_target 24.

let check_proof_of_work pk nonce target =
  let hash =
    Blake2B.hash_bytes [
      Box.unsafe_to_bytes pk ;
      nonce ;
    ] in
  compare_target hash target

let generate_proof_of_work ?max pk target =
  let may_interupt =
    match max with
    | None -> (fun _ -> ())
    | Some max -> (fun cpt -> if max < cpt then raise Not_found) in
  let rec loop nonce cpt =
    may_interupt cpt ;
    if check_proof_of_work pk nonce target then
      nonce
    else
      loop (Nonce.increment nonce) (cpt + 1) in
  loop (random_nonce ()) 0

let public_key_to_bigarray pk =
  let buf = MBytes.create Box.pkbytes in
  Box.blit_to_bytes pk buf ;
  buf

let public_key_of_bigarray buf =
  let pk = MBytes.copy buf in
  Box.unsafe_pk_of_bytes pk

let public_key_size = Box.pkbytes

let secret_key_to_bigarray sk =
  let buf = MBytes.create Box.skbytes in
  Box.blit_to_bytes sk buf ;
  buf

let secret_key_of_bigarray buf =
  let sk = MBytes.copy buf in
  Box.unsafe_sk_of_bytes sk

let secret_key_size = Box.skbytes

let nonce_size = Nonce.bytes

let public_key_encoding =
  let open Data_encoding in
  conv
    public_key_to_bigarray
    public_key_of_bigarray
    (Fixed.bytes public_key_size)

let secret_key_encoding =
  let open Data_encoding in
  conv
    secret_key_to_bigarray
    secret_key_of_bigarray
    (Fixed.bytes secret_key_size)

let nonce_encoding =
  Data_encoding.Fixed.bytes nonce_size
let neuterize : secret_key -> public_key = Box.neuterize
let equal : public_key -> public_key -> bool = Box.equal

let pp_pk ppf pk =
  MBytes.pp_hex ppf (public_key_to_bigarray pk)
