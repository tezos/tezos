(*---------------------------------------------------------------------------
   Copyright (c) 2017 Vincent Bernardoff. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Ledgerwallet

type ins =
  | Get_public_key
  | Sign

let int_of_ins = function
  | Get_public_key -> 0x02
  | Sign -> 0x04

type curve =
  | Ed25519
  | Secp256k1
  | Secp256r1

let int_of_curve = function
  | Ed25519 -> 0x00
  | Secp256k1 -> 0x01
  | Secp256r1 -> 0x02

let wrap_ins cmd =
  Apdu.create_cmd ~cmd ~cla_of_cmd:(fun _ -> 0x80) ~ins_of_cmd:int_of_ins

let write_path cs path =
  ListLabels.fold_left path ~init:cs ~f:begin fun cs i ->
    Cstruct.BE.set_uint32 cs 0 i ;
    Cstruct.shift cs 4
  end

let get_public_key ?pp ?buf h curve path =
  let nb_derivations = List.length path in
  if nb_derivations > 10 then invalid_arg "get_public_key: max 10 derivations" ;
  let lc = 1 + 4 * nb_derivations in
  let data_init = Cstruct.create lc in
  Cstruct.set_uint8 data_init 0 nb_derivations ;
  let data = Cstruct.shift data_init 1 in
  let _data = write_path data path in
  let msg = "Tezos.get_public_key" in
  let apdu =  Apdu.create ~p2:(int_of_curve curve)
      ~lc ~data:data_init (wrap_ins Get_public_key) in
  let addr = Transport.apdu ~msg ?pp ?buf h apdu in
  let keylen = Cstruct.get_uint8 addr 0 in
  Cstruct.sub addr 1 keylen

let sign ?pp ?buf h curve path payload =
  let nb_derivations = List.length path in
  if nb_derivations > 10 then invalid_arg "get_public_key: max 10 derivations" ;
  let lc = 1 + 4 * nb_derivations in
  let data_init = Cstruct.create lc in
  Cstruct.set_uint8 data_init 0 nb_derivations ;
  let data = Cstruct.shift data_init 1 in
  let _data = write_path data path in
  let cmd = wrap_ins Sign in
  let msg = "Tezos.sign" in
  let apdu = Apdu.create ~p2:(int_of_curve curve) ~lc ~data:data_init cmd in
  let _addr = Transport.apdu ~msg ?pp ?buf h apdu in
  Transport.write_payload ~mark_last:true ?pp ?buf ~msg ~cmd h ~p1:0x01 payload

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
