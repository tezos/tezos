(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Bigarray

type t = (char, int8_unsigned_elt, c_layout) Array1.t

let create sz = Array1.create char c_layout sz
let length = Array1.dim
let sub = Array1.sub
let shift ba off = sub ba off (length ba - off)
let blit src srcoff dst dstoff len =
  Array1.blit (sub src srcoff len) (sub dst dstoff len)
let copy ba =
  let ba' = create (Array1.dim ba) in
  Array1.blit ba ba';
  ba'
let fill = Array1.fill

let init sz v =
  let b = create sz in
  fill b v ;
  b

(** Adapted from ocaml-cstruct. *)

external unsafe_blit_string_to_bigstring
  : string -> int -> t -> int -> int -> unit
  = "caml_blit_string_to_bigstring" [@@noalloc]

external unsafe_blit_bigstring_to_bytes
  : t -> int -> bytes -> int -> int -> unit
  = "caml_blit_bigstring_to_string" [@@noalloc]

(** HACK: force Cstruct at link which provides the previous primitives. *)
let _dummy = Cstruct.byte_to_int

let invalid_bounds j l =
  invalid_arg (Printf.sprintf "invalid bounds (index %d, length %d)" j l)

let blit_from_string src srcoff dst dstoff len =
  if len < 0 || srcoff < 0 || dstoff < 0 || String.length src - srcoff < len then
    raise (Invalid_argument (invalid_bounds srcoff len));
  if length dst - dstoff < len then
    raise (Invalid_argument (invalid_bounds dstoff len));
  unsafe_blit_string_to_bigstring src srcoff dst dstoff len

let blit_to_bytes src srcoff dst dstoff len =
  if len < 0 || srcoff < 0 || dstoff < 0 || length src - srcoff < len then
    raise (Invalid_argument (invalid_bounds srcoff len));
  if Bytes.length dst - dstoff < len then
    raise (Invalid_argument (invalid_bounds dstoff len));
  unsafe_blit_bigstring_to_bytes src srcoff dst dstoff len

let to_string buf =
  let sz = length buf in
  let s = Bytes.create sz in
  unsafe_blit_bigstring_to_bytes buf 0 s 0 sz;
  Bytes.unsafe_to_string s

let of_string buf =
  let buflen = String.length buf in
  let c = create buflen in
  unsafe_blit_string_to_bigstring buf 0 c 0 buflen;
  c

let to_hex s = Hex.of_cstruct (Cstruct.of_bigarray s)
let of_hex s = Cstruct.to_bigarray (Hex.to_cstruct s)

let substring src srcoff len =
  if len < 0 || srcoff < 0 || length src - srcoff < len then
    raise (Invalid_argument (invalid_bounds srcoff len));
  let s = Bytes.create len in
  unsafe_blit_bigstring_to_bytes src srcoff s 0 len;
  Bytes.unsafe_to_string s

include EndianBigstring.BigEndian
include Compare.Make(struct
    type nonrec t = t
    let compare = Pervasives.compare
  end)

module LE = struct
  include EndianBigstring.LittleEndian
end

let concat b1 b2 =
  let l1 = length b1 in
  let l2 = length b2 in
  let b = create (l1 + l2) in
  blit b1 0 b 0 l1 ;
  blit b2 0 b l1 l2 ;
  b

let pp_hex ppf t =
  let `Hex s = to_hex t in
  Format.pp_print_string ppf s
