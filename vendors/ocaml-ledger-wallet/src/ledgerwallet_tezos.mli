(*---------------------------------------------------------------------------
   Copyright (c) 2017 Vincent Bernardoff. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

type curve =
  | Ed25519
  | Secp256k1
  | Secp256r1

val get_public_key :
  ?pp:Format.formatter -> ?buf:Cstruct.t ->
  Hidapi.t -> curve -> int32 list -> Cstruct.t
(** [get_public_key ?pp ?buf ledger curve path] is [0x02 || pk] from
    [ledger] at [path] for curve [curve]. *)

val sign :
  ?pp:Format.formatter -> ?buf:Cstruct.t ->
  Hidapi.t -> curve -> int32 list -> Cstruct.t -> Cstruct.t
(** [sign ?pp ?buf h curve path payload] is [signature], signed from
    [ledger] with key from curve [curve] at [path]. *)

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
