(*---------------------------------------------------------------------------
   Copyright (c) 2017 Vincent Bernardoff. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Ledgerwallet

module Version : sig
  type app_class = Tezos | TezBake
  val pp_app_class : Format.formatter -> app_class -> unit

  type Transport.Status.t +=
      Tezos_impossible_to_read_version

  type t = {
    app_class : app_class ;
    major : int ;
    minor : int ;
    patch : int ;
  }
  val pp : Format.formatter -> t -> unit
end

type curve =
  | Ed25519
  | Secp256k1
  | Secp256r1

val curve_of_string : string -> curve option
val pp_curve : Format.formatter -> curve -> unit
val pp_curve_short : Format.formatter -> curve -> unit

val get_version :
  ?pp:Format.formatter -> ?buf:Cstruct.t ->
  Hidapi.t -> (Version.t, Transport.error) result
(** [get_version ?pp ?buf ledger] is the version information of the
    Ledger app running at [ledger]. *)

val get_git_commit :
  ?pp:Format.formatter -> ?buf:Cstruct.t ->
  Hidapi.t -> (string, Transport.error) result
(** [get_git_commit ?pp ?buf ledger] is the git commit information of
    the Ledger app running at [ledger]. *)

val get_authorized_key :
  ?pp:Format.formatter -> ?buf:Cstruct.t ->
  Hidapi.t -> (int32 list, Transport.error) result
(** [get_authorized_key ?pp ?buf ledger] is the BIP32 path of the key
    authorized to bake on the Ledger app running at [ledger]. *)

val get_public_key :
  ?prompt:bool ->
  ?pp:Format.formatter ->
  ?buf:Cstruct.t ->
  Hidapi.t -> curve -> int32 list -> (Cstruct.t, Transport.error) result
(** [get_public_key ?pp ?buf ?prompt ledger curve path] is [0x02 ||
    pk] from [ledger] at [path] for curve [curve]. If [prompt] is
    [true] (the default), then a prompt on Ledger screen will ask user
    confirmation. *)

val authorize_baking :
  ?pp:Format.formatter ->
  ?buf:Cstruct.t ->
  Hidapi.t -> curve -> int32 list -> (Cstruct.t, Transport.error) result
(** [authorize_baking ?pp ?buf ?prompt ledger curve path] is like
    [get_public_key] with [prompt = true], but only works with the
    baking Ledger application and serves to indicate that the key from
    [curve] at [path] is allowed to bake. *)

val get_high_watermark :
  ?pp:Format.formatter -> ?buf:Cstruct.t ->
  Hidapi.t -> (int32, Transport.error) result
(** [get_high_watermark ?pp ?buf ledger] is the current value of the
    high water mark on [ledger]. This works with the baking app
    only. *)

val set_high_watermark :
  ?pp:Format.formatter -> ?buf:Cstruct.t ->
  Hidapi.t -> int32 -> (unit, Transport.error) result
(** [get_high_watermark ?pp ?buf ledger hwm] reset the high water
    mark on [ledger] to [hwm]. This works with the baking app only. *)

val sign :
  ?pp:Format.formatter ->
  ?buf:Cstruct.t ->
  ?hash_on_ledger:bool ->
  Hidapi.t -> curve -> int32 list ->
  Cstruct.t -> (Cstruct.t, Transport.error) result
(** [sign ?pp ?buf ?hash_on_ledger h curve path payload] is the
    signature of [payload] (or its hash if [hash_on_ledger] is [true],
    the default), signed on [ledger] with key from curve [curve] at
    [path]. *)

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
