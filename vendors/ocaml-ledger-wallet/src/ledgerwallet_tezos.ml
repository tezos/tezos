(*---------------------------------------------------------------------------
   Copyright (c) 2017 Vincent Bernardoff. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Rresult
open Ledgerwallet

module Version = struct
  type app_class = Tezos | TezBake
  let pp_app_class ppf = function
    | Tezos -> Format.pp_print_string ppf "Tezos Wallet"
    | TezBake -> Format.pp_print_string ppf "Tezos Baking"

  let class_of_int = function
    | 0 -> Tezos
    | 1 -> TezBake
    | _ -> invalid_arg "class_of_int"

  type t = {
    app_class : app_class ;
    major : int ;
    minor : int ;
    patch : int ;
  }

  let pp ppf { app_class ; major ; minor ; patch } =
    Format.fprintf ppf "%a %d.%d.%d"
      pp_app_class app_class major minor patch

  let create ~app_class ~major ~minor ~patch = {
    app_class ; major ; minor ; patch
  }

  type Transport.Status.t +=
      Tezos_impossible_to_read_version

  let () = Transport.Status.register_string_f begin function
      | Tezos_impossible_to_read_version ->
          Some "Impossible to read version"
      | _ -> None
    end

  let read cs =
    try
      let app_class = class_of_int (Cstruct.get_uint8 cs 0) in
      let major = Cstruct.get_uint8 cs 1 in
      let minor = Cstruct.get_uint8 cs 2 in
      let patch = Cstruct.get_uint8 cs 3 in
      R.ok (create ~app_class ~major ~minor ~patch)
    with _ ->
      Transport.app_error
        ~msg:"Version.read" (R.error Tezos_impossible_to_read_version)
end

type ins =
  | Version
  | Git_commit
  | Authorize_baking
  | Get_public_key
  | Prompt_public_key
  | Sign
  | Sign_unsafe
  | Reset_high_watermark
  | Query_high_watermark
  | Get_authorized_key

let int_of_ins = function
  | Version -> 0x00
  | Authorize_baking -> 0x01
  | Get_public_key -> 0x02
  | Prompt_public_key -> 0x03
  | Sign -> 0x04
  | Sign_unsafe -> 0x05
  | Reset_high_watermark -> 0x06
  | Query_high_watermark -> 0x08
  | Git_commit -> 0x09
  | Get_authorized_key -> 0x07

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

let get_version ?pp ?buf h =
  let apdu = Apdu.create (wrap_ins Version) in
  Transport.apdu ~msg:"get_version" ?pp ?buf h apdu >>=
  Version.read

let get_git_commit ?pp ?buf h =
  let apdu = Apdu.create (wrap_ins Git_commit) in
  Transport.apdu ~msg:"get_git_commit" ?pp ?buf h apdu >>|
  Cstruct.to_string

let get_authorized_key ?pp ?buf h =
  let apdu = Apdu.create (wrap_ins Get_authorized_key) in
  Transport.apdu ~msg:"get_authorized_key" ?pp ?buf h apdu >>| fun path ->
  let rec read_numbers acc path =
    if Cstruct.len path = 0 then List.rev acc
    else
      read_numbers (Cstruct.BE.get_uint32 path 0 :: acc)
        (Cstruct.shift path 4) in
  read_numbers [] (Cstruct.shift path 1)

let write_path cs path =
  ListLabels.fold_left path ~init:cs ~f:begin fun cs i ->
    Cstruct.BE.set_uint32 cs 0 i ;
    Cstruct.shift cs 4
  end

let get_public_key_like cmd ?pp ?buf h curve path =
  let nb_derivations = List.length path in
  if nb_derivations > 10 then invalid_arg "get_public_key: max 10 derivations" ;
  let lc = 1 + 4 * nb_derivations in
  let data_init = Cstruct.create lc in
  Cstruct.set_uint8 data_init 0 nb_derivations ;
  let data = Cstruct.shift data_init 1 in
  let _data = write_path data path in
  let msg = "get_public_key" in
  let apdu = Apdu.create
      ~p2:(int_of_curve curve) ~lc ~data:data_init (wrap_ins cmd) in
  Transport.apdu ~msg ?pp ?buf h apdu >>| fun addr ->
  let keylen = Cstruct.get_uint8 addr 0 in
  Cstruct.sub addr 1 keylen

let get_public_key ?(prompt=true) =
  let cmd = if prompt then Prompt_public_key else Get_public_key in
  get_public_key_like cmd

let authorize_baking = get_public_key_like Authorize_baking

let get_high_watermark ?pp ?buf h =
  let apdu = Apdu.create (wrap_ins Query_high_watermark) in
  Transport.apdu ~msg:"get_high_watermark" ?pp ?buf h apdu >>| fun hwm ->
  Cstruct.BE.get_uint32 hwm 0

let set_high_watermark ?pp ?buf h hwm =
  let data = Cstruct.create 4 in
  Cstruct.BE.set_uint32 data 0 hwm ;
  let apdu = Apdu.create ~lc:4 ~data (wrap_ins Reset_high_watermark) in
  Transport.apdu ~msg:"set_high_watermark" ?pp ?buf h apdu >>|
  ignore

let sign ?pp ?buf ?(hash_on_ledger=true) h curve path payload =
  let nb_derivations = List.length path in
  if nb_derivations > 10 then invalid_arg "get_public_key: max 10 derivations" ;
  let lc = 1 + 4 * nb_derivations in
  let data_init = Cstruct.create lc in
  Cstruct.set_uint8 data_init 0 nb_derivations ;
  let data = Cstruct.shift data_init 1 in
  let _data = write_path data path in
  let cmd = wrap_ins (if hash_on_ledger then Sign else Sign_unsafe) in
  let msg = "sign" in
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
