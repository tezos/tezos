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

open Signer_logging

let log = lwt_log_notice


module Authorized_key =
  Client_aliases.Alias (struct
    include Signature.Public_key
    let name = "authorized_key"
    let to_source s = return (to_b58check s)
    let of_source t = Lwt.return (of_b58check t)
  end)

let check_magic_byte magic_bytes data =
  match magic_bytes with
  | None -> return_unit
  | Some magic_bytes ->
      let byte = MBytes.get_uint8 data 0 in
      if MBytes.length data > 1
      && (List.mem byte magic_bytes) then
        return_unit
      else
        failwith "magic byte 0x%02X not allowed" byte

let sign
    (cctxt : #Client_context.wallet)
    Signer_messages.Sign.Request.{ pkh ; data ; signature }
    ?magic_bytes ~require_auth =
  log Tag.DSL.(fun f ->
      f "Request for signing %d bytes of data for key %a, magic byte = %02X"
      -% t event "request_for_signing"
      -% s num_bytes (MBytes.length data)
      -% a Signature.Public_key_hash.Logging.tag pkh
      -% s magic_byte (MBytes.get_uint8 data 0)) >>= fun () ->
  check_magic_byte magic_bytes data >>=? fun () ->
  begin match require_auth, signature with
    | false, _ -> return_unit
    | true, None -> failwith "missing authentication signature field"
    | true, Some signature ->
        let to_sign = Signer_messages.Sign.Request.to_sign ~pkh ~data in
        Authorized_key.load cctxt >>=? fun keys ->
        if List.fold_left
            (fun acc (_, key) -> acc || Signature.check key signature to_sign)
            false keys
        then
          return_unit
        else
          failwith "invalid authentication signature"
  end >>=? fun () ->
  Client_keys.get_key cctxt pkh >>=? fun (name, _pkh, sk_uri) ->
  log Tag.DSL.(fun f ->
      f "Signing data for key %s"
      -% t event "signing_data"
      -% s Client_keys.Logging.tag name) >>= fun () ->
  Client_keys.sign cctxt sk_uri data >>=? fun signature ->
  return signature

let public_key (cctxt : #Client_context.wallet) pkh =
  log Tag.DSL.(fun f ->
      f "Request for public key %a"
      -% t event "request_for_public_key"
      -% a Signature.Public_key_hash.Logging.tag pkh) >>= fun () ->
  Client_keys.list_keys cctxt >>=? fun all_keys ->
  match List.find_opt (fun (_, h, _, _) -> Signature.Public_key_hash.equal h pkh) all_keys with
  | None ->
      log Tag.DSL.(fun f ->
          f "No public key found for hash %a"
          -% t event "not_found_public_key"
          -% a Signature.Public_key_hash.Logging.tag pkh) >>= fun () ->
      Lwt.fail Not_found
  | Some (_, _, None, _) ->
      log Tag.DSL.(fun f ->
          f "No public key found for hash %a"
          -% t event "not_found_public_key"
          -% a Signature.Public_key_hash.Logging.tag pkh) >>= fun () ->
      Lwt.fail Not_found
  | Some (name, _, Some pk, _) ->
      log Tag.DSL.(fun f ->
          f "Found public key for hash %a (name: %s)"
          -% t event "found_public_key"
          -% a Signature.Public_key_hash.Logging.tag pkh
          -% s Client_keys.Logging.tag name) >>= fun () ->
      return pk
