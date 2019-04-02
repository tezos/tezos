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

type error += No_identity_file of string
type error += Insufficient_proof_of_work of { expected: float }
type error += Identity_mismatch of {
    filename: string ;
    peer_id: Crypto_box.Public_key_hash.t ;
  }
type error += Identity_keys_mismatch of {
    filename: string ;
    expected_key:Crypto_box.public_key ;
  }

let () =
  register_error_kind
    `Permanent
    ~id:"main.identity.no_file"
    ~title:"No identity file"
    ~description:"The node identity file cannot be found"
    ~pp:(fun ppf file ->
        Format.fprintf ppf
          "Cannot read the identity file: `%s`. \
           See `%s identity --help` on how to generate an identity."
          file Sys.argv.(0))
    Data_encoding.(obj1 (req "file" string))
    (function No_identity_file file -> Some file | _ -> None)
    (fun file -> No_identity_file file)

let () =
  register_error_kind
    `Permanent
    ~id:"main.identity.insufficient_proof_of_work"
    ~title:"Insufficient proof of work"
    ~description:"The proof of work embeded by the current identity is not sufficient"
    ~pp:(fun ppf expected ->
        Format.fprintf ppf
          "The current identity does not embed a sufficient stamp of proof-of-work. \
           (expected level: %.2f). \
           See `%s identity --help` on how to generate a new identity."
          expected Sys.argv.(0))
    Data_encoding.(obj1 (req "expected" float))
    (function Insufficient_proof_of_work { expected } -> Some expected | _ -> None)
    (fun expected -> Insufficient_proof_of_work { expected })


let () =
  register_error_kind
    `Permanent
    ~id:"main.identity.identity_mismatch"
    ~title:"Identity mismatch"
    ~description:"The identity (public key hash) does not match the keys provided with it"
    ~pp:(fun ppf (file, public_key_hash) ->
        Format.fprintf ppf
          "The current identity (public key hash) does not match the keys in %s.
           Expected identity %a."
          file
          Crypto_box.Public_key_hash.pp
          public_key_hash)
    Data_encoding.(obj2 (req "file" string) (req "public_key_hash" Crypto_box.Public_key_hash.encoding))
    (function Identity_mismatch { filename ; peer_id } ->
       Some (filename, peer_id) | _ -> None)
    (fun (filename,peer_id) -> Identity_mismatch { filename ; peer_id })

let () =
  register_error_kind
    `Permanent
    ~id:"main.identity.identity_keys_mismatch"
    ~title:"Identity keys mismatch"
    ~description:"The current identity file has non-matching keys (secret key/ public key pair is not valid)"
    ~pp:(fun ppf (file, public_key) ->
        Format.fprintf ppf
          "The current identity file %s has non-matching keys (secret key/ public key pair is not valid).
           Expected public key %a."
          file
          Crypto_box.pp_pk
          public_key)
    Data_encoding.(obj2 (req "file" string) (req "public_key" Crypto_box.public_key_encoding))
    (function
      | Identity_keys_mismatch { filename ; expected_key } ->
          Some (filename, expected_key)
      | _ -> None)
    (fun (filename, expected_key) ->
       Identity_keys_mismatch { filename ; expected_key })

let read ?expected_pow filename =
  Lwt_unix.file_exists filename >>= function
  | false ->
      fail (No_identity_file filename)
  | true ->
      Lwt_utils_unix.Json.read_file filename >>=? fun json ->
      let id = Data_encoding.Json.destruct P2p_identity.encoding json in
      let pkh = Crypto_box.hash id.public_key in
      (* check public_key hash *)
      if not (Crypto_box.Public_key_hash.equal pkh id.peer_id) then
        fail (Identity_mismatch { filename ; peer_id = pkh })
        (* check public/private keys correspondance *)
      else if not Crypto_box.(equal (neuterize id.secret_key) id.public_key) then
        fail (Identity_keys_mismatch { filename ; expected_key = id.public_key })
      else (* check PoW level *)
        match expected_pow with
        | None -> return id
        | Some expected ->
            let target = Crypto_box.make_target expected in
            if
              not (Crypto_box.check_proof_of_work
                     id.public_key id.proof_of_work_stamp target)
            then
              fail (Insufficient_proof_of_work { expected })
            else
              return id

type error += Existent_identity_file of string

let () =
  register_error_kind
    `Permanent
    ~id:"main.identity.existent_file"
    ~title:"Cannot overwrite identity file"
    ~description:"Cannot implicitely overwrite the current identity file"
    ~pp:(fun ppf file ->
        Format.fprintf ppf
          "Cannot implicitely overwrite the current identity file: '%s'. \
           See `%s identity --help` on how to generate a new identity."
          file Sys.argv.(0))
    Data_encoding.(obj1 (req "file" string))
    (function Existent_identity_file file -> Some file | _ -> None)
    (fun file -> Existent_identity_file file)

let write file identity =
  if Sys.file_exists file then
    fail (Existent_identity_file file)
  else
    Node_data_version.ensure_data_dir (Filename.dirname file) >>=? fun () ->
    Lwt_utils_unix.Json.write_file file
      (Data_encoding.Json.construct P2p_identity.encoding identity)
