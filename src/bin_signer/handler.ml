(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

let log = Signer_logging.lwt_log_notice

module Authorized_key =
  Client_aliases.Alias (struct
    include Signature.Public_key
    let name = "authorized_key"
    let to_source s = return (to_b58check s)
    let of_source t = Lwt.return (of_b58check t)
  end)

let sign
    (cctxt : #Client_context.wallet)
    Signer_messages.Sign.Request.{ pkh ; data ; signature } ~require_auth =
  log "Request for signing %d bytes of data for key %a, magic byte = %02X"
    (MBytes.length data)
    Signature.Public_key_hash.pp pkh
    (MBytes.get_uint8 data 0) >>= fun () ->
  begin match require_auth, signature with
    | false, _ -> return ()
    | true, None -> failwith "missing authentication signature field"
    | true, Some signature ->
        let to_sign = Signer_messages.Sign.Request.to_sign ~pkh ~data in
        Authorized_key.load cctxt >>=? fun keys ->
        if List.fold_left
            (fun acc (_, key) -> acc || Signature.check key signature to_sign)
            false keys
        then
          return ()
        else
          failwith "invalid authentication signature"
  end >>=? fun () ->
  Client_keys.get_key cctxt pkh >>=? fun (name, _pkh, sk_uri) ->
  log "Signing data for key %s" name >>= fun () ->
  Client_keys.sign sk_uri data >>=? fun signature ->
  return signature

let public_key (cctxt : #Client_context.wallet) pkh =
  log "Request for public key %a"
    Signature.Public_key_hash.pp pkh >>= fun () ->
  Client_keys.get_public_key cctxt pkh >>=? fun (name, pk) ->
  log "Found public key for hash %a (name: %s)"
    Signature.Public_key_hash.pp pkh name >>= fun () ->
  return pk
