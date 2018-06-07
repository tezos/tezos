(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

let log = Signer_logging.lwt_log_notice

let sign (cctxt : #Client_context.wallet) pkh data =
  log "Request for signing %d bytes of data for key %a, magic byte = %02X"
    (MBytes.length data)
    Signature.Public_key_hash.pp pkh
    (MBytes.get_uint8 data 0) >>= fun () ->
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
