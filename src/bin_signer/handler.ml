(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Client_signer_remote_messages

let log = Logging.Client.Sign.lwt_log_notice

let sign (cctxt : #Client_context.wallet) key data =
  log "Request for signing %d bytes of data for key %s, magic byte = %02X"
    (MBytes.length data)
    key
    (MBytes.get_uint8 data 0) >>= fun () ->
  Client_keys.alias_keys cctxt key >>=? function
  | None -> failwith "Unknown alias key (%s)" key
  | Some (_, _, None) -> failwith "Unknown secret key (%s)" key
  | Some (_, _, Some skloc) ->
      log "Signing data for key %s" key >>= fun () ->
      Client_keys.sign cctxt skloc data >>=? fun signature ->
      return { Sign.Response.signature = signature }

let public_key (cctxt : #Client_context.wallet) key =
  Client_keys.alias_keys cctxt key >>=? function
  | None -> failwith "Unkown alias key (%s)" key
  | Some (public_key_hash, _, _) ->
      log "Found public key hash %a for key %s"
        Signature.Public_key_hash.pp public_key_hash key >>= fun () ->
      Client_keys.get_key cctxt public_key_hash >>=? fun (_, public_key, _) ->
      log "Found public key for key %s" key >>= fun () ->
      return { Public_key.Response.public_key }
