(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Client_keys

let scheme = "https"

let title = "..."

let description = "..."

let parse uri =
  let path = String.split '/' (Uri.path uri) in
  match List.rev path with
  | [] -> invalid_arg "..."
  | key :: rev_path ->
      Lwt.return (Signature.Public_key_hash.of_b58check key) >>=? fun key ->
      return (Uri.with_path uri (String.concat "/" (List.rev rev_path)),
              key)

let public_key uri =
  parse (uri : pk_uri :> Uri.t) >>=? fun (base, pkh) ->
  RPC_client.call_service
    Media_type.all_media_types
    ~base Signer_services.public_key ((), pkh) () ()

let neuterize uri =
  return (Client_keys.make_pk_uri (uri : sk_uri :> Uri.t))

let public_key_hash uri =
  public_key uri >>=? fun pk ->
  return (Signature.Public_key.hash pk)

let sign ?watermark uri msg =
  parse (uri : sk_uri :> Uri.t) >>=? fun (base, pkh) ->
  let msg =
    match watermark with
    | None -> msg
    | Some watermark ->
        MBytes.concat "" [ Signature.bytes_of_watermark watermark ; msg ] in
  RPC_client.call_service
    Media_type.all_media_types
    ~base Signer_services.sign ((), pkh) () msg
