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

let title =
  "Built-in tezos-signer using remote signer through hardcoded https requests."

let description =
  "Valid locators are of this form:\n\
  \ -  https://host/tz1...\n\
  \ -  https://host:port/path/to/service/tz1...\n"

let parse uri =
  (* extract `tz1..` from the last component of the path *)
  assert (Uri.scheme uri = Some scheme) ;
  let path = Uri.path uri in
  let base, pkh =
    match String.rindex_opt path '/' with
    | None ->
        Uri.with_path uri "", path
    | Some i ->
        let pkh = String.sub path i (String.length path - i) in
        let path = String.sub path 0 i in
        Uri.with_path uri path, pkh in
  Lwt.return (Signature.Public_key_hash.of_b58check pkh) >>=? fun pkh ->
  return (base, pkh)

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

let make_base host port =
  Uri.make ~scheme ~host ~port ()
