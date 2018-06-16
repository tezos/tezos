(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

module Make(N : sig val scheme : string end) = struct

  open Client_keys

  let scheme = N.scheme

  module Make(P : sig
      val authenticate: Signature.Public_key_hash.t list -> MBytes.t -> Signature.t tzresult Lwt.t
      val logger: RPC_client.logger
    end) = struct

    let scheme = scheme

    let title =
      "Built-in tezos-signer using remote signer through hardcoded " ^ scheme ^ " requests."

    let description =
      "Valid locators are of this form:\n"
      ^ " - " ^ scheme ^ "://host/tz1...\n"
      ^ " - " ^ scheme ^ "://host:port/path/to/service/tz1..."

    let parse uri =
      (* extract `tz1..` from the last component of the path *)
      assert (Uri.scheme uri = Some scheme) ;
      let path = Uri.path uri in
      begin match String.rindex_opt path '/' with
        | None ->
            failwith "Invalid locator %a" Uri.pp_hum uri
        | Some i ->
            let pkh = String.sub path (i + 1) (String.length path - i - 1) in
            let path = String.sub path 0 i in
            return (Uri.with_path uri path, pkh)
      end >>=? fun (base, pkh) ->
      Lwt.return (Signature.Public_key_hash.of_b58check pkh) >>=? fun pkh ->
      return (base, pkh)

    let public_key uri =
      parse (uri : pk_uri :> Uri.t) >>=? fun (base, pkh) ->
      RPC_client.call_service
        ~logger: P.logger
        Media_type.all_media_types
        ~base Signer_services.public_key ((), pkh) () ()

    let neuterize uri =
      return (Client_keys.make_pk_uri (uri : sk_uri :> Uri.t))

    let public_key_hash uri =
      public_key uri >>=? fun pk ->
      return (Signature.Public_key.hash pk, Some pk)

    let sign ?watermark uri msg =
      parse (uri : sk_uri :> Uri.t) >>=? fun (base, pkh) ->
      let msg =
        match watermark with
        | None -> msg
        | Some watermark ->
            MBytes.concat "" [ Signature.bytes_of_watermark watermark ; msg ] in
      RPC_client.call_service
        ~logger: P.logger
        Media_type.all_media_types
        ~base Signer_services.authorized_keys () () () >>=? fun authorized_keys ->
      begin match authorized_keys with
        | Some authorized_keys ->
            P.authenticate
              authorized_keys
              (Signer_messages.Sign.Request.to_sign ~pkh ~data:msg) >>=? fun signature ->
            return (Some signature)
        | None -> return None
      end >>=? fun signature ->
      RPC_client.call_service
        ~logger: P.logger
        Media_type.all_media_types
        ~base Signer_services.sign ((), pkh)
        signature
        msg

  end

  let make_base host port =
    Uri.make ~scheme ~host ~port ()

end
