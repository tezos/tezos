(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

module Make(S : sig
    val default : Uri.t
    val authenticate: Signature.Public_key_hash.t list -> MBytes.t -> Signature.t tzresult Lwt.t
    val logger: RPC_client.logger
  end) : Client_keys.SIGNER

val make_pk: Signature.public_key -> Client_keys.pk_uri
val make_sk: Signature.secret_key -> Client_keys.sk_uri

val read_base_uri_from_env: unit -> Uri.t option tzresult Lwt.t
val parse_base_uri: string -> Uri.t tzresult Lwt.t
