(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

module Make(C : sig val cctxt: Client_context.prompter end) : Client_keys.SIGNER

val decrypt:
  #Client_context.io_wallet ->
  ?name:string ->
  Client_keys.sk_uri -> Signature.secret_key tzresult Lwt.t

val decrypt_all:
  #Client_context.io_wallet -> unit tzresult Lwt.t

val encrypt:
  #Client_context.io ->
  Signature.secret_key -> Client_keys.sk_uri tzresult Lwt.t
