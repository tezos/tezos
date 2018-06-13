(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

module Make(P : sig
    val authenticate: Signature.Public_key_hash.t list -> MBytes.t -> Signature.t tzresult Lwt.t
    val logger: RPC_client.logger
  end)
  : Client_keys.SIGNER

val make_base: string -> int -> Uri.t
