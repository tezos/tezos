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
  end) : sig
  module Unix : Client_keys.SIGNER
  module Tcp : Client_keys.SIGNER
end

val make_unix_base: string -> Uri.t
val make_tcp_base: string -> int -> Uri.t
