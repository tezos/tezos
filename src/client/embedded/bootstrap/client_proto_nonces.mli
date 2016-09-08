(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

val mem: Block_hash.t -> bool Lwt.t
val find: Block_hash.t -> Nonce.t option Lwt.t
val add: Block_hash.t -> Nonce.t -> unit tzresult Lwt.t
val del: Block_hash.t -> unit tzresult Lwt.t
val dels: Block_hash.t list -> unit tzresult Lwt.t
