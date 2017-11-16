(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Ed25519

type error += Inconsistent_hash of Public_key.t * Public_key_hash.t * Public_key_hash.t

val get:
  Raw_context.t -> Public_key_hash.t -> Public_key.t tzresult Lwt.t
val get_option:
  Raw_context.t -> Public_key_hash.t -> Public_key.t option tzresult Lwt.t
val reveal:
  Raw_context.t -> Public_key_hash.t -> Public_key.t -> Raw_context.t tzresult Lwt.t
val remove:
  Raw_context.t -> Public_key_hash.t -> Raw_context.t Lwt.t

val list:
  Raw_context.t -> (Public_key_hash.t * Public_key.t) list Lwt.t
