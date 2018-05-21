(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Alpha_context

val list:
  'a #RPC_context.simple -> 'a -> Contract.t list shell_tzresult Lwt.t

type info = {
  manager: public_key_hash ;
  balance: Tez.t ;
  spendable: bool ;
  delegate: bool * public_key_hash option ;
  counter: int32 ;
  script: Script.t option ;
  storage: Script.expr option ;
}

val info_encoding: info Data_encoding.t

val info:
  'a #RPC_context.simple -> 'a -> Contract.t -> info shell_tzresult Lwt.t

val balance:
  'a #RPC_context.simple -> 'a -> Contract.t -> Tez.t shell_tzresult Lwt.t

val manager:
  'a #RPC_context.simple -> 'a -> Contract.t -> public_key_hash shell_tzresult Lwt.t

val manager_key:
  'a #RPC_context.simple -> 'a -> Contract.t -> (public_key_hash * public_key option) shell_tzresult Lwt.t

val delegate:
  'a #RPC_context.simple -> 'a -> Contract.t -> public_key_hash shell_tzresult Lwt.t

val delegate_opt:
  'a #RPC_context.simple -> 'a -> Contract.t -> public_key_hash option shell_tzresult Lwt.t

val is_delegatable:
  'a #RPC_context.simple -> 'a -> Contract.t -> bool shell_tzresult Lwt.t

val is_spendable:
  'a #RPC_context.simple -> 'a -> Contract.t -> bool shell_tzresult Lwt.t

val counter:
  'a #RPC_context.simple -> 'a -> Contract.t -> int32 shell_tzresult Lwt.t

val script:
  'a #RPC_context.simple -> 'a -> Contract.t -> Script.t shell_tzresult Lwt.t

val script_opt:
  'a #RPC_context.simple -> 'a -> Contract.t -> Script.t option shell_tzresult Lwt.t

val storage:
  'a #RPC_context.simple -> 'a -> Contract.t -> Script.expr shell_tzresult Lwt.t

val storage_opt:
  'a #RPC_context.simple -> 'a -> Contract.t -> Script.expr option shell_tzresult Lwt.t
