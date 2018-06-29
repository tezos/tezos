(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Proto_alpha

type t = {
  pkh : Signature.Public_key_hash.t ;
  pk :  Signature.Public_key.t ;
  sk :  Signature.Secret_key.t ;
}
type account = t

val activator_account: account
val dummy_account: account

val new_account: unit -> account

val add_account : t -> unit

val find: Signature.Public_key_hash.t -> t tzresult Lwt.t
val find_alternate: Signature.Public_key_hash.t -> t

(** [generate_accounts n] : generates [n] random accounts with
    4.000.000.000 tz and add them to the global account state *)
val generate_accounts : int -> (t * Tez_repr.t) list
