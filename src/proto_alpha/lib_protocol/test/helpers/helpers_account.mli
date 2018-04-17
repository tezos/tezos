(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Proto_alpha
open Alpha_context

(** Facilities to deal with accounts , bootstrap accounts and make new
    accounts *)

(** Explicit account type *)
type account = {
  hpub : Signature.Public_key_hash.t;
  pub : Signature.Public_key.t;
  ppk : Signature.Secret_key.t;
  contract :
    Contract.contract;
}
type t = account

(** Bootstrap accounts of the sandbox *)
val bootstrap_accounts : account list

(** Generates a new (pub , ppk) pair and the associated default_contract *)
val new_account : unit -> account

(** Amount of cents in a new account *)
val init_amount : int

(** Credits a new account *)
val init_account :
  tc:context -> account ->
  (account * context) proto_tzresult Lwt.t

(** Generates a new account and credits it *)
val make_account :
  tc:context ->
  (account * context) proto_tzresult Lwt.t

(** Generates a list of new accounts and credits them *)
val make_accounts :
  tc:context -> int ->
  (account list * context) proto_tzresult Lwt.t

(** Better typed "make_accounts tc 2" *)
val make_2_accounts :
  tc:context ->
  ((account * account) * context) proto_tzresult Lwt.t

(** Better typed "make_accounts tc 4" *)
val make_4_accounts :
  tc:context ->
  ((t * t * t * t) * context) proto_tzresult Lwt.t

(** Debug : Displays an account and its balance *)
val display_account : tc:context -> account -> unit Lwt.t

(** Debug : Displays several accounts and their balances *)
val display_accounts : tc:context -> account list -> unit Lwt.t
