(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type bootstrap_account = {
  public_key : Signature.Public_key.t ;
  amount : Tez_repr.t ;
  script : (Contract_repr.t * Script_repr.t) option ;
}

type t = {
  bootstrap_accounts : bootstrap_account list ;
  commitments : Commitment_repr.t list ;
  constants : Constants_repr.parametric ;
  security_deposit_ramp_up_cycles : int option ;
  no_reward_cycles : int option ;
}

val encoding: t Data_encoding.t
val constants_encoding: Constants_repr.parametric Data_encoding.t
