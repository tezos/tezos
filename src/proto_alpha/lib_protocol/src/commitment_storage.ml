(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

let get_opt = Storage.Commitments.get_option
let delete = Storage.Commitments.delete

let init ctxt commitments  =
  let init_commitment ctxt Commitment_repr.{ blinded_public_key_hash ; amount } =
    Storage.Commitments.init ctxt blinded_public_key_hash amount in
  fold_left_s init_commitment ctxt commitments >>=? fun ctxt ->
  return ctxt
