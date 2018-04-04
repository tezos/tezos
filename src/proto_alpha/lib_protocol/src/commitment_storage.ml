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
  let init_commitment ctxt ( hpkh, commitment ) =
    Storage.Commitments.init ctxt hpkh commitment in
  fold_left_s init_commitment ctxt commitments >>=? fun ctxt ->
  return ctxt
