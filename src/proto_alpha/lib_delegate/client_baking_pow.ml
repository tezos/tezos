(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Proto_alpha

let generate_proof_of_work_nonce () =
  Rand.generate Alpha_context.Constants.proof_of_work_nonce_size

let empty_proof_of_work_nonce =
  MBytes.of_string
    (String.make Constants_repr.proof_of_work_nonce_size  '\000')

let mine cctxt chain block shell builder =
  Alpha_services.Constants.all cctxt (chain, block) >>=? fun constants ->
  let threshold = constants.parametric.proof_of_work_threshold in
  let rec loop () =
    let block = builder (generate_proof_of_work_nonce ()) in
    if Baking.check_header_proof_of_work_stamp shell block threshold then
      return block
    else
      loop ()
  in
  loop ()
