(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc.< contact@tezos.com >                 *)
(*                                                                        *)
(*    All rights reserved.No warranty, explicit or implicit, provided.    *)
(*                                                                        *)
(**************************************************************************)

open Proto_alpha.Tezos_context

let generate_proof_of_work_nonce () =
  Rand.generate Constants.proof_of_work_nonce_size

let generate_seed_nonce () =
  match Nonce.of_bytes @@
    Rand.generate Constants.nonce_length with
  | Error _ -> assert false
  | Ok nonce -> nonce
