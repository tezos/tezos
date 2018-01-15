(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc.< contact@tezos.com >                 *)
(*                                                                        *)
(*    All rights reserved.No warranty, explicit or implicit, provided.    *)
(*                                                                        *)
(**************************************************************************)

(** Extension of the Sodium module with helpers functions *)

val generate_proof_of_work_nonce : unit -> MBytes.t
val generate_seed_nonce : unit -> Proto_alpha.Tezos_context.Nonce.nonce
