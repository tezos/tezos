(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)


(** A null proof-of-work nonce. This should only be used to non-sensical blocks
    of the correct size and shape. *)
val empty_proof_of_work_nonce: Cstruct.buffer

(** [mine cctxt chain block header builder] returns a block with a valid
    proof-of-work nonce. The function [builder], provided by the caller, is used
    to make the block. All the internal logic of generating nonces and checking
    for the proof-of-work threshold is handled by [mine]. *)
val mine:
  #Proto_alpha.full ->
  Shell_services.chain ->
  Block_services.block ->
  Block_header.shell_header ->
  (Cstruct.buffer -> Proto_alpha.Alpha_context.Block_header.contents) ->
  Proto_alpha.Alpha_context.Block_header.contents tzresult Lwt.t



