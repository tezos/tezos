(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(***************** Prevalidation errors ***********************************)

type error += Parse_error
type error += Too_many_operations
type error += Oversized_operation of { size: int ; max: int }
type error += Future_block_header of Block_hash.t

(************************* State errors ***********************************)

type error +=  Unknown_chain of Chain_id.t
type error += Bad_data_dir
type error += Block_not_invalid of Block_hash.t

(* Block database error *)

type error += Inconsistent_hash of Context_hash.t * Context_hash.t

(******************* Bootstrap pipeline errors ****************************)

type error += Invalid_locator of P2p_peer.Id.t * Block_locator.t

(******************* Protocol validator errors ****************************)

type protocol_error =
  | Compilation_failed
  | Dynlinking_failed

type error += Invalid_protocol of { hash: Protocol_hash.t ; error: protocol_error }

(********************* Peer validator errors ******************************)

type error +=
  | Unknown_ancestor
  | Known_invalid

(************************ Validator errors ********************************)

type error +=  Inactive_chain of Chain_id.t
type error += Checkpoint_error of Block_hash.t * P2p_peer.Id.t option
