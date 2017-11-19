(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(**

   Basic roll manipulation.

   If storage related to roll (a.k.a. `Storage.Roll`) are not used
   outside this module, this interface enforce the invariant that a
   roll is always either in the limbo list or in a contract list.

*)

type error +=
  | Consume_roll_change
  | No_roll_in_contract

val init : Raw_context.t -> Raw_context.t tzresult Lwt.t

val fold :
  Raw_context.t ->
  f:(Roll_repr.roll -> Contract_repr.t -> 'a -> 'a tzresult Lwt.t) ->
  'a -> 'a tzresult Lwt.t

val freeze_rolls_for_cycle :
  Raw_context.t -> Cycle_repr.t -> Raw_context.t tzresult Lwt.t

val clear_cycle :
  Raw_context.t -> Cycle_repr.t -> Raw_context.t tzresult Lwt.t

val baking_rights_owner :
  Raw_context.t -> Level_repr.t -> priority:int ->
  Ed25519.Public_key_hash.t tzresult Lwt.t

val endorsement_rights_owner :
  Raw_context.t -> Level_repr.t -> slot:int ->
  Ed25519.Public_key_hash.t tzresult Lwt.t

module Contract : sig

  val init :
    Raw_context.t -> Contract_repr.t -> Raw_context.t tzresult Lwt.t

  val add_amount :
    Raw_context.t -> Contract_repr.t -> Tez_repr.t -> Raw_context.t tzresult Lwt.t

  val remove_amount :
    Raw_context.t -> Contract_repr.t -> Tez_repr.t -> Raw_context.t tzresult Lwt.t

  val assert_empty : Raw_context.t -> Contract_repr.t -> unit tzresult Lwt.t

end

(**/**)

val get_contract_delegate:
  Raw_context.t -> Contract_repr.t -> Ed25519.Public_key_hash.t option tzresult Lwt.t

val value: Raw_context.t -> Tez_repr.t

(** HACK for the alphanet. *)
val may_recompute_rolls: Raw_context.t -> Raw_context.t tzresult Lwt.t
val next: Raw_context.t -> Int32.t tzresult Lwt.t
