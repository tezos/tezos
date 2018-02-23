(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
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
  | No_roll_for_delegate
  | Unregistered_delegate of Ed25519.Public_key_hash.t (* `Permanent *)

val init : Raw_context.t -> Raw_context.t tzresult Lwt.t
val init_first_cycles : Raw_context.t -> Raw_context.t tzresult Lwt.t

val cycle_end : Raw_context.t -> Cycle_repr.t -> Raw_context.t tzresult Lwt.t

val fold :
  Raw_context.t ->
  f:(Roll_repr.roll -> Ed25519.Public_key.t -> 'a -> 'a tzresult Lwt.t) ->
  'a -> 'a tzresult Lwt.t

val baking_rights_owner :
  Raw_context.t -> Level_repr.t -> priority:int ->
  Ed25519.Public_key.t tzresult Lwt.t

val endorsement_rights_owner :
  Raw_context.t -> Level_repr.t -> slot:int ->
  Ed25519.Public_key.t tzresult Lwt.t

module Delegate : sig

  val add_amount :
    Raw_context.t -> Ed25519.Public_key_hash.t -> Tez_repr.t -> Raw_context.t tzresult Lwt.t

  val remove_amount :
    Raw_context.t -> Ed25519.Public_key_hash.t -> Tez_repr.t -> Raw_context.t tzresult Lwt.t

end

module Contract : sig

  val add_amount :
    Raw_context.t -> Contract_repr.t -> Tez_repr.t -> Raw_context.t tzresult Lwt.t

  val remove_amount :
    Raw_context.t -> Contract_repr.t -> Tez_repr.t -> Raw_context.t tzresult Lwt.t

end

val delegate_pubkey:
  Raw_context.t -> Ed25519.Public_key_hash.t ->
  Ed25519.Public_key.t tzresult Lwt.t

(**/**)

val get_contract_delegate:
  Raw_context.t -> Contract_repr.t -> Ed25519.Public_key_hash.t option tzresult Lwt.t

val value: Raw_context.t -> Tez_repr.t
