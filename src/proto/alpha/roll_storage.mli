(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
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

val init : Storage.t -> Storage.t tzresult Lwt.t

val fold :
  Storage.t ->
  f:(Roll_repr.roll -> Contract_repr.t -> 'a -> 'a tzresult Lwt.t) ->
  'a -> 'a tzresult Lwt.t

val freeze_rolls_for_cycle :
  Storage.t -> Cycle_repr.t -> Storage.t tzresult Lwt.t

val clear_cycle :
  Storage.t -> Cycle_repr.t -> Storage.t tzresult Lwt.t

val mining_rights_owner :
  Storage.t -> Level_repr.t -> priority:int ->
  Ed25519.Public_key_hash.t tzresult Lwt.t

val endorsement_rights_owner :
  Storage.t -> Level_repr.t -> slot:int ->
  Ed25519.Public_key_hash.t tzresult Lwt.t

module Contract : sig

  val init :
    Storage.t -> Contract_repr.t -> Storage.t tzresult Lwt.t

  val add_amount :
    Storage.t -> Contract_repr.t -> Tez_repr.t -> Storage.t tzresult Lwt.t

  val remove_amount :
    Storage.t -> Contract_repr.t -> Tez_repr.t -> Storage.t tzresult Lwt.t

  val assert_empty : Storage.t -> Contract_repr.t -> unit tzresult Lwt.t

end

(**/**)

val get_contract_delegate:
  Storage.t -> Contract_repr.t -> Ed25519.Public_key_hash.t option tzresult Lwt.t
