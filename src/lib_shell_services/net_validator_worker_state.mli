(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

module Request : sig
  type view = Block_hash.t
  val encoding : view Data_encoding.encoding
  val pp : Format.formatter -> view -> unit
end

module Event : sig
  type update =
    | Ignored_head
    | Branch_switch
    | Head_incrememt
  type t =
    | Processed_block of
        { request : Request.view ;
          request_status : Worker_types.request_status ;
          update : update ;
          fitness : Fitness.t }
    | Could_not_switch_testnet of error list
  val level : t -> Logging.level
  val encoding : t Data_encoding.encoding
  val pp : Format.formatter -> t -> unit
end

module Worker_state : sig
  type view =
    { active_peers : P2p_peer.Id.t list ;
      bootstrapped_peers : P2p_peer.Id.t list ;
      bootstrapped : bool }
  val encoding : view Data_encoding.encoding
  val pp : Format.formatter -> view -> unit
end
