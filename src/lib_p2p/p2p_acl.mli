(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(**
   This module implements four Access Control Lists:
   - ip greylist is a set of banned ip addresses automatically added by
     the p2p layer.
   - peer_id greylist is a set of banned peers ids automatically added by
     the p2p layer.
   - ip blacklist is a set of ip addresses manually added by the node admin.
   - peers blacklist is a set of peers ids manually added by the node admin.

   IP greylists use a time based GC to periodically remove entries from
   the table, while peer_id grey lists are built using a ring structure,
   where peers are removed from the table when removed from the fixed size
   ring. Other tables are user defined and static.

*)

type t

(** [create size] is a set of four ACLs (see above) with the peer_id
    greylist being a ring buffer of size [size]. *)
val create : int -> t

(** [banned_addr t addr] is [true] if [addr] is blacklisted or
    greylisted. *)
val banned_addr : t -> P2p_addr.t -> bool

(** [banned_peer t peer_id] is [true] if peer with id [peer_id] is
    blacklisted or greylisted. *)
val banned_peer : t -> P2p_peer.Id.t -> bool

(** [clear t] clears all four ACLs. *)
val clear : t -> unit

module IPGreylist : sig

  (** [add t addr] adds [addr] to the address greylist. *)
  val add: t -> P2p_addr.t -> unit

  (** [gc time] removes all banned peers older than the given time in
      seconds. *)
  val gc: t -> delay:float -> unit

  val encoding: P2p_addr.t list Data_encoding.t

end

module IPBlacklist : sig

  val add: t -> P2p_addr.t -> unit
  val remove: t -> P2p_addr.t -> unit

end

module PeerBlacklist : sig

  val add: t -> P2p_peer.Id.t -> unit
  val remove: t -> P2p_peer.Id.t -> unit

end


module PeerGreylist : sig

  val add: t -> P2p_peer.Id.t -> unit

end

(** / *)

module PeerRing : Ring.TABLE with type v = P2p_peer.Id.t

module IpSet : sig
  type t
  val empty: t
  val add : Ipaddr.V6.t -> Time.t -> t -> t
  val add_prefix : Ipaddr.V6.Prefix.t -> Time.t -> t -> t
  val remove : Ipaddr.V6.t -> t -> t
  val remove_prefix : Ipaddr.V6.Prefix.t -> t -> t
  val mem : Ipaddr.V6.t -> t -> bool
  val fold: (Ipaddr.V6.Prefix.t -> Time.t -> 'a -> 'a) -> t -> 'a -> 'a
  val pp : Format.formatter -> t -> unit
  val gc : t -> delay:float -> t
end

module IpTable : Hashtbl.S with type key = Ipaddr.V6.t
