(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(**
   This module implements four tables
   - ip grey lists used to automatically ban a given ip addr
   - peer_id greylist used to automatically ban a given peer_id
   - ip black lists used to manually ban a given ip addr
   - peers black list used to manually trust a given peer_id

   IP greylists use a time based GC to periodically remove entries from
   the table, while peer_id grey lists are built using a ring structure,
   where peers are removed from the table when removed from the fixed size
   ring. Other tables are user defined and static.

*)


type t

(** Create a new ACL of given size *)
val create : int -> t

(** Check if an address is banned either temporally or permanently *)
val is_banned_addr : t -> P2p_addr.t -> bool

(** Check if a peer is banned either temporally or permanently *)
val is_banned_peer : t -> P2p_peer.Id.t -> bool

(** Reinitialize the Greylist tables *)
val greylist_clear : t -> unit

module IPGreylist : sig

  (* Add the given ip address to the ip greylist *)
  val add: t -> P2p_addr.t -> unit

  (** [gc time] removes all banned peers older than the given time in seconds
      The GC operation works only on the address set. Peers are removed
      from the ring in a round-robin fashion. If a address is removed
      by the GC from the greylist set, it could potentially
      persist in the peers' blacklist set until more peers are banned. *)
  val gc: t -> delay:float -> unit

  val encoding: P2p_addr.t list Data_encoding.t

end

module IPBlacklist : sig

  (* Add the given ip address to the ip blacklist *)
  val add: t -> P2p_addr.t -> unit

  (* Remove the given ip address to the ip blacklist *)
  val remove: t -> P2p_addr.t -> unit

end

module PeerBlacklist : sig

  (* Add the given ip address to the ip blacklist *)
  val add: t -> P2p_peer.Id.t -> unit

  (* Remove the given ip address to the ip blacklist *)
  val remove: t -> P2p_peer.Id.t -> unit

end


module PeerGreylist : sig

  (* Ban the given peer_id. It also add the given ip address to the blacklist. *)
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
