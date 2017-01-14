(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

module Canceler = Lwt_utils.Canceler

(** Protocol version *)

module Version : sig
  type t = {
    name : string ;
    major : int ;
    minor : int ;
  }
  (** Type of a protocol version. *)

  val encoding : t Data_encoding.t
  val common: t list -> t list -> t option
end


(** Gid, i.e. persistent peer identifier *)

module Gid : sig
  type t = Crypto_box.Public_key_hash.t
  (** Type of a gid, a public key hash. *)

  val compare : t -> t -> int
  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val encoding : t Data_encoding.t
  module Map : Map.S with type key = t
  module Set : Set.S with type elt = t
  module Table : Hashtbl.S with type key = t
end

type addr = Ipaddr.V6.t
type port = int


(** Point, i.e. socket address *)

module Point : sig
  type t = addr * port
  val compare : t -> t -> int
  val pp : Format.formatter -> t -> unit
  val pp_opt : Format.formatter -> t option -> unit
  val encoding : t Data_encoding.t
  val is_local : t -> bool
  val is_global : t -> bool
  val to_sockaddr : t -> Unix.sockaddr
  module Map : Map.S with type key = t
  module Set : Set.S with type elt = t
  module Table : Hashtbl.S with type key = t
end

(** Point representing a reachable socket address *)

module Id_point : sig
  type t = addr * port option
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val pp_opt : Format.formatter -> t option -> unit
  val encoding : t Data_encoding.t
  val is_local : t -> bool
  val is_global : t -> bool
  module Map : Map.S with type key = t
  module Set : Set.S with type elt = t
  module Table : Hashtbl.S with type key = t
end


(** Identity *)

module Identity : sig
  type t = {
    gid : Gid.t ;
    public_key : Crypto_box.public_key ;
    secret_key : Crypto_box.secret_key ;
    proof_of_work_stamp : Crypto_box.nonce ;
  }
  (** Type of an identity, comprising a gid, a crypto keypair, and a
      proof of work stamp with enough difficulty so that the network
      accept this identity as genuine. *)

  val encoding : t Data_encoding.t

  val generate : Crypto_box.target -> t
  (** [generate target] is a freshly minted identity whose proof of
      work stamp difficulty is at least equal to [target]. *)
end


(** Bandwidth usage statistics *)

module Stat : sig

  type t = {
    total_sent : int ;
    total_recv : int ;
    current_inflow : int ;
    current_outflow : int ;
  }

  val pp: Format.formatter -> t -> unit

end

(** Information about a connection *)

module Connection_info : sig

  type t = {
    incoming : bool;
    gid : Gid.t;
    id_point : Id_point.t;
    remote_socket_port : port;
    versions : Version.t list ;
  }

  val pp: Format.formatter -> t -> unit

end
