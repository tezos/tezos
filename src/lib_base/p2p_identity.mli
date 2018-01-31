(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type t = {
  peer_id : P2p_peer.Id.t ;
  public_key : Crypto_box.public_key ;
  secret_key : Crypto_box.secret_key ;
  proof_of_work_stamp : Crypto_box.nonce ;
}
(** Type of an identity, comprising a peer_id, a crypto keypair, and a
    proof of work stamp with enough difficulty so that the network
    accept this identity as genuine. *)

val encoding : t Data_encoding.t

val generate : Crypto_box.target -> t
(** [generate target] is a freshly minted identity whose proof of
    work stamp difficulty is at least equal to [target]. *)

val generate_with_animation :
  Format.formatter -> Crypto_box.target -> t
(** [generate_with_animation ppf target] is a freshly minted identity
    whose proof of work stamp difficulty is at least equal to [target]. *)
