(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

module type S = sig
  include Irmin.RO
  val update: t -> key -> value -> t Lwt.t
  val remove: t -> key -> t Lwt.t
  val list: t -> key -> key list Lwt.t
  val remove_rec: t -> key -> t Lwt.t
  val empty: t
  type db
  val of_path: db -> key -> t Lwt.t
  val update_path: db -> key -> t -> unit Lwt.t
end

module Make (S: Irmin.S):
  S with type db = S.t
     and type key = S.key
     and type value = S.value
