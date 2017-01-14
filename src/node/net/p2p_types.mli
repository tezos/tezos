(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

module Canceler = Lwt_utils.Canceler


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
