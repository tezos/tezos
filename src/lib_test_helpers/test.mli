(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

module Make(Error : sig
    type error
    val pp_print_error: Format.formatter -> error list -> unit
  end) : sig

  val run : string -> (string * (string -> (unit, Error.error list) result Lwt.t)) list -> unit

end
