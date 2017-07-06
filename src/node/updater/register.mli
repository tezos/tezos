(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

module Make(Proto : Protocol_sigs.PACKED_PROTOCOL) : sig
  type Error_monad.error += Ecoproto_error of Proto.error list
  val wrap_error: 'a Proto.tzresult -> 'a tzresult
end

val register: (module Protocol_sigs.PACKED_PROTOCOL) -> unit
