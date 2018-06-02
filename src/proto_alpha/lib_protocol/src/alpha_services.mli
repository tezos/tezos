(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Alpha_context

module Nonce : sig

  type info =
    | Revealed of Nonce.t
    | Missing of Nonce_hash.t
    | Forgotten

  val get:
    'a #RPC_context.simple ->
    'a -> Raw_level.t -> info shell_tzresult Lwt.t

end

module Contract = Contract_services
module Constants = Constants_services
module Delegate = Delegate_services
module Helpers = Helpers_services
module Forge = Helpers_services.Forge
module Parse = Helpers_services.Parse

val register: unit -> unit
