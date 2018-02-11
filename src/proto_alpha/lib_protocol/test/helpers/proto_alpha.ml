(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

module Name = struct let name = "alpha" end
module Context =
  Tezos_protocol_environment_client.Mem_context
module Updater =
  Tezos_protocol_environment_client.Fake_updater.Make(Context)
module Alpha_environment =
  Tezos_base.Protocol_environment.MakeV1(Name)(Context)(Updater)()
include Tezos_protocol_alpha.Functor.Make(Alpha_environment)

module Error_monad = Alpha_environment.Error_monad
type proto_error = Error_monad.error
type 'a proto_tzresult = 'a Error_monad.tzresult

