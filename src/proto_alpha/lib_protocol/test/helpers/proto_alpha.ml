(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

module Name = struct let name = "alpha" end
module Context = Tezos_protocol_environment_memory.Context
module Alpha_environment = Tezos_protocol_environment_memory.MakeV1(Name)()
include Tezos_protocol_alpha.Functor.Make(Alpha_environment)

module Error_monad = Alpha_environment.Error_monad
type proto_error = Error_monad.error
type 'a proto_tzresult = 'a Error_monad.tzresult

