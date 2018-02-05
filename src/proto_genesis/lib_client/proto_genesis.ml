(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

module Name = struct let name = "genesis" end
module Environment = Tezos_protocol_environment_client.Fake.Make(Name)()
include Tezos_protocol_genesis.Functor.Make(Environment)
