(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

module Name = struct let name = "demo" end
module Environment = Tezos_client_environment.Environment.Make(Name)()
include Tezos_protocol_demo.Functor.Make(Environment)
