(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

module Raw = Tezos_embedded_protocol_demo

module Environment = Tezos_protocol_environment.Make(Raw.Register.Name)()
module P = Raw.Functor.Make(Environment)

include P
include Updater.LiftProtocol(Raw.Register.Name)(Environment)(P)
