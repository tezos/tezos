(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

module Make(Param : sig val name: string end)() =
  Tezos_base.Protocol_environment.MakeV1
    (Param)(Mem_context)(Fake_updater.Make(Mem_context))()
