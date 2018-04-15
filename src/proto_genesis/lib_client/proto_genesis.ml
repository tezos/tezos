(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

module Name = struct let name = "genesis" end
module Genesis_environment = Tezos_protocol_environment_faked.MakeV1(Name)()
module Proto = Tezos_protocol_genesis.Functor.Make(Genesis_environment)
module Block_services = struct
  include Block_services
  include Block_services.Make(Proto)(Proto)
end
include Proto
