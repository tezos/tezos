(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type cors = Resto_cohttp.Cors.t = {
  allowed_headers : string list ;
  allowed_origins : string list ;
}

include Resto_directory
module Directory = Resto_directory.Make(RPC.Data)

include Resto_cohttp.Server.Make(RPC.Data)(Logging.RPC)
