(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type cors = Resto_cohttp.Cors.t = {
  allowed_headers : string list ;
  allowed_origins : string list ;
}

include Resto_cohttp.Server.Make(RPC_encoding)(Logging.RPC)
