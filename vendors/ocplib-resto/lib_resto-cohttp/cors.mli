(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type t = {
  allowed_headers : string list ;
  allowed_origins : string list ;
}

val default: t

val add_headers:
  Cohttp.Header.t -> t -> string option -> Cohttp.Header.t

