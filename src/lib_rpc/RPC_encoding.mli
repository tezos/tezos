(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type schema = Data_encoding.json_schema * Data_encoding.Binary_schema.t

include Resto.ENCODING with type 'a t = 'a Data_encoding.t
                        and type schema := schema

