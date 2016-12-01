(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

module Arg = Resto.Arg
module Path = Resto.Path
module Description = Resto.Description
let read_answer = Resto.read_answer
let forge_request = Resto.forge_request
let service ?description ~input ~output path =
  Resto.service
    ?description
    ~input:(Data_encoding.Json.convert input)
    ~output:(Data_encoding.Json.convert output)
    path
type ('prefix, 'params, 'input, 'output) service =
  ('prefix, 'params, 'input, 'output) Resto.service

include RestoDirectory
