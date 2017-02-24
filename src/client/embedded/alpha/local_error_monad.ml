(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

module Shell_error_monad = Error_monad

module Error_monad = struct
  type error_category = [ `Branch | `Temporary | `Permanent ]
  include Shell_error_monad.Make()
end

type error += Ecoproto_error of Error_monad.error list

let wrap = function
  | Ok x -> Ok x
  | Error errors -> Error [Ecoproto_error errors]
