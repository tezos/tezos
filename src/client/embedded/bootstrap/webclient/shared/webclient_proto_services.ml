(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

module type RPC_CONTEXT = sig
  type root
end

module Make (RPC_context : RPC_CONTEXT) = struct

  let box_result field enc =
    let open Data_encoding in
    obj1 (req field enc)

  let contracts =
    let input = Data_encoding.empty in
    let output = box_result "contracts" Data_encoding.(list string) in
    RPC.service ~input ~output RPC.Path.(root / "contracts")

  let hash =
    let input = Data_encoding.empty in
    let output = box_result "hash" Data_encoding.string in
    RPC.service ~input ~output RPC.Path.(root / "hash")

end
