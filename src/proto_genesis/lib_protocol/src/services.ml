(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

module Forge = struct
  let block custom_root =
    let open Data_encoding in
    RPC_service.post_service
      ~description: "Forge a block"
      ~query: RPC_query.empty
      ~input:
        (merge_objs
           (obj6
              (req "level" int32)
              (req "proto_level" uint8)
              (req "predecessor" Block_hash.encoding)
              (req "timestamp" Time.encoding)
              (req "fitness" Fitness.encoding)
              (req "context" Context_hash.encoding))
           Data.Command.encoding)
      ~output: (obj1 (req "payload" bytes))
      RPC_path.(custom_root / "helpers" / "forge" / "block")
end

let int64_to_bytes i =
  let b = MBytes.create 8 in
  MBytes.set_int64 b 0 i;
  b

let operations_hash =
  Operation_list_list_hash.compute []

let rpc_services : Updater.rpc_context RPC_directory.t =
  let dir = RPC_directory.empty in
  let dir =
    RPC_directory.register
      dir
      (Forge.block RPC_path.open_root)
      (fun _ctxt () ((level, proto_level, predecessor,
                      timestamp, fitness, context), command) ->
        let shell = { Block_header.level ; proto_level ; predecessor ;
                      timestamp ; fitness ; validation_passes = 0 ;
                      operations_hash ; context } in
        let bytes = Data.Command.forge shell command in
        return bytes) in
  dir
