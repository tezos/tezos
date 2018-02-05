(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

let error_encoding =
  let open Data_encoding in
  describe
    ~description:
      "The full list of error is available with \
       the global RPC `/errors`"
    (conv
       (fun exn -> `A (List.map json_of_error exn))
       (function `A exns -> List.map error_of_json exns | _ -> [])
       json)

let wrap_tzerror encoding =
  let open Data_encoding in
  union [
    case (Tag 0)
      (obj1 (req "ok" encoding))
      (function Ok x -> Some x | _ -> None)
      (fun x -> Ok x) ;
    case (Tag 1)
      (obj1 (req "error" error_encoding))
      (function Error x -> Some x | _ -> None)
      (fun x -> Error x) ;
  ]

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
      ~error: Data_encoding.empty
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
        RPC_answer.return bytes) in
  dir
