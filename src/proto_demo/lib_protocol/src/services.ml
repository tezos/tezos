(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

let echo_service custom_root =
  RPC_service.post_service
    ~description: "An dummy echo service"
    ~query: RPC_query.empty
    ~input: Data_encoding.(obj1 (req "msg" string))
    ~output: Data_encoding.(obj1 (req "msg" string))
    RPC_path.(custom_root / "echo")

let failing_service custom_root =
  RPC_service.post_service
    ~description: "A failing service"
    ~query: RPC_query.empty
    ~input: Data_encoding.(obj1 (req "arg" int31))
    ~output: Data_encoding.empty
    RPC_path.(custom_root / "failing")

let rpc_services : Updater.rpc_context RPC_directory.t =
  let dir = RPC_directory.empty in
  let dir =
    RPC_directory.register
      dir
      (failing_service RPC_path.open_root)
      (fun _ctxt () x -> Error.demo_error x)
  in
  let dir =
    RPC_directory.register
      dir
      (echo_service RPC_path.open_root)
      (fun _ctxt () x -> return x)
  in
  dir
