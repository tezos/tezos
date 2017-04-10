(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
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
    case
      (obj1 (req "ok" encoding))
      (function Ok x -> Some x | _ -> None)
      (fun x -> Ok x) ;
    case
      (obj1 (req "error" error_encoding))
      (function Error x -> Some x | _ -> None)
      (fun x -> Error x) ;
  ]

let echo_service custom_root =
  RPC.service
    ~description: "An dummy echo service"
    ~input: Data_encoding.(obj1 (req "msg" string))
    ~output: Data_encoding.(obj1 (req "msg" string))
    RPC.Path.(custom_root / "echo")

let failing_service custom_root =
  RPC.service
    ~description: "A failing service"
    ~input: Data_encoding.(obj1 (req "arg" int31))
    ~output: (wrap_tzerror Data_encoding.empty)
    RPC.Path.(custom_root / "failing")

let rpc_services : Updater.rpc_context RPC.directory =
  let dir = RPC.empty in
  let dir =
    RPC.register
      dir
      (failing_service RPC.Path.root)
      (fun _ctxt x -> Error.demo_error x >>= RPC.Answer.return)
  in
  let dir =
    RPC.register
      dir
      (echo_service RPC.Path.root)
      (fun _ctxt x -> RPC.Answer.return x)
  in
  dir
