(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Data_encoding

let protocols_arg = Protocol_hash.rpc_arg

let contents =
  RPC_service.post_service
    ~query: RPC_query.empty
    ~input: empty
    ~output:
      (obj1 (req "data"
               (describe ~title: "Tezos protocol"
                  (Protocol.encoding))))
    ~error: Data_encoding.empty
    RPC_path.(root / "protocols" /: protocols_arg)

type list_param = {
  contents: bool option ;
  monitor: bool option ;
}

let list_param_encoding =
  conv
    (fun {contents; monitor} -> (contents, monitor))
    (fun (contents, monitor) -> {contents; monitor})
    (obj2
       (opt "contents" bool)
       (opt "monitor" bool))

let list =
  RPC_service.post_service
    ~query: RPC_query.empty
    ~input: list_param_encoding
    ~output:
      (obj1
         (req "protocols"
            (list
               (obj2
                  (req "hash" Protocol_hash.encoding)
                  (opt "contents"
                     (dynamic_size Protocol.encoding)))
            )))
    ~error: Data_encoding.empty
    RPC_path.(root / "protocols")
