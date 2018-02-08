(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

let service =
  RPC_service.post_service
    ~description: "Schema for all the RPC errors from the shell"
    ~query: RPC_query.empty
    ~input: Data_encoding.empty
    ~output: Data_encoding.json_schema
    RPC_path.(root / "errors")

let encoding =
  let { RPC_service.meth ; uri ; _ } =
    RPC_service.forge_request service () () in
  let open Data_encoding in
  describe
    ~description:
      (Printf.sprintf
         "The full list of error is available with \
          the global RPC `%s %s`"
         (RPC_service.string_of_meth meth) (Uri.path_and_query uri))
    (conv
       ~schema:Json_schema.any
       (fun exn -> `A (List.map Error_monad.json_of_error exn))
       (function `A exns -> List.map Error_monad.error_of_json exns | _ -> [])
       json)

let wrap param_encoding =
  let open Data_encoding in
  union [
    case (Tag 0)
      (obj1 (req "ok" param_encoding))
      (function Ok x -> Some x | _ -> None)
      (fun x -> Ok x) ;
    case (Tag 1)
      (obj1 (req "error" encoding))
      (function Error x -> Some x | _ -> None)
      (fun x -> Error x) ;
  ]

module F = struct
  open RPC_context
  let schema ctxt = make_call service ctxt () () ()
end
