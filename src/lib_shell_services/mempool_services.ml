(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Data_encoding



module S = struct

  let pending_operations =
    let operation_encoding =
      merge_objs
        (obj1 (req "hash" Operation_hash.encoding))
        Operation.encoding in
    (* TODO: branch_delayed/... *)
    RPC_service.post_service
      ~description:
        "List the not-yet-prevalidated operations."
      ~query: RPC_query.empty
      ~input: empty
      ~output:
        (conv
           (fun (preapplied, unprocessed) ->
              ({ preapplied with
                 Preapply_result.refused = Operation_hash.Map.empty },
               Operation_hash.Map.bindings unprocessed))
           (fun (preapplied, unprocessed) ->
              (preapplied,
               List.fold_right
                 (fun (h, op) m -> Operation_hash.Map.add h op m)
                 unprocessed Operation_hash.Map.empty))
           (merge_objs
              (dynamic_size
                 (Preapply_result.encoding RPC_error.encoding))
              (obj1 (req "unprocessed" (list (dynamic_size operation_encoding))))))
      RPC_path.(root / "mempool" / "pending_operations")

end

open RPC_context

let pending_operations ctxt = make_call S.pending_operations ctxt  () () () 
