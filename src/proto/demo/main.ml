(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type operation = Operation_hash.t
let max_operation_data_length = 42

type block_header = unit

let max_block_header_length = 42
let max_number_of_operations = 42

let parse_block_header _ = Ok ()
let parse_operation h _ = Ok h

let fitness_key = ["v1";"store";"fitness"]

let get_fitness ctxt =
  Context.get ctxt fitness_key >>= function
  | None -> Lwt.return 0L
  | Some b ->
      match Data_encoding.Binary.of_bytes Data_encoding.int64 b with
      | None -> Lwt.return 0L
      | Some v -> Lwt.return v

let set_fitness ctxt v =
  Context.set ctxt fitness_key @@
  Data_encoding.Binary.to_bytes Data_encoding.int64 v

let int64_to_bytes i =
  let b = MBytes.create 8 in
  MBytes.set_int64 b 0 i;
  b

let fitness ctxt =
  get_fitness ctxt >|= fun v ->
  [ MBytes.of_string "\000" ;
    int64_to_bytes v ]

let increase_fitness ctxt =
  get_fitness ctxt >>= fun v ->
  set_fitness ctxt (Int64.succ v) >>= fun ctxt ->
  Lwt.return ctxt

let apply ctxt () _operations =
  increase_fitness ctxt >>= fun ctxt ->
  return ctxt

let preapply context _block_pred _timestamp _sort operations =
  Lwt.return
    (Ok
       (context,
        { Updater.applied = List.map (fun h -> h) operations;
          refused = Operation_hash_map.empty;
          branch_delayed = Operation_hash_map.empty;
          branch_refused = Operation_hash_map.empty;
        }))

let rpc_services = Services.rpc_services

let configure_sandbox ctxt _ = Lwt.return (Ok ctxt)
