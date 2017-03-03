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

type block = unit

let max_block_length = 42
let max_number_of_operations = 42

let parse_block _ _pred_timestamp = Ok ()
let parse_operation h _ = Ok h

module Fitness = struct

  let version_number = "\000"

  type error += Invalid_fitness
  type error += Invalid_fitness2

  let int64_to_bytes i =
    let b = MBytes.create 8 in
    MBytes.set_int64 b 0 i;
    b

  let int64_of_bytes b =
    if Compare.Int.(MBytes.length b <> 8) then
      fail Invalid_fitness2
    else
      return (MBytes.get_int64 b 0)

  let from_int64 fitness =
    [ MBytes.of_string version_number ;
      int64_to_bytes fitness ]

  let to_int64 = function
    | [ version ;
        fitness ]
      when Compare.String.
             (MBytes.to_string version = version_number) ->
        int64_of_bytes fitness
    | [] -> return 0L
    | _ -> fail Invalid_fitness

  let get ctxt =
    Context.get_fitness ctxt >>= fun fitness ->
    to_int64 fitness

  let set ctxt v =
    Context.set_fitness ctxt (from_int64 v) >>= fun ctxt ->
    Lwt.return ctxt

  let increase ctxt =
    get ctxt >>=? fun v ->
    set ctxt (Int64.succ v) >>= fun ctxt ->
    return ctxt

end

let apply ctxt () _operations =
  Fitness.increase ctxt >>=? fun ctxt ->
  Fitness.get ctxt >>=? fun fitness ->
  let commit_message =
    Format.asprintf "fitness <- %Ld" fitness in
  Context.set_commit_message ctxt commit_message >>= fun ctxt ->
  return ctxt

let preapply context _block_pred _sort operations =
  Lwt.return
    (Ok
       (context,
        { Updater.applied = List.map (fun h -> h) operations;
          refused = Operation_hash.Map.empty;
          branch_delayed = Operation_hash.Map.empty;
          branch_refused = Operation_hash.Map.empty;
        }))

let rpc_services = Services.rpc_services

let configure_sandbox ctxt _ = Lwt.return (Ok ctxt)
