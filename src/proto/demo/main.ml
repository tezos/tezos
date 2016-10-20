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

let max_block_length = 42
let max_number_of_operations = 42

let parse_operation h _ = Ok h

let compare_operations _ _ = 0

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

type validation_state = Context.t

let current_context ctxt =
  return ctxt

let precheck_block
    ~ancestor_context:_
    ~ancestor_timestamp:_
    _raw_block =
  return ()

let begin_application
    ~predecessor_context:ctxt
    ~predecessor_timestamp:_
    _raw_block =
  return ctxt

let begin_construction
    ~predecessor_context:ctxt
    ~predecessor_timestamp:_
    ~predecessor:_
    ~timestamp:_ =
  return ctxt

let apply_operation ctxt _ =
  return ctxt

let finalize_block ctxt =
  Fitness.increase ctxt >>=? fun ctxt ->
  Fitness.get ctxt >>=? fun fitness ->
  let commit_message =
    Format.asprintf "fitness <- %Ld" fitness in
  Context.set_commit_message ctxt commit_message >>= fun ctxt ->
  return ctxt

let rpc_services = Services.rpc_services

let configure_sandbox ctxt _ = Lwt.return (Ok ctxt)
