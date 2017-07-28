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

type validation_state = {
  context : Context.t ;
  fitness : Int64.t ;
}

let current_context { context } =
  return context

module Fitness = struct

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
    [ int64_to_bytes fitness ]

  let to_int64 = function
    | [ fitness ] -> int64_of_bytes fitness
    | [] -> return 0L
    | _ -> fail Invalid_fitness

  let get { fitness } = fitness

end

let precheck_block
    ~ancestor_context:_
    ~ancestor_timestamp:_
    raw_block =
  Fitness.to_int64 raw_block.Block_header.shell.fitness >>=? fun _ ->
  return ()

let begin_application
    ~predecessor_context:context
    ~predecessor_timestamp:_
    ~predecessor_fitness:_
    raw_block =
  Fitness.to_int64 raw_block.Block_header.shell.fitness >>=? fun fitness ->
  return { context ; fitness }

let begin_construction
    ~predecessor_context:context
    ~predecessor_timestamp:_
    ~predecessor_level:_
    ~predecessor_fitness:pred_fitness
    ~predecessor:_
    ~timestamp:_
    ?proto_header:_ () =
  Fitness.to_int64 pred_fitness >>=? function pred_fitness ->
  let fitness = Int64.succ pred_fitness in
  return { context ; fitness }

let apply_operation ctxt _ =
  return ctxt

let finalize_block ctxt =
  let fitness = Fitness.get ctxt in
  let message = Some (Format.asprintf "fitness <- %Ld" fitness) in
  let fitness = Fitness.from_int64 fitness in
  return { Updater.message ; context = ctxt.context ; fitness ;
           max_operations_ttl = 0 }

let rpc_services = Services.rpc_services

let configure_sandbox ctxt _ = Lwt.return (Ok ctxt)
