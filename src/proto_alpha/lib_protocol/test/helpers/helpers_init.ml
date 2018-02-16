(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Proto_alpha
open Error_monad

let get_sandbox () =
  Lwt_utils_unix.Json.read_file
    "src/proto_alpha/lib_protocol/test/sandbox.json" >>= function
  | Ok x -> Lwt.return x
  | Error _ ->
      Lwt_utils_unix.Json.read_file "test/sandbox.json" >>= fun x ->
      Lwt.return @@ Helpers_assert.no_error ~msg:__LOC__ x

let main () =
  let context = Tezos_protocol_environment_client.Mem_context.empty in
  get_sandbox () >>= fun json ->
  Main.configure_sandbox context @@ Some json >>=? fun context ->
  let genesis_hash =
    Block_hash.of_b58check_exn
      "BLockGenesisGenesisGenesisGenesisGenesisCCCCCeZiLHU" in
  let header = {
    Block_header.level = 1l ;
    proto_level = 0 ;
    predecessor = genesis_hash ;
    timestamp = Time.of_notation_exn "2017-09-22T00:00:00Z" ;
    validation_passes = List.length Proto_alpha.Main.validation_passes ;
    operations_hash = Helpers_misc.no_ops_hash ;
    fitness = [] ; (* don't care *)
    context = Context_hash.zero ; (* don't care *)
  } in
  let protocol_data =
    Data_encoding.Binary.to_bytes
      Alpha_context.Block_header.protocol_data_encoding
      (Helpers_block.get_protocol_data 0) in
  let tezos_header = { Block_header.shell = header ; protocol_data } in
  Proto_alpha.Main.begin_construction
    ~predecessor_context: context
    ~predecessor_fitness:[]
    ~predecessor_timestamp:Time.epoch
    ~predecessor_level: 0l
    ~predecessor: genesis_hash
    ~timestamp: header.timestamp
    ~protocol_data
    () >>=? fun vstate ->
  let hash = Block_header.hash tezos_header in
  Proto_alpha.Main.finalize_block vstate >>=? fun validation ->
  Alpha_context.init
    ~level: (Int32.succ header.level)
    ~timestamp: header.timestamp
    ~fitness: header.fitness
    validation.context >>=? fun tezos_context ->
  return
    { Helpers_block.tezos_header ; hash ; level = tezos_header.shell.level ;
      validation ; tezos_context }
