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

let sandbox_parameters =
  match Data_encoding.Json.from_string {json|
{
    "genesis_pubkey":
      "edpkuSLWfVU1Vq7Jg9FucPyKmma6otcMHac9zG4oU1KMHSTBpJuGQ2"
}
|json} with
  | Error err -> raise (Failure err)
  | Ok json ->
      Data_encoding.Binary.to_bytes_exn Data_encoding.json json

let protocol_parameters =
  let json_result =
    Data_encoding.Json.from_string {json|
{ "bootstrap_accounts": [
    [ "edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav", "4000000000000" ],
    [ "edpktzNbDAUjUk697W7gYg2CRuBQjyPxbEg8dLccYYwKSKvkPvjtV9", "4000000000000" ],
    [ "edpkuTXkJDGcFd5nh6VvMz8phXxU3Bi7h6hqgywNFi1vZTfQNnS1RV", "4000000000000" ],
    [ "edpkuFrRoDSEbJYgxRtLx2ps82UdaYc1WwfS9sE11yhauZt5DgCHbU", "4000000000000" ],
    [ "edpkv8EUUH68jmo3f7Um5PezmfGrRF24gnfLpH3sVNwJnV5bVCxL2n", "4000000000000" ]
  ],
  "commitments": [
    [ "btz1bRL4X5BWo2Fj4EsBdUwexXqgTf75uf1qa", "23932454669343" ],
    [ "btz1SxjV1syBgftgKy721czKi3arVkVwYUFSv", "72954577464032" ],
    [ "btz1LtoNCjiW23txBTenALaf5H6NKF1L3c1gw", "217487035428348" ],
    [ "btz1SUd3mMhEBcWudrn8u361MVAec4WYCcFoy", "4092742372031" ],
    [ "btz1MvBXf4orko1tsGmzkjLbpYSgnwUjEe81r", "17590039016550" ],
    [ "btz1LoDZ3zsjgG3k3cqTpUMc9bsXbchu9qMXT", "26322312350555" ],
    [ "btz1RMfq456hFV5AeDiZcQuZhoMv2dMpb9hpP", "244951387881443" ],
    [ "btz1Y9roTh4A7PsMBkp8AgdVFrqUDNaBE59y1", "80065050465525" ],
    [ "btz1Q1N2ePwhVw5ED3aaRVek6EBzYs1GDkSVD", "3569618927693" ],
    [ "btz1VFFVsVMYHd5WfaDTAt92BeQYGK8Ri4eLy", "9034781424478" ]
  ],
  "time_between_blocks" : [ 1, 0 ],
  "blocks_per_cycle" : 4,
  "blocks_per_roll_snapshot" : 2,
  "first_free_baking_slot" : 4
}
|json} in
  match json_result with
  | Error err -> raise (Failure err)
  | Ok json ->
      Data_encoding.Binary.to_bytes_exn Data_encoding.json json


let main () =
  let context = Tezos_protocol_environment_memory.Context.empty in
  Tezos_protocol_environment_memory.Context.set context
    ["sandbox_parameter"] sandbox_parameters >>= fun context ->
  Tezos_protocol_environment_memory.Context.set context
    ["protocol_parameters"] protocol_parameters >>= fun context ->
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
    Data_encoding.Binary.to_bytes_exn
      Alpha_context.Block_header.contents_encoding
      (Helpers_block.get_protocol_data 0 true) in
  let tezos_header = { Block_header.shell = header ; protocol_data } in
  Proto_alpha.init context header >>=? fun validation ->
  let hash = Block_header.hash tezos_header in
  Alpha_context.prepare
    ~level: (Int32.succ header.level)
    ~timestamp: header.timestamp
    ~fitness: header.fitness
    validation.context >>=? fun tezos_context ->
  return
    { Helpers_block.tezos_header ; hash ; level = tezos_header.shell.level ;
      validation ; tezos_context }
