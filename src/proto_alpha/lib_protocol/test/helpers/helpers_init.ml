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
      "edpkuSLWfVU1Vq7Jg9FucPyKmma6otcMHac9zG4oU1KMHSTBpJuGQ2",
    "slot_durations" : [ 1, 0 ],
    "cycle_length" : 4,
    "block_per_roll_snapshot" : 2,
    "time_before_reward" : 1,
    "first_free_baking_slot" : 4
}
|json} with
  | Error err -> raise (Failure err)
  | Ok json ->
      Data_encoding.Binary.to_bytes Data_encoding.json json

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
    [ "tz1fkmDXEQdua3u71JBVh4eGUGqK4t1G8xhZ", "btz1T77Ly5U1bWNBR5KzDSgNFST5Bh5F1eB6g", "1868898542104130027" ],
    [ "tz1doNkK6RKaRswsuKJV4erT6HauYSm9fuHi", "btz1QxTPgszARgWioEor3eMehxW3osfhw3KoJ", "517697389496079974" ],
    [ "tz1g6zFsci4YR8p1MJrkyc6wAKizR6mqJmyQ", "btz1NUYc1tV5VBksMNuQG4AuZF9Xudh1sDJni", "962290491831710023" ],
    [ "tz1MpbcwGFWVBBWoxwm6iQH5Hzh9mCXbnETJ", "btz1Wwifd8vbQqnbuzSbQLvJEjQ9FUoxVJm68", "1233665184704419921" ],
    [ "tz1U4t2PmX5cZVUui4BNaiRVokLa6AxB5G9Z", "btz1RrFkp9GmnypoNGRSyURkBQNUs4PPYG8SR", "131959324067470008" ],
    [ "tz1RUxPjviua4XJM78XjSKGUCAS9R3y8Bdof", "btz1hozabzP9HdRakJddzyea7DgPHzJ5PB37N", "112378240876120002" ],
    [ "tz1fJPeueQKJrTj2SFV2PmyCinLYJKXCEMf4", "btz1j1k4nrZB4r8RpTYiy8zbi3Fappopkb8ZF", "1060667014046690017" ],
    [ "tz1iDPZLxcGf5CqCNpTuuMdtu3zKpJ6HvvFR", "btz1TaSfoSNhFoqwqbPC9iC19rN24KJtB7skD", "71300478465380003" ],
    [ "tz1i2kbtVu65dP739qLGRpJNujRM8pdpkH3p", "btz1USqQRuvASPXwseXkGTWeWv4dn3VwMVPEk", "283380756728119992" ],
    [ "tz1LKGCg6ESJLDviHkT8Jc7tUwjw4h3d9MaF", "btz1YwCKMbBLRL1qkBjAHGCwjbWDqiTAEFpbw", "1357762577679880028" ]
  ]
}
|json} in
  match json_result with
  | Error err -> raise (Failure err)
  | Ok json ->
      Data_encoding.Binary.to_bytes Data_encoding.json json


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
    Data_encoding.Binary.to_bytes
      Alpha_context.Block_header.protocol_data_encoding
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
