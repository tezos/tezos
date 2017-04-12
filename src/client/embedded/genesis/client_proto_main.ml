(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Client_commands

let protocol =
  Protocol_hash.of_b58check_exn
    "ProtoGenesisGenesisGenesisGenesisGenesisGenesk612im"

let call_service1 rpc_config s block a1 =
  Client_rpcs.call_service1 rpc_config
    (s Node_rpc_services.Blocks.proto_path) block a1

let call_error_service1 rpc_config s block a1 =
  call_service1 rpc_config s block a1 >>= function
  | Ok (Error _ as err) -> Lwt.return (wrap_error err)
  | Ok (Ok v) -> return v
  | Error _ as err -> Lwt.return err

let forge_block
    rpc_config block net_id ?(timestamp = Time.now ()) command fitness =
  Client_blocks.get_block_hash rpc_config block >>=? fun pred ->
  Client_node_rpcs.Blocks.level rpc_config block >>=? fun level ->
  call_service1 rpc_config
    Services.Forge.block block
    ((net_id, Int32.succ level, 1, pred, timestamp, fitness), command)

let mine rpc_config ?timestamp block command fitness seckey =
  Client_blocks.get_block_info rpc_config block >>=? fun bi ->
  forge_block
    rpc_config ?timestamp block bi.net_id command fitness >>=? fun blk ->
  let signed_blk = Environment.Ed25519.Signature.append seckey blk in
  Client_node_rpcs.inject_block rpc_config signed_blk [[]]

let commands () =
  let timestamp = ref None in
  let args =
    [ "-timestamp",
      Arg.String (fun t -> timestamp := Some (Time.of_notation_exn t)),
      "Set the timestamp of the block (and initial time of the chain)" ] in
  let open Cli_entries in
  [

    command ~args ~desc: "Activate a protocol" begin
      prefixes [ "activate" ; "protocol" ] @@
      Protocol_hash.param ~name:"version" ~desc:"Protocol version (b58check)" @@
      prefixes [ "with" ; "fitness" ] @@
      param ~name:"fitness"
        ~desc:"Hardcoded fitness of the first block (integer)"
        (fun _ p ->
           try return (Int64.of_string p)
           with _ -> failwith "Cannot read int64") @@
      prefixes [ "and" ; "key" ] @@
      Client_keys.Secret_key.source_param
        ~name:"password" ~desc:"Dictator's key" @@
      stop
    end begin fun hash fitness seckey cctxt ->
         let timestamp = !timestamp in
         let fitness =
           Client_embedded_proto_alpha.Fitness_repr.from_int64 fitness in
         mine cctxt.rpc_config ?timestamp cctxt.config.block
           (Activate hash) fitness seckey >>=? fun hash ->
         cctxt.answer "Injected %a" Block_hash.pp_short hash >>= fun () ->
         return ()
    end ;

    command ~args ~desc: "Fork a test protocol" begin
      prefixes [ "fork" ; "test" ; "protocol" ] @@
      Protocol_hash.param ~name:"version" ~desc:"Protocol version (b58check)" @@
      prefixes [ "with" ; "fitness" ] @@
      param ~name:"fitness"
        ~desc:"Hardcoded fitness of the first block (integer)"
        (fun _ p ->
           try return (Int64.of_string p)
           with _ -> failwith "Cannot read int64") @@
      prefixes [ "and" ; "key" ] @@
      Environment.Ed25519.Secret_key.param
        ~name:"password" ~desc:"Dictator's key" @@
      stop
    end begin fun hash fitness seckey cctxt ->
      let timestamp = !timestamp in
      let fitness =
        Client_embedded_proto_alpha.Fitness_repr.from_int64 fitness in
      mine cctxt.rpc_config ?timestamp cctxt.config.block
        (Activate_testnet (hash, Int64.mul 24L 3600L))
        fitness seckey >>=? fun hash ->
      cctxt.answer "Injected %a" Block_hash.pp_short hash >>= fun () ->
      return ()
    end ;

  ]

let () =
  Client_commands.register protocol @@
  commands ()
