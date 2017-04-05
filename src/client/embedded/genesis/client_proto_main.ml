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

let call_service1 cctxt s block a1 =
  Client_rpcs.call_service1 cctxt
    (s Node_rpc_services.Blocks.proto_path) block a1

let call_error_service1 cctxt s block a1 =
  call_service1 cctxt s block a1 >>= function
  | Ok (Error _ as err) -> Lwt.return (wrap_error err)
  | Ok (Ok v) -> return v
  | Error _ as err -> Lwt.return err

let forge_block
    cctxt block net_id ?(timestamp = Time.now ()) command fitness =
  Client_blocks.get_block_hash cctxt block >>=? fun pred ->
  call_service1 cctxt
    Services.Forge.block block
    ((net_id, pred, timestamp, fitness), command)

let mine cctxt ?timestamp block command fitness seckey =
  Client_blocks.get_block_info cctxt.rpc_config block >>=? fun bi ->
  forge_block cctxt.rpc_config ?timestamp block bi.net command fitness >>=? fun blk ->
  let signed_blk = Environment.Ed25519.Signature.append seckey blk in
  Client_node_rpcs.inject_block cctxt.rpc_config signed_blk [[]] >>=? fun hash ->
  cctxt.answer "Injected %a" Block_hash.pp_short hash >>= fun () ->
  return ()

let handle_error cctxt = function
  | Ok res ->
      Lwt.return res
  | Error exns ->
      pp_print_error Format.err_formatter exns ;
      cctxt.Client_commands.error "%s" "cannot continue"

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
         mine cctxt ?timestamp cctxt.config.block
           (Activate hash) fitness seckey
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
      mine cctxt ?timestamp cctxt.config.block
        (Activate_testnet hash) fitness seckey
    end ;

  ]

let () =
  Client_commands.register protocol @@
  commands ()
