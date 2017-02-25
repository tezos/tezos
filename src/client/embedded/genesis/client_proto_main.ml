(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

let protocol =
  Protocol_hash.of_b58check
    "ProtoGenesisGenesisGenesisGenesisGenesisGenesk612im"

let call_service1 cctxt s block a1 =
  Client_node_rpcs.call_service1 cctxt
    (s Node_rpc_services.Blocks.proto_path) block a1

let call_error_service1 cctxt s block a1 =
  call_service1 cctxt s block a1 >|= wrap_error

let forge_block
    cctxt block net_id ?(timestamp = Time.now ()) command fitness =
  Client_blocks.get_block_hash cctxt block >>= fun pred ->
  call_service1 cctxt
    Services.Forge.block block
    ((net_id, pred, timestamp, fitness), command)

let mine cctxt block command fitness seckey =
  Client_blocks.get_block_info cctxt block >>= fun bi ->
  forge_block cctxt block bi.net command fitness >>= fun blk ->
  let signed_blk = Environment.Ed25519.append_signature seckey blk in
  Client_node_rpcs.inject_block cctxt ~wait:true signed_blk >>=? fun hash ->
  cctxt.answer "Injected %a" Block_hash.pp_short hash >>= fun () ->
  return ()

let handle_error cctxt = function
  | Ok res ->
      Lwt.return res
  | Error exns ->
      pp_print_error Format.err_formatter exns ;
      cctxt.Client_commands.error "%s" "cannot continue"

let commands () =
  let open Cli_entries in
  [
    command ~desc: "Activate a protocol" begin
      prefixes [ "activate" ; "protocol" ] @@
      param ~name:"version" ~desc:"Protocol version (b58check)"
        (fun _ p -> Lwt.return @@ Protocol_hash.of_b58check p) @@
      prefixes [ "with" ; "fitness" ] @@
      param ~name:"fitness"
        ~desc:"Hardcoded fitness of the first block (integer)"
        (fun _ p -> Lwt.return (Int64.of_string p)) @@
      prefixes [ "and" ; "key" ] @@
      param ~name:"password" ~desc:"Dictator's key"
        (fun _ key ->
           Lwt.return (Environment.Ed25519.secret_key_of_b58check key))
        stop
    end
      (fun hash fitness seckey cctxt ->
         let block = Client_config.block () in
         mine cctxt block (Activate hash) fitness seckey >>= handle_error cctxt)
    ;
    command ~desc: "Fork a test protocol" begin
      prefixes [ "fork" ; "test" ; "protocol" ] @@
      param ~name:"version" ~desc:"Protocol version (b58check)"
        (fun _ p -> Lwt.return (Protocol_hash.of_b58check p)) @@
      prefixes [ "with" ; "fitness" ] @@
      param ~name:"fitness"
        ~desc:"Hardcoded fitness of the first block (integer)"
        (fun _ p -> Lwt.return (Int64.of_string p)) @@
      prefixes [ "and" ; "key" ] @@
      param ~name:"password" ~desc:"Dictator's key"
        (fun _ key ->
           Lwt.return (Environment.Ed25519.secret_key_of_b58check key))
        stop
    end
      (fun hash fitness seckey cctxt ->
         let block = Client_config.block () in
         mine cctxt block (Activate_testnet hash) fitness seckey >>= handle_error cctxt) ;
  ]

let () =
  Client_commands.register protocol @@
  commands ()
