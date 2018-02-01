(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Proto_genesis

let protocol =
  Protocol_hash.of_b58check_exn
    "ProtoGenesisGenesisGenesisGenesisGenesisGenesk612im"

let call_service1 rpc_config s block a1 =
  Client_rpcs.call_service1 rpc_config
    (s Block_services.proto_path) block a1

let call_error_service1 rpc_config s block a1 =
  call_service1 rpc_config s block a1 >>= function
  | Ok (Error _ as err) -> Lwt.return (Environment.wrap_error err)
  | Ok (Ok v) -> return v
  | Error _ as err -> Lwt.return err

let bake rpc_config ?(timestamp = Time.now ()) block command sk =
  let block = Client_rpcs.last_baked_block block in
  let proto_header = Data_encoding.Binary.to_bytes Data.Command.encoding command in
  Client_node_rpcs.Blocks.preapply
    rpc_config block ~timestamp ~proto_header [] >>=? fun { shell_header } ->
  let blk =
    Data_encoding.Binary.to_bytes Block_header.encoding
      { shell = shell_header ; proto = proto_header } in
  Client_keys.append sk blk >>=? fun signed_blk ->
  Client_node_rpcs.inject_block rpc_config signed_blk []

let int64_parameter =
  (Cli_entries.parameter (fun _ p ->
       try return (Int64.of_string p)
       with _ -> failwith "Cannot read int64"))

let int_parameter =
  (Cli_entries.parameter (fun _ p ->
       try return (int_of_string p)
       with _ -> failwith "Cannot read int"))

let commands () =
  let open Cli_entries in
  let args =
    args1
      (arg
         ~parameter:"-timestamp"
         ~placeholder:"date"
         ~doc:"Set the timestamp of the block (and initial time of the chain)"
         (parameter (fun _ t ->
              match (Time.of_notation t) with
              | None -> Error_monad.failwith "Could not parse value provided to -timestamp option"
              | Some t -> return t))) in
  [

    command ~desc: "Activate a protocol"
      args
      (prefixes [ "activate" ; "protocol" ]
       @@ Protocol_hash.param ~name:"version" ~desc:"Protocol version (b58check)"
       @@ prefixes [ "with" ; "fitness" ]
       @@ param ~name:"fitness"
         ~desc:"Hardcoded fitness of the first block (integer)"
         int64_parameter
       @@ prefixes [ "and" ; "passes" ]
       @@ param ~name:"passes"
         ~desc:"Hardcoded number of validation passes (integer)"
         int_parameter
       @@ prefixes [ "and" ; "key" ]
       @@ Client_keys.Secret_key.source_param
         ~name:"password" ~desc:"Dictator's key"
       @@ stop)
      begin fun timestamp hash fitness validation_passes sk (cctxt : Client_commands.full_context) ->
        let fitness =
          Tezos_client_alpha.Proto_alpha.Fitness_repr.from_int64 fitness in
        bake cctxt ?timestamp cctxt#block
          (Activate { protocol = hash ; validation_passes ; fitness }) sk >>=? fun hash ->
        cctxt#answer "Injected %a" Block_hash.pp_short hash >>= fun () ->
        return ()
      end ;

    command ~desc: "Fork a test protocol"
      args
      (prefixes [ "fork" ; "test" ; "protocol" ]
       @@ Protocol_hash.param ~name:"version" ~desc:"Protocol version (b58check)"
       @@ prefixes [ "with" ; "passes" ]
       @@ param ~name:"passes"
         ~desc:"Hardcoded number of validation passes (integer)"
         int_parameter
       @@ prefixes [ "and" ; "key" ]
       @@ Client_keys.Secret_key.source_param
         ~name:"password" ~desc:"Dictator's key"
       @@ stop)
      begin fun timestamp hash validation_passes sk cctxt ->
        bake cctxt ?timestamp cctxt#block
          (Activate_testnet { protocol = hash ;
                              validation_passes ;
                              delay = Int64.mul 24L 3600L })
          sk >>=? fun hash ->
        cctxt#answer "Injected %a" Block_hash.pp_short hash >>= fun () ->
        return ()
      end ;

  ]

let () =
  Client_commands.register protocol @@
  commands ()
