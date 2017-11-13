(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

let protocol =
  Protocol_hash.of_b58check_exn
    "ProtoDemoDemoDemoDemoDemoDemoDemoDemoDemoDemoD3c8k9"

let demo cctxt =
  let block = Client_commands.(cctxt.config.block) in
  cctxt.Client_commands.message "Calling the 'echo' RPC." >>= fun () ->
  let msg = "test" in
  Client_proto_rpcs.echo cctxt.rpc_config block msg >>=? fun reply ->
  fail_unless (reply = msg) (failure "...") >>=? fun () ->
  begin
    cctxt.message "Calling the 'failing' RPC." >>= fun () ->
    Client_proto_rpcs.failing cctxt.rpc_config block 3 >>= function
    | Error [Environment.Ecoproto_error [Error.Demo_error 3]] ->
        return ()
    | _ -> failwith "..."
  end >>=? fun () ->
  cctxt.message "Direct call to `demo_error`." >>= fun () ->
  begin Error.demo_error 101010 >|= Environment.wrap_error >>= function
    | Error [Environment.Ecoproto_error [Error.Demo_error 101010]] ->
        return ()
    | _ -> failwith "...."
  end >>=? fun () ->
  cctxt.answer "All good!" >>= fun () ->
  return ()

let mine cctxt =
  let block = Client_rpcs.last_mined_block cctxt.Client_commands.config.block in
  Client_node_rpcs.Blocks.info cctxt.rpc_config block >>=? fun bi ->
  let fitness =
    match bi.fitness with
    | [ v ; b ] ->
        let f = MBytes.get_int64 b 0 in
        MBytes.set_int64 b 0 (Int64.succ f) ;
        [ v ; b ]
    | _ ->
        Lwt.ignore_result
          (cctxt.message "Cannot parse fitness: %a" Environment.Fitness.pp bi.fitness);
        exit 2 in
  Client_node_rpcs.forge_block_header cctxt.rpc_config
    { shell = { net_id = bi.net_id ;
                predecessor = bi.hash ;
                proto_level = bi.proto_level ;
                level = Int32.succ bi.level ;
                timestamp = Time.now () ;
                fitness ;
                validation_passes = 0 ;
                operations_hash = Operation_list_list_hash.empty } ;
      proto = MBytes.create 0 } >>=? fun bytes ->
  Client_node_rpcs.inject_block cctxt.rpc_config bytes [] >>=? fun hash ->
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
  let group = {name = "demo" ; title = "Some demo command" } in
  [
    command ~group ~desc: "A demo command"
      no_options
      (fixed [ "demo" ])
      (fun () cctxt -> demo cctxt) ;
    command ~group ~desc: "A failing command"
      no_options
      (fixed [ "fail" ])
      (fun () _cctxt ->
         Error.demo_error 101010
         >|= Environment.wrap_error) ;
    command ~group ~desc: "Mine an empty block"
      no_options
      (fixed [ "mine" ])
      (fun () cctxt -> mine cctxt) ;
  ]

let () =
  Client_commands.register protocol @@
  commands ()
