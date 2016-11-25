(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

let protocol =
  Protocol_hash.of_b48check
    "2gagsSEvTKAHRjxAamgSdBNkv39VtNCqpaDXrrH4K8R4KQAAHrhe3"

let demo () =
  let block = Client_config.block () in
  Cli_entries.message "Calling the 'echo' RPC." ;
  let msg = "test" in
  Client_proto_rpcs.echo block msg >>= fun reply ->
  fail_unless (reply = msg) (Unclassified "...") >>=? fun () ->
  begin
    Cli_entries.message "Calling the 'failing' RPC." ;
    Client_proto_rpcs.failing block 3 >>= function
    | Error [Ecoproto_error [Error.Demo_error 3]] ->
        return ()
    | _ -> failwith "..."
  end >>=? fun () ->
  Cli_entries.message "Direct call to `demo_error`." ;
  begin Error.demo_error 101010 >|= wrap_error >>= function
    | Error [Ecoproto_error [Error.Demo_error 101010]] ->
        return ()
    | _ -> failwith "...."
  end >>=? fun () ->
  Cli_entries.answer "All good!" ;
  return ()

let mine () =
  let block =
    match Client_config.block () with
    | `Prevalidation -> `Head 0
    | `Test_prevalidation -> `Test_head 0
    | b -> b in
  Client_node_rpcs.Blocks.info block >>= fun bi ->
  let fitness =
    match bi.fitness with
    | [ v ; b ] ->
        let f = MBytes.get_int64 b 0 in
        MBytes.set_int64 b 0 (Int64.succ f) ;
        [ v ; b ]
    | _ ->
        Cli_entries.message "Cannot parse fitness: %a" Fitness.pp bi.fitness  ;
        exit 2 in
  Client_node_rpcs.forge_block
    ~net:bi.net ~predecessor:bi.hash
    fitness [] (MBytes.create 0) >>= fun bytes ->
  Client_node_rpcs.inject_block ~wait:true bytes >>=? fun hash ->
  Cli_entries.answer "Injected %a" Block_hash.pp_short hash ;
  return ()

let handle_error = function
  | Ok res ->
      Lwt.return res
  | Error exns ->
      pp_print_error Format.err_formatter exns ;
      Cli_entries.error "cannot continue"

let commands () =
  let open Cli_entries in
  register_group "demo" "Some demo command" ;
  [
    command
      ~group: "demo"
      ~desc: "A demo command"
      (fixed [ "demo" ])
      (fun () -> demo () >>= handle_error) ;
    command
      ~group: "demo"
      ~desc: "An failing command"
      (fixed [ "fail" ])
      (fun () ->
         Error.demo_error 101010
         >|= wrap_error
         >>= handle_error ) ;
    command
      ~group: "demo"
      ~desc: "Mine an empty block"
      (fixed [ "mine" ])
      (fun () -> mine () >>= handle_error) ;
  ]

let () =
  Client_version.register protocol @@
  commands ()
