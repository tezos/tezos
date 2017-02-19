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
    "ProtoDemoDemoDemoDemoDemoDemoDemoDemoDemoDemoD3c8k9"

let demo cctxt =
  let block = Client_config.block () in
  cctxt.Client_commands.message "Calling the 'echo' RPC." >>= fun () ->
  let msg = "test" in
  Client_proto_rpcs.echo cctxt block msg >>= fun reply ->
  fail_unless (reply = msg) (Unclassified "...") >>=? fun () ->
  begin
    cctxt.message "Calling the 'failing' RPC." >>= fun () ->
    Client_proto_rpcs.failing cctxt block 3 >>= function
    | Error [Ecoproto_error [Error.Demo_error 3]] ->
        return ()
    | _ -> failwith "..."
  end >>=? fun () ->
  cctxt.message "Direct call to `demo_error`." >>= fun () ->
  begin Error.demo_error 101010 >|= wrap_error >>= function
    | Error [Ecoproto_error [Error.Demo_error 101010]] ->
        return ()
    | _ -> failwith "...."
  end >>=? fun () ->
  cctxt.answer "All good!" >>= fun () ->
  return ()

let mine cctxt =
  let block =
    match Client_config.block () with
    | `Prevalidation -> `Head 0
    | `Test_prevalidation -> `Test_head 0
    | b -> b in
  Client_node_rpcs.Blocks.info cctxt block >>= fun bi ->
  let fitness =
    match bi.fitness with
    | [ v ; b ] ->
        let f = MBytes.get_int64 b 0 in
        MBytes.set_int64 b 0 (Int64.succ f) ;
        [ v ; b ]
    | _ ->
        Lwt.ignore_result
          (cctxt.message "Cannot parse fitness: %a" Fitness.pp bi.fitness);
        exit 2 in
  Client_node_rpcs.forge_block cctxt
    ~net:bi.net ~predecessor:bi.hash
    fitness [] (MBytes.create 0) >>= fun bytes ->
  Client_node_rpcs.inject_block cctxt ~wait:true bytes >>=? fun hash ->
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
      (fixed [ "demo" ])
      (fun cctxt -> demo cctxt >>= handle_error cctxt) ;
    command ~group ~desc: "A failing command"
      (fixed [ "fail" ])
      (fun cctxt ->
         Error.demo_error 101010
         >|= wrap_error
         >>= handle_error cctxt) ;
    command ~group ~desc: "Mine an empty block"
      (fixed [ "mine" ])
      (fun cctxt -> mine cctxt >>= handle_error cctxt) ;
  ]

let () =
  Client_commands.register protocol @@
  commands ()
