(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Client_commands

let group =
  { Cli_entries.name = "protocols" ;
    title = "Commands for managing protocols" }

let commands () =
  let open Cli_entries in
  let check_dir _ dn =
    if Sys.is_directory dn then
      return dn
    else
      failwith "%s is not a directory" dn in
  [

    command ~group ~desc: "list known protocols"
      no_options
      (prefixes [ "list" ; "protocols" ] stop)
      (fun () cctxt ->
         Client_node_rpcs.Protocols.list cctxt.rpc_config ~contents:false () >>=? fun protos ->
         Lwt_list.iter_s (fun (ph, _p) -> cctxt.message "%a" Protocol_hash.pp ph) protos >>= fun () ->
         return ()
      );

    command ~group ~desc: "inject a new protocol to the shell database"
      no_options
      (prefixes [ "inject" ; "protocol" ]
       @@ param ~name:"dir" ~desc:"directory containing a protocol" check_dir
       @@ stop)
      (fun () dirname cctxt ->
         Lwt.catch
           (fun () ->
              let _hash, proto = Tezos_protocol_compiler.Native.read_dir dirname in
              Client_node_rpcs.inject_protocol cctxt.rpc_config proto >>= function
              | Ok hash ->
                  cctxt.message "Injected protocol %a successfully" Protocol_hash.pp_short hash >>= fun () ->
                  return ()
    
              | Error err ->
                  cctxt.error "Error while injecting protocol from %s: %a"
                    dirname Error_monad.pp_print_error err >>= fun () ->
                  return ())
           (fun exn ->
              cctxt.error "Error while injecting protocol from %s: %a"
                dirname Error_monad.pp_print_error [Error_monad.Exn exn] >>= fun () ->
              return ())
      );

    command ~group ~desc: "dump a protocol from the shell database"
      no_options
      (prefixes [ "dump" ; "protocol" ]
       @@ Protocol_hash.param ~name:"protocol hash" ~desc:""
       @@ stop)
      (fun () ph cctxt ->
         Client_node_rpcs.Protocols.contents cctxt.rpc_config ph >>=? fun proto ->
         Updater.extract (Protocol_hash.to_short_b58check ph) ~hash:ph proto >>= fun () ->
         cctxt.message "Extracted protocol %a" Protocol_hash.pp_short ph >>= fun () ->
         return ()
    ) ;
         (* | Error err -> *)
             (* cctxt.error "Error while dumping protocol %a: %a" *)
               (* Protocol_hash.pp_short ph Error_monad.pp_print_error err); *)
  ]
