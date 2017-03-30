(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

let group =
  { Cli_entries.name = "protocols" ;
    title = "Commands for managing protocols" }

let commands () =
  let open Cli_entries in
  let check_dir _ dn =
    if Sys.is_directory dn then
      Lwt.return dn
    else
      Lwt.fail_with (dn ^ " is not a directory") in
  let check_hash _ ph =
    Lwt.wrap1 Protocol_hash.of_b58check ph in
  [
    command ~group ~desc: "list known protocols"
      (prefixes [ "list" ; "protocols" ] stop)
      (fun cctxt ->
         Client_node_rpcs.Protocols.list cctxt ~contents:false () >>= fun protos ->
         Lwt_list.iter_s (fun (ph, _p) -> cctxt.message "%a" Protocol_hash.pp ph) protos
      );
    command ~group ~desc: "inject a new protocol to the shell database"
      (prefixes [ "inject" ; "protocol" ]
       @@ param ~name:"dir" ~desc:"directory containing a protocol" check_dir
       @@ stop)
      (fun dirname cctxt ->
         Lwt.catch
           (fun () ->
              let proto = Tezos_compiler.Protocol.of_dir dirname in
              Client_node_rpcs.inject_protocol cctxt proto >>= function
              | Ok hash ->
                  cctxt.message "Injected protocol %a successfully" Protocol_hash.pp_short hash
              | Error err ->
                  cctxt.error "Error while injecting protocol from %s: %a"
                    dirname Error_monad.pp_print_error err)
           (fun exn ->
              cctxt.error "Error while injecting protocol from %s: %a"
                dirname Error_monad.pp_print_error [Error_monad.Exn exn])
      );
    command ~group ~desc: "dump a protocol from the shell database"
      (prefixes [ "dump" ; "protocol" ]
       @@ param ~name:"protocol hash" ~desc:"" check_hash
       @@ stop)
      (fun ph cctxt ->
         Client_node_rpcs.Protocols.contents cctxt ph >>= fun proto ->
         Updater.extract "" ph proto >>= fun () ->
         cctxt.message "Extracted protocol %a" Protocol_hash.pp_short ph) ;
         (* | Error err -> *)
             (* cctxt.error "Error while dumping protocol %a: %a" *)
               (* Protocol_hash.pp_short ph Error_monad.pp_print_error err); *)
  ]
