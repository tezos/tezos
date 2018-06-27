(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

let group =
  { Clic.name = "protocols" ;
    title = "Commands for managing protocols" }

let commands () =
  let open Clic in
  let check_dir _ dn =
    if Sys.is_directory dn then
      return dn
    else
      failwith "%s is not a directory" dn in
  let check_dir_parameter = parameter check_dir in
  [

    command ~group ~desc: "List protocols known by the node."
      no_options
      (prefixes [ "list" ; "protocols" ] stop)
      (fun () (cctxt : #Client_context.full) ->
         Shell_services.Protocol.list cctxt >>=? fun protos ->
         Lwt_list.iter_s (fun ph -> cctxt#message "%a" Protocol_hash.pp ph) protos >>= fun () ->
         return_unit
      );

    command ~group ~desc: "Inject a new protocol into the node."
      no_options
      (prefixes [ "inject" ; "protocol" ]
       @@ param ~name:"dir" ~desc:"directory containing a protocol" check_dir_parameter
       @@ stop)
      (fun () dirname (cctxt : #Client_context.full) ->
         Lwt.catch
           (fun () ->
              Lwt_utils_unix.Protocol.read_dir dirname >>=? fun (_hash, proto) ->
              Shell_services.Injection.protocol cctxt proto >>= function
              | Ok hash ->
                  cctxt#message "Injected protocol %a successfully" Protocol_hash.pp_short hash >>= fun () ->
                  return_unit
              | Error err ->
                  cctxt#error "Error while injecting protocol from %s: %a"
                    dirname Error_monad.pp_print_error err >>= fun () ->
                  return_unit)
           (fun exn ->
              cctxt#error "Error while injecting protocol from %s: %a"
                dirname Error_monad.pp_print_error [Error_monad.Exn exn] >>= fun () ->
              return_unit)
      );

    command ~group ~desc: "Dump a protocol from the node's record of protocol."
      no_options
      (prefixes [ "dump" ; "protocol" ]
       @@ Protocol_hash.param ~name:"protocol hash" ~desc:""
       @@ stop)
      (fun () ph (cctxt : #Client_context.full) ->
         Shell_services.Protocol.contents cctxt ph >>=? fun proto ->
         Lwt_utils_unix.Protocol.write_dir (Protocol_hash.to_short_b58check ph) ~hash:ph proto >>=? fun () ->
         cctxt#message "Extracted protocol %a" Protocol_hash.pp_short ph >>= fun () ->
         return_unit
      ) ;
  ]
