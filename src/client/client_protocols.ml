let commands () =
  let open Cli_entries in
  let check_dir dn =
    if Sys.is_directory dn then Lwt.return dn else Lwt.fail_invalid_arg "not a directory"
  in
  let check_hash ph = Lwt.wrap1 Protocol_hash.of_b48check ph in
  register_group "protocols" "Commands for managing protocols" ;
  [
    command
      ~group: "protocols"
      ~desc: "list known protocols"
      (prefixes [ "list" ; "protocols" ] stop)
      (fun () ->
         Client_node_rpcs.Protocols.list ~contents:false () >>= fun protos ->
         Lwt_list.iter_s (fun (ph, _p) -> message "%a" Protocol_hash.pp ph) protos
      );
    command
      ~group: "protocols"
      ~desc: "inject a new protocol to the shell database"
      (prefixes [ "inject" ; "protocol" ]
       @@ param ~name:"directory containing a protocol" ~desc:"" check_dir
       @@ stop)
      (fun dirname () ->
         Lwt.catch
           (fun () ->
              let proto = Tezos_compiler.Protocol.of_dir dirname in
              Client_node_rpcs.inject_protocol proto >>= function
              | Ok hash ->
                  message "Injected protocol %a successfully" Protocol_hash.pp_short hash
              | Error err ->
                  error "Error while injecting protocol from %s: %a"
                    dirname Error_monad.pp_print_error err)
           (fun exn ->
              error "Error while injecting protocol from %s: %a"
                dirname Error_monad.pp_print_error [Error_monad.Exn exn])
      );
    command
      ~group: "protocols"
      ~desc: "dump a protocol from the shell database"
      (prefixes [ "dump" ; "protocol" ]
       @@ param ~name:"protocol hash" ~desc:"" check_hash
       @@ stop)
      (fun ph () ->
         Client_node_rpcs.Protocols.bytes ph >>= fun { data } -> match data with
         | Ok proto ->
             Updater.extract "" ph proto >>= fun () ->
             message "Extracted protocol %a" Protocol_hash.pp_short ph
         | Error err ->
             error "Error while dumping protocol %a: %a"
               Protocol_hash.pp_short ph Error_monad.pp_print_error err);
  ]
