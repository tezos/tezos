(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(* Commands used to debug the node/alphanet *)

let pp_block ppf
    { Node_rpc_services.Blocks.hash ; net_id ; level ;
      proto_level ; predecessor ; timestamp ;
      operations_hash ; fitness ; data ;
      operations ; protocol ; test_network } =
  Format.fprintf ppf
    "@[<v 2>Hash: %a\
     @ Test network: %a\
     @ Level: %ld\
     @ Proto_level: %d\
     @ Predecessor: %a\
     @ Protocol: %a\
     @ Net id: %a\
     @ Timestamp: %a\
     @ Fitness: @[<v>%a@]\
     @ Operations hash: %a\
     @ Operations: @[<v>%a@]\
     @ Data (hex encoded): \"%s\"@]"
    Block_hash.pp hash
    Context.pp_test_network test_network
    level
    proto_level
    Block_hash.pp predecessor
    Protocol_hash.pp protocol
    Net_id.pp net_id
    Time.pp_hum timestamp
    (Format.pp_print_list
       ~pp_sep:Format.pp_print_space
       Format.pp_print_string)
    (List.map Hex_encode.hex_of_bytes fitness)
    Operation_list_list_hash.pp operations_hash
    (fun ppf -> function
       | None -> Format.fprintf ppf "None"
       | Some operations ->
           Format.pp_print_list ~pp_sep:Format.pp_print_newline
             (Format.pp_print_list ~pp_sep:Format.pp_print_space
                (fun ppf (oph, _) -> Operation_hash.pp ppf oph))
             ppf operations)
    operations
    (Hex_encode.hex_of_bytes data)

let stuck_node_report (cctxt : Client_commands.context) file =
  let ppf = Format.formatter_of_out_channel (open_out file) in
  let skip_line () =
    Format.pp_print_newline ppf ();
    return @@ Format.pp_print_newline ppf () in
  let print_title title level =
    Format.fprintf ppf "%s %s@.@." (String.init level (fun _ -> '#')) title;
    return () in
  print_title "Stuck node report:" 1 >>=? fun () ->
  return @@ Format.fprintf ppf "Date: %a@;"
    Time.pp_hum (Time.now ()) >>=? fun () ->
  skip_line () >>=? fun () ->
  print_title "Registered protocols:" 2 >>=? fun () ->
  return @@ Format.pp_print_list
    ~pp_sep:Format.pp_print_newline
    (fun ppf (protocol, _) -> Protocol_hash.pp ppf protocol)
    ppf
    (Client_commands.get_versions ()) >>=? fun () ->
  skip_line () >>=? fun () ->
  print_title "Heads:" 2 >>=? fun () ->
  Client_rpcs.call_service0 cctxt.rpc_config Node_rpc_services.Blocks.list
    { include_ops = true ;
      length = Some 1 ;
      heads = None ;
      monitor = None ;
      delay = None ;
      min_date = None ;
      min_heads = None } >>=? fun heads ->
  return @@
  Format.pp_print_list ~pp_sep:Format.pp_print_newline
    (fun ppf blocks ->
       Format.pp_print_list
         ~pp_sep:Format.pp_print_newline
         pp_block
         ppf
         blocks)
    ppf heads >>=? fun () ->
  skip_line () >>=? fun () ->
  print_title "Rejected blocks:" 2 >>=? fun () ->
  Client_rpcs.call_service0
    cctxt.rpc_config
    Node_rpc_services.Blocks.list_invalid () >>=? fun invalid ->
  return @@
  Format.pp_print_list
    (fun ppf (hash, level, errors) ->
       Format.fprintf ppf
         "@[<v 2>Hash: %a\
          @ Level: %ld\
          @ Errors: @[<v>%a@]@]"
         Block_hash.pp hash
         level
         (Format.pp_print_list ~pp_sep:Format.pp_print_newline
            Error_monad.pp)
         errors)
    ppf
    invalid



let commands () =
  let open Cli_entries in
  [
    command ~desc: "debug report"
      no_options
      (prefixes [ "debug" ; "stuck" ; "node" ]
       @@ string ~name:"file" ~desc:"file in which to save report"
       @@ stop)
      (fun () file cctxt ->
         stuck_node_report cctxt file)
  ]
