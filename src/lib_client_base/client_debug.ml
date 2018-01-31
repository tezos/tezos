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
    { Block_services.hash ; net_id ; level ;
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
     @ Data (hex encoded): \"%a\"@]"
    Block_hash.pp hash
    Test_network_status.pp test_network
    level
    proto_level
    Block_hash.pp predecessor
    Protocol_hash.pp protocol
    Net_id.pp net_id
    Time.pp_hum timestamp
    Fitness.pp fitness
    Operation_list_list_hash.pp operations_hash
    (fun ppf -> function
       | None -> Format.fprintf ppf "None"
       | Some operations ->
           Format.pp_print_list ~pp_sep:Format.pp_print_newline
             (Format.pp_print_list ~pp_sep:Format.pp_print_space
                (fun ppf (oph, _) -> Operation_hash.pp ppf oph))
             ppf operations)
    operations
    Hex.pp (MBytes.to_hex data)

let print_md_title ppf title level =
  Format.fprintf ppf "%s %s@.@." (String.init level (fun _ -> '#')) title

let skip_line ppf =
  Format.pp_print_newline ppf ();
  return @@ Format.pp_print_newline ppf ()

let registered_protocols ppf =
  Format.pp_print_list
    ~pp_sep:Format.pp_print_newline
    (fun ppf (protocol, _) -> Protocol_hash.pp ppf protocol)
    ppf
    (Client_commands.get_versions ())

let print_heads ppf cctxt =
  Client_rpcs.call_service0 cctxt Block_services.list
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
    ppf heads

let print_rejected ppf cctxt =
  Client_rpcs.call_service0 cctxt
    Block_services.list_invalid () >>=? fun invalid ->
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
  let group = { name = "debug" ;
                title = "commands to report debug information" } in
  let output_arg =
    arg
      ~doc:"Write output of debug command to file"
      ~parameter:"-file"
    @@ parameter (fun _ str -> return str) in
  let output_to_ppf = function
    | None -> Format.std_formatter
    | Some file -> Format.formatter_of_out_channel (open_out file) in
  [
    command ~group ~desc: "list protocols"
      (args1 output_arg)
      (fixed [ "list" ; "registered" ; "protocols" ])
      (fun output (_cctxt : Client_commands.full_context) ->
         let ppf = output_to_ppf output in
         registered_protocols ppf ;
         Format.fprintf ppf "@." ;
         return ()) ;
    command ~group ~desc: "current heads"
      (args1 output_arg)
      (fixed [ "list" ; "heads" ])
      (fun output cctxt ->
         let ppf = output_to_ppf output in
         print_heads ppf cctxt >>=? fun () ->
         Format.fprintf ppf "@." ;
         return ()) ;
    command ~group ~desc: "rejected blocks"
      (args1 output_arg)
      (fixed [ "list" ; "rejected" ; "blocks" ])
      (fun output cctxt ->
         let ppf = output_to_ppf output in
         print_rejected ppf cctxt >>|? fun () ->
         Format.fprintf ppf "@.") ;
    command ~group ~desc: "report on current node state"
      (args1 output_arg)
      (fixed [ "full" ; "report" ])
      (fun output cctxt ->
         let ppf = output_to_ppf output in
         print_md_title ppf "Node report:" 1 ;
         return @@ Format.fprintf ppf "Date: %a@;"
           Time.pp_hum (Time.now ()) >>=? fun () ->
         skip_line ppf >>=? fun () ->
         print_md_title ppf "Registered protocols:" 2 ;
         registered_protocols ppf ;
         skip_line ppf >>=? fun () ->
         print_md_title ppf "Heads:" 2 ;
         print_heads ppf cctxt >>=? fun () ->
         skip_line ppf >>=? fun () ->
         print_md_title ppf "Rejected blocks:" 2 ;
         print_rejected ppf cctxt >>|? fun () ->
         Format.fprintf ppf "@.") ;
  ]
