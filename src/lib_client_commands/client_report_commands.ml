(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(* Commands used to introspect the node's state *)

let pp_block ppf
    { Block_services.hash ; chain_id ; level ;
      proto_level ; predecessor ; timestamp ;
      operations_hash ; fitness ; data ;
      operations ; protocol ; test_chain } =
  Format.fprintf ppf
    "@[<v 2>Hash: %a\
     @ Test chain: %a\
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
    Test_chain_status.pp test_chain
    level
    proto_level
    Block_hash.pp predecessor
    Protocol_hash.pp protocol
    Chain_id.pp chain_id
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

let skip_line ppf =
  Format.pp_print_newline ppf ();
  return @@ Format.pp_print_newline ppf ()

let print_heads ppf heads =
  Format.pp_print_list ~pp_sep:Format.pp_print_newline
    (fun ppf blocks ->
       Format.pp_print_list
         ~pp_sep:Format.pp_print_newline
         pp_block
         ppf
         blocks)
    ppf heads

let print_rejected ppf = function
  | [] -> Format.fprintf ppf "No invalid blocks."
  | invalid ->
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
  let group = { name = "report" ;
                title = "Commands to report the node's status" } in
  let output_arg =
    default_arg
      ~doc:"write to a file"
      ~long:"output"
      ~short:'o'
      ~placeholder:"path"
      ~default: "-"
      (parameter (fun _ -> function
           | "-" -> return Format.std_formatter
           | file ->
               let ppf = Format.formatter_of_out_channel (open_out file) in
               ignore Cli_entries.(setup_formatter ppf Plain Full) ;
               return ppf)) in
  [
    command ~group
      ~desc: "The last heads that have been considered by the node."
      (args1 output_arg)
      (fixed [ "list" ; "heads" ])
      (fun ppf cctxt ->
         Block_services.list ~include_ops:true ~length:1 cctxt >>=? fun heads ->
         Format.fprintf ppf "%a@." print_heads heads ;
         return ()) ;
    command ~group ~desc: "The blocks that have been marked invalid by the node."
      (args1 output_arg)
      (fixed [ "list" ; "rejected" ; "blocks" ])
      (fun ppf cctxt ->
         Block_services.list_invalid cctxt >>=? fun invalid ->
         Format.fprintf ppf "%a@." print_rejected invalid ;
         return ()) ;
    command ~group ~desc: "A full report of the node's state."
      (args1 output_arg)
      (fixed [ "full" ; "report" ])
      (fun ppf cctxt ->
         Block_services.list ~include_ops:true ~length:1 cctxt >>=? fun heads ->
         Block_services.list_invalid cctxt >>=? fun invalid ->
         Format.fprintf ppf
           "@[<v 0>@{<title>Date@} %a@,\
            @[<v 2>@{<title>Heads@}@,%a@]@,\
            @[<v 2>@{<title>Rejected blocks@}@,%a@]@]"
           Time.pp_hum (Time.now ())
           print_heads heads
           print_rejected invalid ;
         return ()) ;
  ]
