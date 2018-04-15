(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(* Commands used to introspect the node's state *)

let skip_line ppf =
  Format.pp_print_newline ppf ();
  return @@ Format.pp_print_newline ppf ()

let print_invalid_blocks ppf (b: Chain_services.invalid_block) =
  Format.fprintf ppf
    "@[<v 2>Hash: %a\
     @ Level: %ld\
     @ %a@]"
    Block_hash.pp b.hash
    b.level
    pp_print_error b.errors

let commands () =
  let open Clic in
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
               ignore Clic.(setup_formatter ppf Plain Full) ;
               return ppf)) in
  [
    command ~group
      ~desc: "The last heads that have been considered by the node."
      (args1 output_arg)
      (fixed [ "list" ; "heads" ])
      (fun ppf cctxt ->
         Chain_services.Blocks.list cctxt () >>=? fun heads ->
         Format.fprintf ppf "@[<v>%a@]@."
           (Format.pp_print_list Block_hash.pp)
           (List.concat heads) ;
         return ()) ;
    command ~group ~desc: "The blocks that have been marked invalid by the node."
      (args1 output_arg)
      (fixed [ "list" ; "rejected" ; "blocks" ])
      (fun ppf cctxt ->
         Chain_services.Invalid_blocks.list cctxt () >>=? function
         | [] ->
             Format.fprintf ppf "No invalid blocks." ;
             return ()
         | _ :: _ as invalid ->
             Format.fprintf ppf "@[<v>%a@]@."
               (Format.pp_print_list print_invalid_blocks)
               invalid ;
             return ()) ;
  ]
