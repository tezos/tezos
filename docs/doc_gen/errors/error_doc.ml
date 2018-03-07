(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(* Todo : add section descriptions *)

let default_section_id = "default"
let default_section_title = "Miscellaneous"

(* Association list where keys are set of identifier's prefixes that
   maps to a section title. The ordering of sections in the rst output
   depends on their position in this list.

   e.g. : an error which id is 'utils.Timeout' will be documented
   under the `Miscellaneous` section which will be displayed at the
   bottom of the document. Unprefixed ids or unreferenced prefixes
   will default to `Miscellaneous` *)
let section_titles =
  [
    [ "proto" ], "Protocol Alpha";
    [ "baking" ], "Baking" ;
    [ "contract" ], "Smart Contracts" ;
    [ "distributed_db" ], "Database" ;
    [ "micheline" ; "michelson" ], "Smart Contracts" ;
    (* [ "michelson" ], "Michelson" ; *)
    [ "node" ], "Client Node" ;
    [ "operation" ], "Operations" ;
    [ "prevalidation" ], "Prevalidation" ;
    [ "raw_store" ], "Store" ;
    [ "rpc_client" ], "RPC Client" ;
    [ "tez" ], "Tezos operations" ;
    [ "validator" ], "Validator" ;
    [ "worker" ], "Worker" ;
    (* [ "cli" ], "Command Line" ; *)
    [ "cli"; "utils"; default_section_id ], default_section_title ;
  ]

let categories_detail =
  [ "temporary", "An error resulting from an operation that might be \
                  valid in the future, for example, a contract’s balance \
                  being too low to execute the intended operation. This \
                  can be fixed by adding more to the contract’s balance."
  ; "branch", "An error that occurs in one branch of the chain, but may not \
               occur in a different one. For example, receiving an \
               operation for an old or future protocol version."
  ; "permanent", "An error that is not recoverable because the operation \
                  is never going to be valid. For example, an invalid ꜩ \
                  notation." ]

let pp_rst_title ~char ppf title =
  let sub = String.map (fun _ -> char) title in
  Format.fprintf ppf "@[<v 0>%s@\n@]@[<v 0>%s@\n@\n@]" title sub

let pp_rst_h1 = pp_rst_title ~char:'#'
let pp_rst_h2 = pp_rst_title ~char:'*'
let pp_rst_h3 = pp_rst_title ~char:'='
let pp_rst_h4 = pp_rst_title ~char:'`'

let string_of_err_category =
  let open Error_monad in function
    | `Branch -> "branch"
    | `Temporary -> "temporary"
    | `Permanent -> "permanent"

let pp_info_to_rst
    ppf
    { Error_monad.id ; title ; category ; description ; schema } =
  let open Format in

  fprintf ppf "@[<v 2>- **%s**@,@,"
    (if title = "" then "<Untitled>" else title) ;

  fprintf ppf "@[<v 0>%s@\n@\n@]"
    (if description = "" then "Not description available" else description) ;

  fprintf ppf "@[<v 0>* *Id* : %s@\n@\n@]" id ;

  fprintf ppf "@[* *Category* : %s@\n@\n@]" (string_of_err_category category) ;

  fprintf ppf "@[<v 2>.. container:: schema-button@\n@\n" ;
  fprintf ppf "@[<v 2>Show schema@]@]@\n@\n" ;

  fprintf ppf "@[<v 2>.. container:: schema@\n@\n" ;
  fprintf ppf "@[<v 2>.. code-block:: json@\n@\n" ;

  fprintf ppf "@[%a@]@]@]@]" Json_schema.pp schema

module ErrorSet = Set.Make(struct
    type t = Error_monad.error_info
    let compare { Error_monad.id ; _ } { Error_monad.id = id' ; _ } =
      String.compare id id'
  end)

module ErrorPartition = struct
  include Map.Make(struct
      include String
      let titles = List.map snd section_titles

      let compare s s' =
        let idx s =
          let rec loop acc = function
            | [] -> assert false
            | h::_ when h = s -> acc
            | _::t -> loop (acc + 1) t
          in loop 0 titles
        in
        Pervasives.compare (idx s) (idx s')
    end)

  let add_error (id : key) (error : Error_monad.error_info) (map : 'a t) =
    let lr_opt = Stringext.cut id ~on:"." in

    let id_prefix =
      match lr_opt with
      | None -> default_section_id
      | Some (prefix, _r) -> prefix
    in

    let title =
      try
        snd (List.find
               (fun (id_set, _) -> List.mem id_prefix id_set)
               section_titles)
      with
      | Not_found -> default_section_title
    in

    let set =
      try
        find title map
      with
      | Not_found -> ErrorSet.empty
    in

    add title (ErrorSet.add error set) map

end

let pp_error_map ppf (map : ErrorSet.t ErrorPartition.t) : unit =
  let open Format in
  ErrorPartition.iter (fun section_title set ->
      fprintf ppf "%a" pp_rst_h2 section_title ;

      ErrorSet.iter
        (fun error_repr ->
           fprintf ppf "@[%a@]@\n@\n" pp_info_to_rst error_repr
        ) set
    ) map

let print_script ppf =
  (* HACK : show/hide JSON schemas + style *)
  Format.fprintf ppf "@[<v 2>.. raw:: html@\n@\n" ;
  Format.fprintf ppf "@[<v 0>%s%s@]@\n@\n@]"
    "<script>document.addEventListener('DOMContentLoaded', function(){\
     $(\".schema-button\").click(function(){$(this).next(\".schema\")\
     .first().toggle()})}, false);</script>"
    "<style>.schema { display:none; margin:0 0 0 10px; }\
     .schema-button { cursor:pointer; font-size:11px;\
     font-weight: bold; background-color: #EEEEEE;\
     color: #333333; padding: 2px 6px 2px 6px;\
     border-top: 1px solid #CCCCCC; border-right: 1px solid #333333;\
     border-bottom: 1px solid #333333; border-left: 1px solid #CCCCCC;\
     width : -moz-fit-content; }\
     section li { margin:10px 0 10px 0;  } </style>"

(* Main *)
let () =
  let open Format in
  let ppf = std_formatter in

  (* Header *)
  let title = "Tezos Client Errors" in
  fprintf ppf "%a" pp_rst_h1 title ;

  print_script ppf ;

  fprintf ppf "This document references possible errors.@\n@\n" ;

  fprintf ppf "There are three categories of error :@\n@\n" ;

  List.iter (fun (cat, descr) ->
      fprintf ppf "- :literal:`%s` - %s@\n@\n" cat descr) categories_detail ;

  fprintf ppf "See `The Error Monad`_ for further details.@\n@\n" ;
  fprintf
    ppf ".. _The Error Monad: \
         ../tutorials/error_monad.html#the-actual-tezos-error-monad@\n@\n" ;

  (* Body *)
  let map =
    let all_errors =
      Error_monad.get_registered_errors () in
    List.fold_left
      (fun acc ( Error_monad.{ id ; _ } as error ) ->
         ErrorPartition.add_error id error acc
      ) ErrorPartition.empty all_errors
  in

  fprintf ppf "%a" pp_error_map map
