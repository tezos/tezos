(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Cli_entries

type error += Bad_tez_arg of string * string (* Arg_name * value *)
type error += Bad_max_priority of string
type error += Bad_endorsement_delay of string

let () =
  register_error_kind
    `Permanent
    ~id:"badTezArg"
    ~title:"Bad Tez Arg"
    ~description:("Invalid \xEA\x9C\xA9 notation in parameter.")
    ~pp:(fun ppf (arg_name, literal) ->
        Format.fprintf ppf
          "Invalid \xEA\x9C\xA9 notation in parameter %s: '%s'"
          arg_name literal)
    Data_encoding.(obj2
                     (req "parameter" string)
                     (req "literal" string))
    (function Bad_tez_arg (parameter, literal) -> Some (parameter, literal) | _ -> None)
    (fun (parameter, literal) -> Bad_tez_arg (parameter, literal)) ;
  register_error_kind
    `Permanent
    ~id:"badMaxPriorityArg"
    ~title:"Bad -max-priority arg"
    ~description:("invalid priority in -max-priority")
    ~pp:(fun ppf literal ->
        Format.fprintf ppf "invalid priority '%s'in -max-priority" literal)
    Data_encoding.(obj1 (req "parameter" string))
    (function Bad_max_priority parameter -> Some parameter | _ -> None)
    (fun parameter -> Bad_max_priority parameter) ;
  register_error_kind
    `Permanent
    ~id:"badEndorsementDelayArg"
    ~title:"Bad -endorsement-delay arg"
    ~description:("invalid priority in -endorsement-delay")
    ~pp:(fun ppf literal ->
        Format.fprintf ppf "Bad argument value for -endorsement-delay. Expected an integer, but given '%s'" literal)
    Data_encoding.(obj1 (req "parameter" string))
    (function Bad_endorsement_delay parameter -> Some parameter | _ -> None)
    (fun parameter -> Bad_endorsement_delay parameter)


let tez_sym =
  "\xEA\x9C\xA9"

let string_parameter =
  parameter (fun _ x -> return x)

let init_arg =
  default_arg
    ~parameter:"-init"
    ~doc:"The initial value of the contract's storage."
    ~default:"Unit"
    string_parameter

let arg_arg =
  default_arg
    ~parameter:"-arg"
    ~doc:"The argument passed to the contract's script, if needed."
    ~default:"Unit"
    string_parameter

let delegate_arg =
  arg
    ~parameter:"-delegate"
    ~doc:"Set the delegate of the contract.\
          Must be a known identity."
    string_parameter

let source_arg =
  arg
    ~parameter:"-source"
    ~doc:"Set the source of the bonds to be paid.\
          Must be a known identity."
    string_parameter

let spendable_switch =
  switch
    ~parameter:"-spendable"
    ~doc:"Set the created contract to be spendable"

let force_switch =
  switch
    ~parameter:"-force"
    ~doc:"Force the injection of branch-invalid operation or force \
         \ the injection of block without a fitness greater than the \
         \ current head."

let delegatable_switch =
  switch
    ~parameter:"-delegatable"
    ~doc:"Set the created contract to be delegatable"

let tez_format = "text format: D,DDD,DDD.DD (centiles are optional, commas are optional)"

let tez_parameter param =
  parameter
    (fun _ s ->
       match Tez.of_string s with
       | Some tez -> return tez
       | None -> fail (Bad_tez_arg (param, s)))

let tez_arg ~default ~parameter ~doc =
  default_arg ~parameter ~doc ~default (tez_parameter parameter)

let tez_param ~name ~desc next =
  Cli_entries.param
    name
    (desc ^ " in \xEA\x9C\xA9\n" ^ tez_format)
    (tez_parameter name)
    next

let fee_arg =
  tez_arg
    ~default:"0.05"
    ~parameter:"-fee"
    ~doc:"The fee in \xEA\x9C\xA9 to pay to the baker."

let max_priority_arg =
  arg
    ~parameter:"-max-priority"
    ~doc:"Set the max_priority used when looking for baking slot."
    (parameter (fun _ s ->
         try return (int_of_string s)
         with _ -> fail (Bad_max_priority s)))

let free_baking_switch =
  switch
    ~parameter:"-free-baking"
    ~doc:"Only consider free baking slots."

let endorsement_delay_arg =
  default_arg
    ~parameter:"-endorsement-delay"
    ~doc:"Set the delay used before to endorse the current block."
    ~default:"15"
    (parameter (fun _ s ->
         try return (int_of_string s)
         with _ -> fail (Bad_endorsement_delay s)))

let no_print_source_flag =
  switch
    ~parameter:"-no-print-source"
    ~doc:"Don't print the source code if an error is encountered.\
          This should be enabled for extremely large programs"

module Daemon = struct
  let baking_switch =
    switch
      ~parameter:"-baking"
      ~doc:"Run the baking daemon"
  let endorsement_switch =
    switch
      ~parameter:"-endorsement"
      ~doc:"Run the endorsement daemon"
  let denunciation_switch =
    switch
      ~parameter:"-denunciation"
      ~doc:"Run the denunciation daemon"
end
