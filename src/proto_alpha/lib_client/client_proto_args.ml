(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

open Proto_alpha
open Alpha_context
open Clic

type error += Bad_tez_arg of string * string (* Arg_name * value *)
type error += Bad_max_priority of string
type error += Bad_minimal_fees of string
type error += Bad_max_waiting_time of string
type error += Bad_endorsement_delay of string
type error += Bad_preserved_levels of string

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
        Format.fprintf ppf "invalid priority '%s' in -max-priority" literal)
    Data_encoding.(obj1 (req "parameter" string))
    (function Bad_max_priority parameter -> Some parameter | _ -> None)
    (fun parameter -> Bad_max_priority parameter) ;
  register_error_kind
    `Permanent
    ~id:"badMinimalFeesArg"
    ~title:"Bad -minimal-fees arg"
    ~description:("invalid fee threshold in -fee-threshold")
    ~pp:(fun ppf literal ->
        Format.fprintf ppf "invalid minimal fees '%s'" literal)
    Data_encoding.(obj1 (req "parameter" string))
    (function Bad_minimal_fees parameter -> Some parameter | _ -> None)
    (fun parameter -> Bad_minimal_fees parameter) ;
  register_error_kind
    `Permanent
    ~id:"badMaxWaitingTimeArg"
    ~title:"Bad -max-waiting-time arg"
    ~description:("invalid duration in -max-waiting-time")
    ~pp:(fun ppf literal ->
        Format.fprintf ppf "Bad argument value for -max-waiting-time. Expected an integer, but given '%s'" literal)
    Data_encoding.(obj1 (req "parameter" string))
    (function Bad_max_waiting_time parameter -> Some parameter | _ -> None)
    (fun parameter -> Bad_max_waiting_time parameter) ;
  register_error_kind
    `Permanent
    ~id:"badEndorsementDelayArg"
    ~title:"Bad -endorsement-delay arg"
    ~description:("invalid duration in -endorsement-delay")
    ~pp:(fun ppf literal ->
        Format.fprintf ppf "Bad argument value for -endorsement-delay. Expected an integer, but given '%s'" literal)
    Data_encoding.(obj1 (req "parameter" string))
    (function Bad_endorsement_delay parameter -> Some parameter | _ -> None)
    (fun parameter -> Bad_endorsement_delay parameter) ;
  register_error_kind
    `Permanent
    ~id:"badPreservedLevelsArg"
    ~title:"Bad -preserved-levels arg"
    ~description:("invalid number of levels in -preserved-levels")
    ~pp:(fun ppf literal ->
        Format.fprintf ppf "Bad argument value for -preserved_levels. Expected a positive integer, but given '%s'" literal)
    Data_encoding.(obj1 (req "parameter" string))
    (function Bad_preserved_levels parameter -> Some parameter | _ -> None)
    (fun parameter -> Bad_preserved_levels parameter)


let tez_sym =
  "\xEA\x9C\xA9"

let string_parameter =
  parameter (fun _ x -> return x)

let init_arg =
  default_arg
    ~long:"init"
    ~placeholder:"data"
    ~doc:"initial value of the contract's storage"
    ~default:"Unit"
    string_parameter

let arg_arg =
  arg
    ~long:"arg"
    ~placeholder:"data"
    ~doc:"argument passed to the contract's script, if needed"
    string_parameter

let delegate_arg =
  Client_keys.Public_key_hash.source_arg
    ~long:"delegate"
    ~placeholder:"address"
    ~doc:"delegate of the contract\n\
          Must be a known address."
    ()

let source_arg =
  arg
    ~long:"source"
    ~placeholder:"address"
    ~doc:"source of the deposits to be paid\n\
          Must be a known address."
    string_parameter

let spendable_switch =
  switch
    ~long:"spendable"
    ~doc:"allow the manager to spend the contract's tokens"
    ()

let force_switch =
  switch
    ~long:"force"
    ~short:'f'
    ~doc:"disables the node's injection checks\n\
          Force the injection of branch-invalid operation or force \
         \ the injection of block without a fitness greater than the \
         \ current head."
    ()

let minimal_timestamp_switch =
  switch
    ~long:"minimal-timestamp"
    ~doc:"Use the minimal timestamp instead of the current date \
          as timestamp of the baked block."
    ()

let delegatable_switch =
  switch
    ~long:"delegatable"
    ~doc:"allow future delegate change"
    ()

let tez_format =
  "Text format: `DDDDDDD.DDDDDD`.\n\
   Tez and mutez and separated by a period sign. Trailing and pending \
   zeroes are allowed."

let tez_parameter param =
  parameter
    (fun _ s ->
       match Tez.of_string s with
       | Some tez -> return tez
       | None -> fail (Bad_tez_arg (param, s)))

let tez_arg ~default ~parameter ~doc =
  default_arg ~long:parameter ~placeholder:"amount" ~doc ~default
    (tez_parameter ("--" ^ parameter))

let tez_param ~name ~desc next =
  Clic.param
    ~name
    ~desc:(desc ^ " in \xEA\x9C\xA9\n" ^ tez_format)
    (tez_parameter name)
    next

let fee_arg =
  arg
    ~long:"fee"
    ~placeholder:"amount"
    ~doc:"fee in \xEA\x9C\xA9 to pay to the baker"
    (tez_parameter ("--fee"))

let gas_limit_arg =
  arg
    ~long:"gas-limit"
    ~short:'G'
    ~placeholder:"amount"
    ~doc:"Set the gas limit of the transaction instead \
          of letting the client decide based on a simulation"
    (parameter (fun _ s ->
         try
           let v = Z.of_string s in
           assert Compare.Z.(v >= Z.zero) ;
           return v
         with _ -> failwith "invalid gas limit (must be a positive number)"))

let storage_limit_arg =
  arg
    ~long:"storage-limit"
    ~short:'S'
    ~placeholder:"amount"
    ~doc:"Set the storage limit of the transaction instead \
          of letting the client decide based on a simulation"
    (parameter (fun _ s ->
         try
           let v = Z.of_string s in
           assert Compare.Z.(v >= Z.zero) ;
           return v
         with _ -> failwith "invalid storage limit (must be a positive number of bytes)"))

let counter_arg =
  arg
    ~long:"counter"
    ~short:'C'
    ~placeholder:"counter"
    ~doc:"Set the counter to be used by the transaction"
    (parameter (fun _ s ->
         try
           let v = Z.of_string s in
           assert Compare.Z.(v >= Z.zero) ;
           return v
         with _ -> failwith "invalid counter (must be a positive number of bytes)"))

let max_priority_arg =
  arg
    ~long:"max-priority"
    ~placeholder:"slot"
    ~doc:"maximum allowed baking slot"
    (parameter (fun _ s ->
         try return (int_of_string s)
         with _ -> fail (Bad_max_priority s)))


let default_minimal_fees = match Tez.of_mutez 100L with None -> assert false | Some t -> t
let default_minimal_nanotez_per_gas_unit = Z.of_int 100
let default_minimal_nanotez_per_byte = Z.of_int 1000

let minimal_fees_arg =
  default_arg
    ~long:"minimal-fees"
    ~placeholder:"amount"
    ~doc:"exclude operations with fees lower than this threshold (in tez)"
    ~default:(Tez.to_string default_minimal_fees)
    (parameter (fun _ s ->
         match Tez.of_string s with
         | Some t -> return t
         | None -> fail (Bad_minimal_fees s)))

let minimal_nanotez_per_gas_unit_arg =
  default_arg
    ~long:"minimal-nanotez-per-gas-unit"
    ~placeholder:"amount"
    ~doc:"exclude operations with fees per gas lower than this threshold (in nanotez)"
    ~default:(Z.to_string default_minimal_nanotez_per_gas_unit)
    (parameter (fun _ s ->
         try return (Z.of_string s)
         with _ -> fail (Bad_minimal_fees s)))

let minimal_nanotez_per_byte_arg =
  default_arg
    ~long:"minimal-nanotez-per-byte"
    ~placeholder:"amount"
    ~default:(Z.to_string default_minimal_nanotez_per_byte)
    ~doc:"exclude operations with fees per byte lower than this threshold (in tez)"
    (parameter (fun _ s ->
         try return (Z.of_string s)
         with _ -> fail (Bad_minimal_fees s)))

let force_low_fee_arg =
  switch
    ~long:"force-low-fee"
    ~doc:"Don't check that the fee is lower than the estimated default value"
    ()

let fee_cap_arg =
  default_arg
    ~long:"fee-cap"
    ~placeholder:"amount"
    ~default:"1.0"
    ~doc:"Set the fee cap"
    (parameter (fun _ s ->
         match Tez.of_string s with
         | Some t -> return t
         | None -> failwith "Bad fee cap"))

let burn_cap_arg =
  default_arg
    ~long:"burn-cap"
    ~placeholder:"amount"
    ~default:"0"
    ~doc:"Set the burn cap"
    (parameter (fun _ s ->
         match Tez.of_string s with
         | Some t -> return t
         | None -> failwith "Bad burn cap"))

let no_waiting_for_endorsements_arg =
  switch
    ~long:"no-waiting-for-late-endorsements"
    ~doc:"Disable waiting for late endorsements"
    ()

let await_endorsements_arg =
  switch
    ~long:"await-late-endorsements"
    ~doc:"Await late endorsements when baking a block"
    ()

let endorsement_delay_arg =
  default_arg
    ~long:"endorsement-delay"
    ~placeholder:"seconds"
    ~doc:"delay before endorsing blocks\n\
          Delay between notifications of new blocks from the node and \
          production of endorsements for these blocks."
    ~default:"5"
    (parameter (fun _ s ->
         try
           let i = int_of_string s in
           fail_when (i < 0) (Bad_endorsement_delay s) >>=? fun () ->
           return (int_of_string s)
         with _ -> fail (Bad_endorsement_delay s)))

let preserved_levels_arg =
  default_arg
    ~long:"preserved-levels"
    ~placeholder:"threshold"
    ~doc:"Number of effective levels kept in the accuser's memory"
    ~default:"4096"
    (parameter (fun _ s ->
         try
           let preserved_cycles = int_of_string s in
           if preserved_cycles < 0 then
             fail (Bad_preserved_levels s)
           else
             return preserved_cycles
         with _ -> fail (Bad_preserved_levels s)))

let no_print_source_flag =
  switch
    ~long:"no-print-source"
    ~short:'q'
    ~doc:"don't print the source code\n\
          If an error is encountered, the client will print the \
          contract's source code by default.\n\
          This option disables this behaviour."
    ()

let no_confirmation =
  switch
    ~long:"no-confirmation"
    ~doc:"don't print wait for the operation to be confirmed."
    ()

module Daemon = struct
  let baking_switch =
    switch
      ~long:"baking"
      ~short:'B'
      ~doc:"run the baking daemon" ()
  let endorsement_switch =
    switch
      ~long:"endorsement"
      ~short:'E'
      ~doc:"run the endorsement daemon" ()
  let denunciation_switch =
    switch
      ~long:"denunciation"
      ~short:'D'
      ~doc:"run the denunciation daemon" ()
end
