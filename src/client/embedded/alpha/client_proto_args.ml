(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

let tez_sym =
  "\xEA\x9C\xA9"

let tez_of_string s =
  match Tez.of_string s with
  | None -> invalid_arg "tez_of_string"
  | Some t -> t

let init = ref "Unit"
let init_arg =
  "-init",
  Arg.Set_string init,
  "The initial value of the contract's storage.\n\
   default: unit"

let arg = ref None
let arg_arg =
  "-arg",
  Arg.String (fun a -> arg := Some a),
  "The argument passed to the contract's script, if needed.\n\
   default: no argument"

let delegate = ref None
let delegate_arg =
  "-delegate",
  Arg.String (fun s -> delegate := Some s),
  "Set the delegate of the contract.\n\
   Must be a known identity."

let source = ref None
let source_arg =
  "-source",
  Arg.String (fun s -> source := Some s),
  "Set the source of the bonds to be paid.\n\
   Must be a known identity."

let spendable = ref true
let spendable_args =
  [ "-spendable",
    Arg.Set spendable,
    "Set the created contract to be spendable (default)" ;
    "-non-spendable",
    Arg.Clear spendable,
    "Set the created contract to be non spendable" ]

let force = ref false
let force_arg =
  "-force",
  Arg.Set force,
  "Force the injection of branch-invalid operation or force \
  \ the injection of bleck without a fitness greater than the \
  \ current head."

let delegatable = ref false
let delegatable_args =
  [ "-delegatable",
    Arg.Set delegatable,
    "Set the created contract to be delegatable" ;
    "-non-delegatable",
    Arg.Clear delegatable,
    "Set the created contract to be non delegatable (default)" ]

let tez_format = "text format: D,DDD,DDD.DD (centiles are optional, commas are optional)"

let tez_arg ~name ~desc ~default =
  let ref_cell = ref (tez_of_string default) in
  (ref_cell,
   (name,
    Arg.String (fun s ->
        try ref_cell := tez_of_string s
        with _ -> raise (Arg.Bad
                           ("invalid \xEA\x9C\xA9 notation in parameter " ^ name))),
    (Printf.sprintf
       "%s\ndefault: \"%s\"\n%s"
       desc
       default
       tez_format)))

let tez_param ~name ~desc next =
  Cli_entries.param
    name
    (desc ^ " in \xEA\x9C\xA9\n" ^ tez_format)
    (fun _ s ->
       try return (tez_of_string s)
       with _ -> failwith "invalid \xEA\x9C\xA9 notation")
    next

let fee, fee_arg =
  tez_arg
    ~name:"-fee"
    ~desc:"The fee in \xEA\x9C\xA9 to pay to the miner."
    ~default:"0.05"

let max_priority = ref None
let max_priority_arg =
  "-max-priority",
  Arg.String (fun s ->
      try max_priority := Some (int_of_string s)
      with _ -> raise (Arg.Bad "invalid priority in -max-priority")),
  "Set the max_priority used when looking for mining slot."

let free_mining = ref false
let free_mining_arg =
    "-free-mining", Arg.Set free_mining, "Only consider free mining slots."

let endorsement_delay = ref 15
let endorsement_delay_arg =
  "-endorsement-delay",
  Arg.String (fun s ->
      try endorsement_delay := int_of_string s
      with _ -> raise (Arg.Bad "invalid priority in -endorsement-delay")),
  "Set the delay used before to endorse the current block."

module Daemon = struct
  let all = ref true
  let arg r = Arg.Unit (fun () -> all := false; r := true)
  let mining = ref false
  let mining_arg =
    "-mining", arg mining, "Run the mining daemon"
  let endorsement = ref false
  let endorsement_arg =
    "-endorsement", arg endorsement, "Run the endorsement daemon"
  let denunciation = ref false
  let denunciation_arg =
    "-denunciation", arg denunciation, "Run the denunciation daemon"
end
