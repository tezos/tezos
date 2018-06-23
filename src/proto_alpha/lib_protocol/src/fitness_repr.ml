(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type error += Invalid_fitness (* `Permanent *)

let () =
  register_error_kind
    `Permanent
    ~id:"invalid_fitness"
    ~title:"Invalid fitness"
    ~description:"Fitness representation should be exactly 8 bytes long."
    ~pp:(fun ppf () -> Format.fprintf ppf "Invalid fitness")
    Data_encoding.empty
    (function Invalid_fitness -> Some () | _ -> None)
    (fun () -> Invalid_fitness)

let int64_to_bytes i =
  let b = MBytes.create 8 in
  MBytes.set_int64 b 0 i;
  b

let int64_of_bytes b =
  if Compare.Int.(MBytes.length b <> 8) then
    error Invalid_fitness
  else
    ok (MBytes.get_int64 b 0)

let from_int64 fitness =
  [ MBytes.of_string Constants_repr.version_number ;
    int64_to_bytes fitness ]

let to_int64 = function
  | [ version ;
      fitness ]
    when Compare.String.
           (MBytes.to_string version = Constants_repr.version_number) ->
      int64_of_bytes fitness
  | [] -> ok 0L
  | _ -> error Invalid_fitness
