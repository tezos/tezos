(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type error += Demo_error of int

let () =
  Error_monad.register_error_kind
    `Temporary
    ~id:"unique.error.id"
    ~title:"Short error description"
    ~description:"Exhaustive error description"
    ~pp:(fun ppf i -> Format.fprintf ppf "Expected demo error: %d." i)
    Data_encoding.(obj1 (req "data" int31))
    (function Demo_error x -> Some x | _ -> None)
    (fun x -> Demo_error x)

let demo_error x : unit tzresult Lwt.t = fail (Demo_error x)
