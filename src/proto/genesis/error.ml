(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Error_monad

type error += Parsing_error
type error += Invalid_signature

let () =
  register_error_kind
    `Temporary
    ~id:"parsing_error"
    ~title:"Parsing error"
    ~description:"Raised when an operation has not been parsed correctly"
    ~pp:(fun ppf () -> Format.fprintf ppf "Operation parsing error")
    Data_encoding.empty
    (function Parsing_error -> Some () | _ -> None)
    (fun () -> Parsing_error)

let () =
  register_error_kind
    `Temporary
    ~id:"invalid_signature"
    ~title:"Invalid signature"
    ~description:"Raised when the provided signature is invalid"
    ~pp:(fun ppf () -> Format.fprintf ppf "Invalid signature")
    Data_encoding.empty
    (function Invalid_signature -> Some () | _ -> None)
    (fun () -> Invalid_signature)

let parsing_error = error Parsing_error
let fail_parsing_error = fail Parsing_error
let invalid_signature = error Invalid_signature
let fail_invalid_signature = fail Invalid_signature
