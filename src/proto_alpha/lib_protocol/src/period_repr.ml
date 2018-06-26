(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type t = Int64.t
type period = t
include (Compare.Int64 : Compare.S with type t := t)
let encoding = Data_encoding.int64

let pp ppf v = Format.fprintf ppf "%Ld" v

type error += (* `Permanent *)
  | Malformed_period
  | Invalid_arg

let () =
  let open Data_encoding in
  (* Malformed period *)
  register_error_kind
    `Permanent
    ~id:"malformed_period"
    ~title:"Malformed period"
    ~description:"Period is negative."
    ~pp:(fun ppf () -> Format.fprintf ppf "Malformed period")
    empty
    (function Malformed_period -> Some () | _ -> None)
    (fun () -> Malformed_period) ;
  (* Invalid arg *)
  register_error_kind
    `Permanent
    ~id:"invalid_arg"
    ~title:"Invalid arg"
    ~description:"Negative multiple of periods are not allowed."
    ~pp:(fun ppf () -> Format.fprintf ppf "Invalid arg")
    empty
    (function Invalid_arg -> Some () | _ -> None)
    (fun () -> Invalid_arg)

let of_seconds t =
  if Compare.Int64.(t >= 0L)
  then ok t
  else error Malformed_period
let to_seconds t = t
let of_seconds_exn t =
  match of_seconds t with
  | Ok t -> t
  | _ -> invalid_arg "Period.of_seconds_exn"

let mult i p =
  (* TODO check overflow *)
  if Compare.Int32.(i < 0l)
  then error Invalid_arg
  else ok (Int64.mul (Int64.of_int32 i) p)

let one_second = of_seconds_exn 1L
let one_minute = of_seconds_exn 60L
let one_hour = of_seconds_exn 3600L
