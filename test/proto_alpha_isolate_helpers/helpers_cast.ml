(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Proto_alpha.Tezos_context

exception Tez_error

let tez_of_int x = Option.unopt_exn Tez_error @@ (
    match (Tez.( *?) Tez.one (Int64.of_int x)) with
    | Error _ -> None
    | Ok x -> Some x
  )

let cents_of_int x = Option.unopt_exn Tez_error @@ (
    match (Tez.( *?) Tez.one_cent (Int64.of_int x)) with
    | Error _ -> None
    | Ok x -> Some x
  )

let tez_add x y = match Tez.(+?) x y with
  | Ok x -> x
  | Error _ -> raise Tez_error


let tez_add_int x y = tez_add x @@ tez_of_int y

let tez_sub x y = match Tez.(-?) x y with
  | Ok x -> x
  | Error _ -> raise Tez_error


let tez_sub_int x y = tez_add x @@ tez_of_int y

let ctxt_of_tc tc = (finalize tc).context
