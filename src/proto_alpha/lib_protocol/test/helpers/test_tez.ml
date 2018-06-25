(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Proto_alpha
open Alpha_context
open Alpha_environment

(* This module is mostly to wrap the errors from the protocol *)
module Tez = struct
  include Tez

  let ( +? ) t1 t2 = (t1 +? t2) |> wrap_error
  let ( -? ) t1 t2 = (t1 -? t2) |> wrap_error
  let ( *? ) t1 t2 = (t1 *? t2) |> wrap_error
  let ( /? ) t1 t2 = (t1 /? t2) |> wrap_error

  let ( + ) t1 t2 =
    match t1 +? t2 with
    | Ok r -> r
    | Error _ ->
        Pervasives.failwith "adding tez"

  let of_int x =
    match Tez.of_mutez (Int64.mul (Int64.of_int x) 1_000_000L) with
    | None -> invalid_arg "tez_of_int"
    | Some x -> x

  let of_mutez_exn x =
    match Tez.of_mutez x with
    | None -> invalid_arg "tez_of_mutez"
    | Some x -> x


  let max_tez =
    match Tez.of_mutez Int64.max_int with
    | None -> assert false
    | Some p -> p

end
