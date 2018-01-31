(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

let map ~f = function
  | None -> None
  | Some x -> Some (f x)

let apply ~f = function
  | None -> None
  | Some x -> f x

let iter ~f = function
  | None -> ()
  | Some x -> f x

let unopt ~default = function
  | None -> default
  | Some x -> x

let unopt_map ~f ~default = function
  | None -> default
  | Some x -> f x

let unopt_exn err = function
  | Some x -> x
  | _ -> raise err

let first_some a b = match a, b with
  | None, None -> None
  | None, Some v -> Some v
  | Some v, _ -> Some v

let try_with f =
  try Some (f ()) with _ -> None

let some x = Some x
