(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type t =
  | Unaccounted
  | Limited of { remaining : int }

type cost =
  { allocations : int ;
    steps : int }

let encoding =
  let open Data_encoding in
  union
    [ case (Tag 0) int31
        (function Limited { remaining } -> Some remaining | _ -> None)
        (fun remaining -> Limited { remaining }) ;
      case (Tag 1) (constant "unaccounted")
        (function Unaccounted -> Some () | _ -> None)
        (fun () -> Unaccounted) ]

let pp ppf = function
  | Unaccounted ->
      Format.fprintf ppf "unaccounted"
  | Limited { remaining } ->
      Format.fprintf ppf "%d units remaining" remaining

let cost_encoding =
  let open Data_encoding in
  conv
    (fun { allocations ; steps } ->
       (allocations, steps))
    (fun (allocations, steps) ->
       { allocations ; steps })
    (obj2
       (req "allocations" int31)
       (req "steps" int31))

let pp_cost ppf { allocations ; steps } =
  Format.fprintf ppf
    "(steps: %d, allocs: %d)"
    steps allocations

type error += Quota_exceeded

let consume t cost = match t with
  | Unaccounted -> ok Unaccounted
  | Limited { remaining } ->
      let remaining =
        remaining
        - 2 * cost.allocations
        - 1 * cost.steps in
      if Compare.Int.(remaining <= 0)
      then error Quota_exceeded
      else ok (Limited { remaining })

let alloc_cost n =
  { allocations = n + 1 ;
    steps = 0 }

let alloc_bytes_cost n =
  alloc_cost (n / 8)

let alloc_bits_cost n =
  alloc_cost (n / 64)

let step_cost n =
  { allocations = 0 ;
    steps = n }

let free =
  { allocations = 0 ;
    steps = 0 }

let ( +@ ) x y =
  { allocations = x.allocations + y.allocations ;
    steps = x.steps + y.steps }

let ( *@ ) x y =
  { allocations = x * y.allocations ;
    steps = x * y.steps }

let () =
  let open Data_encoding in
  register_error_kind
    `Permanent
    ~id:"quotaExceededRuntimeError"
    ~title: "Quota exceeded (runtime script error)"
    ~description:
      "A script or one of its callee took too much \
       time or storage space"
    empty
    (function Quota_exceeded -> Some () | _ -> None)
    (fun () -> Quota_exceeded) ;
