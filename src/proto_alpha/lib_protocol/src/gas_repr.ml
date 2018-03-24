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
  | Limited of { remaining : Z.t }

type cost =
  { allocations : Z.t ;
    steps : Z.t }

let encoding =
  let open Data_encoding in
  union
    [ case (Tag 0) z
        (function Limited { remaining } -> Some remaining | _ -> None)
        (fun remaining -> Limited { remaining }) ;
      case (Tag 1) (constant "unaccounted")
        (function Unaccounted -> Some () | _ -> None)
        (fun () -> Unaccounted) ]

let pp ppf = function
  | Unaccounted ->
      Format.fprintf ppf "unaccounted"
  | Limited { remaining } ->
      Format.fprintf ppf "%s units remaining" (Z.to_string remaining)

let cost_encoding =
  let open Data_encoding in
  conv
    (fun { allocations ; steps } ->
       (allocations, steps))
    (fun (allocations, steps) ->
       { allocations ; steps })
    (obj2
       (req "allocations" z)
       (req "steps" z))

let pp_cost ppf { allocations ; steps } =
  Format.fprintf ppf
    "(steps: %s, allocs: %s)"
    (Z.to_string steps) (Z.to_string allocations)

type error += Block_quota_exceeded (* `Temporary *)
type error += Operation_quota_exceeded (* `Temporary *)

let allocation_weight = Z.of_int 2
let step_weight = Z.of_int 1

let consume block_gas operation_gas cost = match operation_gas with
  | Unaccounted -> ok (block_gas, Unaccounted)
  | Limited { remaining } ->
      let weighted_cost =
        Z.add
          (Z.mul allocation_weight cost.allocations)
          (Z.mul step_weight cost.steps) in
      let remaining =
        Z.sub remaining weighted_cost in
      let block_remaining =
        Z.sub block_gas weighted_cost in
      if Compare.Z.(remaining <= Z.zero)
      then error Operation_quota_exceeded
      else if Compare.Z.(block_remaining <= Z.zero)
      then error Block_quota_exceeded
      else ok (block_remaining, Limited { remaining })

let alloc_cost n =
  { allocations = Z.of_int (n + 1) ;
    steps = Z.zero }

let alloc_bytes_cost n =
  alloc_cost (n / 8)

let alloc_bits_cost n =
  alloc_cost (n / 64)

let step_cost n =
  { allocations = Z.zero ;
    steps = Z.of_int n }

let free =
  { allocations = Z.zero ;
    steps = Z.zero }

let ( +@ ) x y =
  { allocations = Z.add x.allocations y.allocations ;
    steps = Z.add x.steps y.steps }

let ( *@ ) x y =
  { allocations = Z.mul (Z.of_int x) y.allocations ;
    steps = Z.mul (Z.of_int x) y.steps }

let () =
  let open Data_encoding in
  register_error_kind
    `Temporary
    ~id:"gas_exhausted.operation"
    ~title: "Gas quota exceeded for the operation"
    ~description:
      "A script or one of its callee took more \
       time than the operation said it would"
    empty
    (function Operation_quota_exceeded -> Some () | _ -> None)
    (fun () -> Operation_quota_exceeded) ;
  register_error_kind
    `Temporary
    ~id:"gas_exhausted.block"
    ~title: "Gas quota exceeded for the block"
    ~description:
      "The sum of gas consumed by all the operations in the block \
       exceeds the hard gas limit per block"
    empty
    (function Block_quota_exceeded -> Some () | _ -> None)
    (fun () -> Block_quota_exceeded) ;
