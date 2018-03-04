(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type t = { remaining : int } [@@unboxed]

type cost =
  { allocations : int ;
    steps : int }

let encoding =
  let open Data_encoding in
  conv
    (fun { remaining } ->
       (remaining))
    (fun (remaining) ->
       { remaining })
    int31

let pp ppf { remaining } =
  Format.pp_print_int ppf remaining

let of_int remaining = { remaining }

let remaining { remaining } = remaining

(* Maximum gas representable on a 64 bit system *)
let max_gas = of_int 4611686018427387903

let encoding_cost =
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

let check_error gas =
  if Compare.Int.(gas.remaining <= 0)
  then error Quota_exceeded
  else ok ()

let check gas =
  Lwt.return @@ check_error gas

let used ~original ~current =
  { remaining = original.remaining - current.remaining }

let consume t cost =
  { remaining =
      t.remaining
      - 2 * cost.allocations
      - 1 * cost.steps }

let consume_check gas cost =
  let gas = consume gas cost in
  check gas >>|? fun () ->
  gas

let consume_check_error gas cost =
  let gas = consume gas cost in
  check_error gas >|? fun () ->
  gas

(* Cost for heap allocating n words of data. *)
let alloc_cost n =
  { allocations = n + 1 ;
    steps = 0 }

let alloc_bytes_cost n =
  alloc_cost (n / 8)

let alloc_bits_cost n =
  alloc_cost (n / 64)

(* Cost for one computation step. *)
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

(* f should fail if it does not receive sufficient gas *)
let rec fold_left ~cycle_cost gas f acc l =
  consume_check gas cycle_cost >>=? fun gas ->
  match l with
  | [] -> return (acc, gas)
  | hd :: tl -> f gas hd acc >>=? fun (acc, gas) ->
      fold_left ~cycle_cost gas f acc tl

(* f should fail if it does not receive sufficient gas *)
let rec fold_right ~cycle_cost gas f base l =
  consume_check gas cycle_cost >>=? fun gas ->
  match l with
  | [] -> return (base, gas)
  | hd :: tl ->
      fold_right ~cycle_cost gas f base tl >>=? fun (acc, gas) ->
      f gas hd acc

(* f should fail if it does not receive sufficient gas *)
let rec fold_right_error ~cycle_cost gas f base l =
  consume_check_error gas cycle_cost >>? fun gas ->
  match l with
  | [] -> ok (base, gas)
  | hd :: tl ->
      fold_right_error ~cycle_cost gas f base tl >>? fun (acc, gas) ->
      f gas hd acc

(* f should fail if it does not receive sufficient gas *)
let rec fold_left_error ~cycle_cost gas f acc l =
  consume_check_error gas cycle_cost >>? fun gas ->
  match l with
  | [] -> ok (acc, gas)
  | hd :: tl -> f gas hd acc >>? fun (acc, gas) ->
      fold_left_error ~cycle_cost gas f acc tl

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
