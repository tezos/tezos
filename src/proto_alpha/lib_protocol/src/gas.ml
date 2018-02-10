(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Alpha_context

(* FIXME: this really is a preliminary estimation of costs,
   everything in this file needs to be tweaked and proofread. *)

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

let bytes_per_word = 8

let bits_per_word = 8 * bytes_per_word

let words_of_bits n =
  n / bits_per_word

let check_error gas =
  if Compare.Int.(gas.remaining <= 0)
  then error Quota_exceeded
  else ok ()

let check gas =
  Lwt.return @@ check_error gas

let word_cost = 2
let step_cost = 1

let used ~original ~current =
  { remaining = original.remaining - current.remaining }

let consume t cost =
  { remaining =
      t.remaining
      - word_cost * cost.allocations
      - step_cost * cost.steps }

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

(* Cost for one computation step. *)
let step_cost n =
  { allocations = 0 ;
    steps = n }

let free =
  { allocations = 0 ;
    steps = 0 }

let ( + ) x y =
  { allocations = x.allocations + y.allocations ;
    steps = x.steps + y.steps }

let ( * ) x y =
  { allocations = x * y.allocations ;
    steps = x * y.steps }

let max = Compare.Int.max

module Cost_of = struct
  let cycle = step_cost 1
  let typechecking_cycle = cycle
  let nop = free

  let stack_op = step_cost 1

  let bool_binop _ _ = step_cost 1
  let bool_unop _ = step_cost 1

  let pair = alloc_cost 2
  let pair_access = step_cost 1

  let cons = alloc_cost 2

  let variant_no_data = alloc_cost 1

  let branch = step_cost 2

  let string length =
    alloc_cost (length / bytes_per_word)

  let concat s1 s2 =
    let (+) = Pervasives.(+) in
    string ((String.length s1 + String.length s2) / bytes_per_word)

  (* Cost per cycle of a loop, fold, etc *)
  let loop_cycle = step_cost 2

  let list_size = step_cost 1

  let log2 =
    let (+) = Pervasives.(+) in
    let rec help acc = function
      | 0 -> acc
      | n -> help (acc + 1) (n / 2)
    in help 1

  let module_cost = alloc_cost 10

  let map_access : type key value. (key, value) Script_typed_ir.map -> int
    = fun (module Box) ->
      log2 (snd Box.boxed)

  let map_to_list : type key value. (key, value) Script_typed_ir.map -> cost
    = fun (module Box) ->
      let size = snd Box.boxed in
      2 * (alloc_cost @@ Pervasives.(size * 2))

  let map_mem _key map = step_cost (map_access map)

  let map_get = map_mem

  let map_update _ _ map =
    map_access map * alloc_cost 3

  let map_size = step_cost 2

  let big_map_mem _key _map = step_cost 200
  let big_map_get _key _map = step_cost 200
  let big_map_update _key _value _map = step_cost 200

  let set_access : type elt. elt -> elt Script_typed_ir.set -> int
    = fun _key (module Box) ->
      log2 @@ Box.size

  let set_mem key set = step_cost (set_access key set)

  let set_update key _presence set =
    set_access key set * alloc_cost 3

  (* for LEFT, RIGHT, SOME *)
  let wrap = alloc_cost 1

  let mul n1 n2 =
    let words =
      let ( * ) = Pervasives.( * ) in
      words_of_bits
        ((Z.numbits (Script_int.to_zint n1))
         * (Z.numbits (Script_int.to_zint  n2))) in
    step_cost words + alloc_cost words

  let div n1 n2 =
    mul n1 n2 + alloc_cost 2

  let add_sub_z n1 n2 =
    let words = words_of_bits
        (max (Z.numbits n1) (Z.numbits n2)) in
    step_cost (words_of_bits words) + alloc_cost words

  let add n1 n2 =
    add_sub_z (Script_int.to_zint n1) (Script_int.to_zint n2)

  let sub = add

  let abs n =
    alloc_cost (words_of_bits @@ Z.numbits @@ Script_int.to_zint n)

  let neg = abs
  let int _ = step_cost 1

  let add_timestamp t n =
    add_sub_z (Script_timestamp.to_zint t) (Script_int.to_zint n)

  let sub_timestamp t n =
    add_sub_z (Script_timestamp.to_zint t) (Script_int.to_zint n)

  let diff_timestamps t1 t2 =
    add_sub_z (Script_timestamp.to_zint t1) (Script_timestamp.to_zint t2)

  let empty_set = module_cost

  let set_size = step_cost 2

  let set_to_list : type item. item Script_typed_ir.set -> cost
    = fun (module Box) ->
      alloc_cost @@ Pervasives.(Box.size * 2)

  let empty_map = module_cost

  let int64_op = step_cost 1 + alloc_cost 1

  let z_to_int64 = step_cost 2 + alloc_cost 1

  let int64_to_z = step_cost 2 + alloc_cost 1

  let bitwise_binop n1 n2 =
    let words = words_of_bits (max (Z.numbits (Script_int.to_zint n1)) (Z.numbits (Script_int.to_zint n2))) in
    step_cost words + alloc_cost words

  let logor = bitwise_binop
  let logand = bitwise_binop
  let logxor = bitwise_binop
  let lognot n =
    let words = words_of_bits @@ Z.numbits @@ Script_int.to_zint n in
    step_cost words + alloc_cost words

  let unopt ~default = function
    | None -> default
    | Some x -> x

  let max_int = 1073741823

  let shift_left x y =
    (alloc_cost @@ words_of_bits @@
     let (+) = Pervasives.(+) in
     Z.numbits (Script_int.to_zint x) +
     (unopt (Script_int.to_int y) ~default:max_int))

  let shift_right x y =
    (alloc_cost @@ words_of_bits @@
     max 1 @@
     let (-) = Pervasives.(-) in
     Z.numbits (Script_int.to_zint x) -
     unopt (Script_int.to_int y) ~default:max_int)

  let exec = step_cost 1

  let push = step_cost 1

  let compare_res = step_cost 1

  (* TODO: protocol operations *)
  let manager = step_cost 3
  let transfer = step_cost 50
  let create_account = step_cost 20
  let create_contract = step_cost 70
  let implicit_account = step_cost 10
  let balance = step_cost 5
  let now = step_cost 3
  let check_signature = step_cost 3
  let hash_key = step_cost 3
  (* TODO: This needs to be a function of the data being hashed *)
  let hash _data = step_cost 3
  let steps_to_quota = step_cost 1
  let get_steps_to_quota gas = Script_int.abs @@ Script_int.of_int gas.remaining
  let source = step_cost 3
  let self = step_cost 3
  let amount = step_cost 1
  let compare_bool _ _ = step_cost 1
  let compare_string s1 s2 =
    step_cost (max (String.length s1) (String.length s2) / 8) + step_cost 1
  let compare_tez _ _ = step_cost 1
  let compare_zint n1 n2 = step_cost (max (Z.numbits n1) (Z.numbits n2) / 8) + step_cost 1
  let compare_int n1 n2 = compare_zint (Script_int.to_zint n1) (Script_int.to_zint n2)
  let compare_nat = compare_int
  let compare_key_hash _ _ = alloc_cost (36 / bytes_per_word)
  let compare_timestamp t1 t2 = compare_zint (Script_timestamp.to_zint t1) (Script_timestamp.to_zint t2)

  module Typechecking = struct
    let cycle = step_cost 1
    let bool = free
    let unit = free
    let string = string
    let int_of_string str =
      alloc_cost @@ (Pervasives.(/) (String.length str) 5)
    let tez = step_cost 1 + alloc_cost 1
    let string_timestamp = step_cost 3 + alloc_cost 3
    let key = step_cost 3 + alloc_cost 3
    let key_hash = step_cost 1 + alloc_cost 1
    let signature = step_cost 1 + alloc_cost 1
    let contract = step_cost 5
    let get_script = step_cost 20 + alloc_cost 5
    let contract_exists = step_cost 15 + alloc_cost 5
    let pair = alloc_cost 2
    let union = alloc_cost 1
    let lambda = alloc_cost 5 + step_cost 3
    let some = alloc_cost 1
    let none = alloc_cost 0
    let list_element = alloc_cost 2 + step_cost 1
    let set_element = alloc_cost 3 + step_cost 2
    let map_element = alloc_cost 4 + step_cost 2
    let primitive_type = alloc_cost 1
    let one_arg_type = alloc_cost 2
    let two_arg_type = alloc_cost 3
  end

  module Unparse = struct
    let prim_cost = alloc_cost 4 (* location, primitive name, list, annotation *)
    let string_cost length =
      alloc_cost 3 + alloc_cost (length / bytes_per_word)

    let cycle = step_cost 1
    let bool = prim_cost
    let unit = prim_cost
    let string s = string_cost (String.length s)
    (* Approximates log10(x) *)
    let int i =
      let decimal_digits = (Z.numbits (Z.abs (Script_int.to_zint i))) / 4 in
      prim_cost + (alloc_cost @@ decimal_digits / bytes_per_word)
    let tez = string_cost 19 (* max length of 64 bit int *)
    let timestamp x = Script_timestamp.to_zint x |> Script_int.of_zint |> int
    let key = string_cost 54
    let key_hash = string_cost 36
    let signature = string_cost 128
    let contract = string_cost 36
    let pair = prim_cost + alloc_cost 4
    let union = prim_cost + alloc_cost 2
    let lambda = prim_cost + alloc_cost 3
    let some = prim_cost + alloc_cost 2
    let none = prim_cost
    let list_element = prim_cost + alloc_cost 2
    let set_element = alloc_cost 2
    let map_element = alloc_cost 2
    let primitive_type = prim_cost
    let one_arg_type = prim_cost + alloc_cost 2
    let two_arg_type = prim_cost + alloc_cost 4

    let set_to_list = set_to_list
    let map_to_list = map_to_list
  end

end

(* f should fail if it does not receive sufficient gas *)
let rec fold_left ?(cycle_cost = Cost_of.loop_cycle) gas f acc l =
  consume_check gas cycle_cost >>=? fun gas ->
  match l with
  | [] -> return (acc, gas)
  | hd :: tl -> f gas hd acc >>=? fun (acc, gas) ->
      fold_left gas f acc tl

(* f should fail if it does not receive sufficient gas *)
let rec fold_right ?(cycle_cost = Cost_of.loop_cycle) gas f base l =
  consume_check gas cycle_cost >>=? fun gas ->
  match l with
  | [] -> return (base, gas)
  | hd :: tl ->
      fold_right gas f base tl >>=? fun (acc, gas) ->
      f gas hd acc

(* f should fail if it does not receive sufficient gas *)
let rec fold_right_error ?(cycle_cost = Cost_of.loop_cycle) gas f base l =
  consume_check_error gas cycle_cost >>? fun gas ->
  match l with
  | [] -> ok (base, gas)
  | hd :: tl ->
      fold_right_error gas f base tl >>? fun (acc, gas) ->
      f gas hd acc

(* f should fail if it does not receive sufficient gas *)
let rec fold_left_error ?(cycle_cost = Cost_of.loop_cycle) gas f acc l =
  consume_check_error gas cycle_cost >>? fun gas ->
  match l with
  | [] -> ok (acc, gas)
  | hd :: tl -> f gas hd acc >>? fun (acc, gas) ->
      fold_left_error gas f acc tl

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
