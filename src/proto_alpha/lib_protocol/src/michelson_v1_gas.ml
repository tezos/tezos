(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Alpha_context
open Gas

(* FIXME: this really is a preliminary estimation of costs,
   everything in this file needs to be tweaked and proofread. *)

module Cost_of = struct
  let cycle = step_cost 1
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
    alloc_bytes_cost length

  let concat s1 s2 =
    string (String.length s1 + String.length s2)

  (* Cost per cycle of a loop, fold, etc *)
  let loop_cycle = step_cost 2

  let list_size = step_cost 1

  let log2 =
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
      2 *@ (alloc_cost (size * 2))

  let map_mem _key map = step_cost (map_access map)

  let map_get = map_mem

  let map_update _ _ map =
    map_access map *@ alloc_cost 3

  let map_size = step_cost 2

  let big_map_mem _key _map = step_cost 200
  let big_map_get _key _map = step_cost 200
  let big_map_update _key _value _map = step_cost 200

  let set_access : type elt. elt -> elt Script_typed_ir.set -> int
    = fun _key (module Box) ->
      log2 @@ Box.size

  let set_mem key set = step_cost (set_access key set)

  let set_update key _presence set =
    set_access key set *@ alloc_cost 3

  (* for LEFT, RIGHT, SOME *)
  let wrap = alloc_cost 1

  let mul n1 n2 =
    let bits =
      (Z.numbits (Script_int.to_zint n1))
      * (Z.numbits (Script_int.to_zint n2)) in
    step_cost bits +@ alloc_bits_cost bits

  let div n1 n2 =
    mul n1 n2 +@ alloc_cost 2

  let add_sub_z n1 n2 =
    let bits =
      Compare.Int.max (Z.numbits n1) (Z.numbits n2) in
    step_cost bits +@ alloc_cost bits

  let add n1 n2 =
    add_sub_z (Script_int.to_zint n1) (Script_int.to_zint n2)

  let sub = add

  let abs n =
    alloc_bits_cost (Z.numbits @@ Script_int.to_zint n)

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

  let int64_op = step_cost 1 +@ alloc_cost 1

  let z_to_int64 = step_cost 2 +@ alloc_cost 1

  let int64_to_z = step_cost 2 +@ alloc_cost 1

  let bitwise_binop n1 n2 =
    let bits = Compare.Int.max (Z.numbits (Script_int.to_zint n1)) (Z.numbits (Script_int.to_zint n2)) in
    step_cost bits +@ alloc_bits_cost bits

  let logor = bitwise_binop
  let logand = bitwise_binop
  let logxor = bitwise_binop
  let lognot n =
    let bits = Z.numbits @@ Script_int.to_zint n in
    step_cost bits +@ alloc_cost bits

  let unopt ~default = function
    | None -> default
    | Some x -> x

  let max_int = 1073741823

  let shift_left x y =
    alloc_bits_cost
      (Z.numbits (Script_int.to_zint x) +
       (unopt (Script_int.to_int y) ~default:max_int))

  let shift_right x y =
    alloc_bits_cost
      (Compare.Int.max 1
         (Z.numbits (Script_int.to_zint x) -
          unopt (Script_int.to_int y) ~default:max_int))

  let exec = step_cost 1

  let push = step_cost 1

  let compare_res = step_cost 1

  (* TODO: protocol operations *)
  let address = step_cost 3
  let contract = Gas.read_bytes_cost Z.zero +@ step_cost 3
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
  let source = step_cost 3
  let self = step_cost 3
  let amount = step_cost 1
  let compare_bool _ _ = step_cost 1
  let compare_string s1 s2 =
    step_cost (Compare.Int.max (String.length s1) (String.length s2) / 8) +@ step_cost 1
  let compare_tez _ _ = step_cost 1
  let compare_zint n1 n2 = step_cost (Compare.Int.max (Z.numbits n1) (Z.numbits n2) / 8) +@ step_cost 1
  let compare_int n1 n2 = compare_zint (Script_int.to_zint n1) (Script_int.to_zint n2)
  let compare_nat = compare_int
  let compare_key_hash _ _ = alloc_bytes_cost 36
  let compare_timestamp t1 t2 = compare_zint (Script_timestamp.to_zint t1) (Script_timestamp.to_zint t2)
  let compare_address _ _ = step_cost 20

  module Typechecking = struct
    let cycle = step_cost 1
    let bool = free
    let unit = free
    let string = string
    let int_of_string str =
      alloc_cost @@ (Pervasives.(/) (String.length str) 5)
    let tez = step_cost 1 +@ alloc_cost 1
    let string_timestamp = step_cost 3 +@ alloc_cost 3
    let key = step_cost 3 +@ alloc_cost 3
    let key_hash = step_cost 1 +@ alloc_cost 1
    let signature = step_cost 1 +@ alloc_cost 1
    let contract = step_cost 5
    let get_script = step_cost 20 +@ alloc_cost 5
    let contract_exists = step_cost 15 +@ alloc_cost 5
    let pair = alloc_cost 2
    let union = alloc_cost 1
    let lambda = alloc_cost 5 +@ step_cost 3
    let some = alloc_cost 1
    let none = alloc_cost 0
    let list_element = alloc_cost 2 +@ step_cost 1
    let set_element = alloc_cost 3 +@ step_cost 2
    let map_element = alloc_cost 4 +@ step_cost 2
    let primitive_type = alloc_cost 1
    let one_arg_type = alloc_cost 2
    let two_arg_type = alloc_cost 3
    let operation s =
      (* TODO: proper handling of (de)serialization costs *)
      let len = String.length s in
      alloc_cost len +@ step_cost (len * 10)
  end

  module Unparse = struct
    let prim_cost = alloc_cost 4 (* location, primitive name, list, annotation *)
    let string_cost length =
      alloc_cost 3 +@ alloc_bytes_cost length

    let cycle = step_cost 1
    let bool = prim_cost
    let unit = prim_cost
    let string s = string_cost (String.length s)
    (* Approximates log10(x) *)
    let int i =
      let decimal_digits = (Z.numbits (Z.abs (Script_int.to_zint i))) / 4 in
      prim_cost +@ (alloc_bytes_cost decimal_digits)
    let tez = string_cost 19 (* max length of 64 bit int *)
    let timestamp x = Script_timestamp.to_zint x |> Script_int.of_zint |> int
    let operation bytes = string_cost (MBytes.length bytes * 2)
    let key = string_cost 54
    let key_hash = string_cost 36
    let signature = string_cost 128
    let contract = string_cost 36
    let pair = prim_cost +@ alloc_cost 4
    let union = prim_cost +@ alloc_cost 2
    let lambda = prim_cost +@ alloc_cost 3
    let some = prim_cost +@ alloc_cost 2
    let none = prim_cost
    let list_element = prim_cost +@ alloc_cost 2
    let set_element = alloc_cost 2
    let map_element = alloc_cost 2
    let primitive_type = prim_cost
    let one_arg_type = prim_cost +@ alloc_cost 2
    let two_arg_type = prim_cost +@ alloc_cost 4

    let set_to_list = set_to_list
    let map_to_list = map_to_list
  end

end
