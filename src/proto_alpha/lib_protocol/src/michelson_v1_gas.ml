(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

open Alpha_context
open Gas

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

  let bytes length =
    alloc_mbytes_cost length

  let zint z =
    alloc_bits_cost (Z.numbits z)

  let concat cost length ss =
    let rec cum acc = function
      | [] -> acc
      | s :: ss -> cum (cost (length s) +@ acc) ss in
    cum free ss

  let concat_string ss = concat string String.length ss
  let concat_bytes ss = concat bytes MBytes.length ss

  let slice_string length = string length
  let slice_bytes = alloc_cost 0

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
      3 *@ alloc_cost size

  let map_mem _key map = step_cost (map_access map)

  let map_get = map_mem

  let map_update _ _ map =
    map_access map *@ alloc_cost 3

  let map_size = step_cost 2

  let big_map_mem _key _map = step_cost 50
  let big_map_get _key _map = step_cost 50
  let big_map_update _key _value _map = step_cost 10

  let set_access : type elt. elt -> elt Script_typed_ir.set -> int
    = fun _key (module Box) ->
      log2 @@ Box.size

  let set_mem key set = step_cost (set_access key set)

  let set_update key _presence set =
    set_access key set *@ alloc_cost 3

  (* for LEFT, RIGHT, SOME *)
  let wrap = alloc_cost 1

  let mul n1 n2 =
    let steps =
      (Z.numbits (Script_int.to_zint n1))
      * (Z.numbits (Script_int.to_zint n2)) in
    let bits =
      (Z.numbits (Script_int.to_zint n1))
      + (Z.numbits (Script_int.to_zint n2)) in
    step_cost steps +@ alloc_bits_cost bits

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

  let unpack_failed bytes =
    (* We cannot instrument failed deserialization,
       so we take worst case fees: a set of size 1 bytes values. *)
    let len = MBytes.length bytes in
    (len *@ alloc_mbytes_cost 1) +@
    (len *@ (log2 len *@ (alloc_cost 3 +@ step_cost 1)))

  let address = step_cost 1
  let contract = Gas.read_bytes_cost Z.zero +@ step_cost 10000
  let transfer = step_cost 10
  let create_account = step_cost 10
  let create_contract = step_cost 10
  let implicit_account = step_cost 10
  let set_delegate = step_cost 10 +@ write_bytes_cost (Z.of_int 32)
  let balance = step_cost 1 +@ read_bytes_cost (Z.of_int 8)
  let now = step_cost 5
  let check_signature = step_cost 1000
  let hash_key = step_cost 3 +@ bytes 20
  let hash data len = 10 *@ step_cost (MBytes.length data) +@ bytes len
  let steps_to_quota = step_cost 1
  let source = step_cost 1
  let self = step_cost 1
  let amount = step_cost 1
  let compare_bool _ _ = step_cost 1
  let compare_string s1 s2 =
    step_cost ((7 + Compare.Int.max (String.length s1) (String.length s2)) / 8) +@ step_cost 1
  let compare_bytes s1 s2 =
    step_cost ((7 + Compare.Int.max (MBytes.length s1) (MBytes.length s2)) / 8) +@ step_cost 1
  let compare_tez _ _ = step_cost 1
  let compare_zint n1 n2 = step_cost ((7 + Compare.Int.max (Z.numbits n1) (Z.numbits n2)) / 8) +@ step_cost 1
  let compare_int n1 n2 = compare_zint (Script_int.to_zint n1) (Script_int.to_zint n2)
  let compare_nat = compare_int
  let compare_key_hash _ _ = alloc_bytes_cost 36
  let compare_timestamp t1 t2 = compare_zint (Script_timestamp.to_zint t1) (Script_timestamp.to_zint t2)
  let compare_address _ _ = step_cost 20

  let manager_operation = step_cost 10_000

  module Typechecking = struct
    let cycle = step_cost 1
    let bool = free
    let unit = free
    let string = string
    let bytes = bytes
    let z = zint
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
    let set_element size = log2 size *@ (alloc_cost 3 +@ step_cost 2)
    let map_element size = log2 size *@ (alloc_cost 4 +@ step_cost 2)
    let primitive_type = alloc_cost 1
    let one_arg_type = alloc_cost 2
    let two_arg_type = alloc_cost 3
    let operation b = bytes b
    let type_ nb_args = alloc_cost (nb_args + 1)

    (* Cost of parsing instruction, is cost of allocation of
       constructor + cost of contructor parameters + cost of
       allocation on the stack type *)
    let instr
      : type b a. (b, a) Script_typed_ir.instr -> cost
      = fun i ->
        let open Script_typed_ir in
        alloc_cost 1 +@ (* cost of allocation of constructor *)
        match i with
        | Drop -> alloc_cost 0
        | Dup -> alloc_cost 1
        | Swap -> alloc_cost 0
        | Const _ -> alloc_cost 1
        | Cons_pair -> alloc_cost 2
        | Car -> alloc_cost 1
        | Cdr -> alloc_cost 1
        | Cons_some -> alloc_cost 2
        | Cons_none _ -> alloc_cost 3
        | If_none _ -> alloc_cost 2
        | Left -> alloc_cost 3
        | Right -> alloc_cost 3
        | If_left _ -> alloc_cost 2
        | Cons_list -> alloc_cost 1
        | Nil -> alloc_cost 1
        | If_cons _ -> alloc_cost 2
        | List_map _ -> alloc_cost 5
        | List_iter _ -> alloc_cost 4
        | List_size -> alloc_cost 1
        | Empty_set _ -> alloc_cost 1
        | Set_iter _ -> alloc_cost 4
        | Set_mem -> alloc_cost 1
        | Set_update -> alloc_cost 1
        | Set_size -> alloc_cost 1
        | Empty_map _ -> alloc_cost 2
        | Map_map _ -> alloc_cost 5
        | Map_iter _ -> alloc_cost 4
        | Map_mem -> alloc_cost 1
        | Map_get -> alloc_cost 1
        | Map_update -> alloc_cost 1
        | Map_size -> alloc_cost 1
        | Big_map_mem -> alloc_cost 1
        | Big_map_get -> alloc_cost 1
        | Big_map_update -> alloc_cost 1
        | Concat_string -> alloc_cost 1
        | Concat_string_pair -> alloc_cost 1
        | Concat_bytes -> alloc_cost 1
        | Concat_bytes_pair -> alloc_cost 1
        | Slice_string -> alloc_cost 1
        | Slice_bytes -> alloc_cost 1
        | String_size -> alloc_cost 1
        | Bytes_size -> alloc_cost 1
        | Add_seconds_to_timestamp -> alloc_cost 1
        | Add_timestamp_to_seconds -> alloc_cost 1
        | Sub_timestamp_seconds -> alloc_cost 1
        | Diff_timestamps -> alloc_cost 1
        | Add_tez -> alloc_cost 1
        | Sub_tez -> alloc_cost 1
        | Mul_teznat -> alloc_cost 1
        | Mul_nattez -> alloc_cost 1
        | Ediv_teznat -> alloc_cost 1
        | Ediv_tez -> alloc_cost 1
        | Or -> alloc_cost 1
        | And -> alloc_cost 1
        | Xor -> alloc_cost 1
        | Not -> alloc_cost 1
        | Is_nat -> alloc_cost 1
        | Neg_nat -> alloc_cost 1
        | Neg_int -> alloc_cost 1
        | Abs_int -> alloc_cost 1
        | Int_nat -> alloc_cost 1
        | Add_intint -> alloc_cost 1
        | Add_intnat -> alloc_cost 1
        | Add_natint -> alloc_cost 1
        | Add_natnat -> alloc_cost 1
        | Sub_int -> alloc_cost 1
        | Mul_intint -> alloc_cost 1
        | Mul_intnat -> alloc_cost 1
        | Mul_natint -> alloc_cost 1
        | Mul_natnat -> alloc_cost 1
        | Ediv_intint -> alloc_cost 1
        | Ediv_intnat -> alloc_cost 1
        | Ediv_natint -> alloc_cost 1
        | Ediv_natnat -> alloc_cost 1
        | Lsl_nat -> alloc_cost 1
        | Lsr_nat -> alloc_cost 1
        | Or_nat -> alloc_cost 1
        | And_nat -> alloc_cost 1
        | And_int_nat -> alloc_cost 1
        | Xor_nat -> alloc_cost 1
        | Not_nat -> alloc_cost 1
        | Not_int -> alloc_cost 1
        | Seq _ -> alloc_cost 8
        | If _ -> alloc_cost 8
        | Loop _ -> alloc_cost 4
        | Loop_left _ -> alloc_cost 5
        | Dip _ -> alloc_cost 4
        | Exec -> alloc_cost 1
        | Lambda _ -> alloc_cost 2
        | Failwith _ -> alloc_cost 1
        | Nop -> alloc_cost 0
        | Compare _ -> alloc_cost 1
        | Eq -> alloc_cost 1
        | Neq -> alloc_cost 1
        | Lt -> alloc_cost 1
        | Gt -> alloc_cost 1
        | Le -> alloc_cost 1
        | Ge -> alloc_cost 1
        | Address -> alloc_cost 1
        | Contract _ -> alloc_cost 2
        | Transfer_tokens -> alloc_cost 1
        | Create_account -> alloc_cost 2
        | Implicit_account -> alloc_cost 1
        | Create_contract _ -> alloc_cost 8
        | Set_delegate -> alloc_cost 1
        | Now -> alloc_cost 1
        | Balance -> alloc_cost 1
        | Check_signature -> alloc_cost 1
        | Hash_key -> alloc_cost 1
        | Pack _ -> alloc_cost 2
        | Unpack _ -> alloc_cost 2
        | Blake2b -> alloc_cost 1
        | Sha256 -> alloc_cost 1
        | Sha512 -> alloc_cost 1
        | Steps_to_quota -> alloc_cost 1
        | Source -> alloc_cost 1
        | Sender -> alloc_cost 1
        | Self _ -> alloc_cost 2
        | Amount -> alloc_cost 1
  end

  module Unparse = struct
    let prim_cost l annot = Script.prim_node_cost_nonrec_of_length l annot
    let seq_cost = Script.seq_node_cost_nonrec_of_length
    let string_cost length = Script.string_node_cost_of_length length

    let cycle = step_cost 1
    let bool = prim_cost 0 []
    let unit = prim_cost 0 []
    (* We count the length of strings and bytes to prevent hidden
       miscalculations due to non detectable expansion of sharing. *)
    let string s = Script.string_node_cost s
    let bytes s = Script.bytes_node_cost s
    let z i = Script.int_node_cost i
    let int i = Script.int_node_cost (Script_int.to_zint i)
    let tez = Script.int_node_cost_of_numbits 60 (* int64 bound *)
    let timestamp x = Script_timestamp.to_zint x |> Script_int.of_zint |> int
    let operation bytes = Script.bytes_node_cost bytes
    let key = string_cost 54
    let key_hash = string_cost 36
    let signature = string_cost 128
    let contract = string_cost 36
    let pair = prim_cost 2 []
    let union = prim_cost 1 []
    let some = prim_cost 1 []
    let none = prim_cost 0 []
    let list_element = alloc_cost 2
    let set_element = alloc_cost 2
    let map_element = alloc_cost 2
    let one_arg_type = prim_cost 1
    let two_arg_type = prim_cost 2

    let set_to_list = set_to_list
    let map_to_list = map_to_list
  end

end
