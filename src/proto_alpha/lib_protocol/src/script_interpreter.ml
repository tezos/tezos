(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Alpha_context
open Script
open Script_typed_ir
open Script_ir_translator

(* ---- Run-time errors -----------------------------------------------------*)

type error += Reject of Script.location
type error += Overflow of Script.location
type error += Runtime_contract_error : Contract.t * Script.expr -> error

let () =
  let open Data_encoding in
  (* Reject *)
  register_error_kind
    `Temporary
    ~id:"scriptRejectedRuntimeError"
    ~title: "Script failed (runtime script error)"
    ~description: "A FAIL instruction was reached"
    (obj1 (req "location" Script.location_encoding))
    (function Reject loc -> Some loc | _ -> None)
    (fun loc -> Reject loc);
  (* Overflow *)
  register_error_kind
    `Temporary
    ~id:"scriptOverflowRuntimeError"
    ~title: "Script failed (overflow error)"
    ~description: "A FAIL instruction was reached due to the detection of an overflow"
    (obj1 (req "location" Script.location_encoding))
    (function Overflow loc -> Some loc | _ -> None)
    (fun loc -> Overflow loc);
  (* Runtime contract error *)
  register_error_kind
    `Temporary
    ~id:"scriptRuntimeError"
    ~title: "Script runtime error"
    ~description: "Toplevel error for all runtime script errors"
    (obj2
       (req "contractHandle" Contract.encoding)
       (req "contractCode" Script.expr_encoding))
    (function
      | Runtime_contract_error (contract, expr) ->
          Some (contract, expr)
      | _ -> None)
    (fun (contract, expr) ->
       Runtime_contract_error (contract, expr))

(* ---- interpreter ---------------------------------------------------------*)

type 'tys stack =
  | Item : 'ty * 'rest stack -> ('ty * 'rest) stack
  | Empty : end_of_stack stack

let unparse_stack ctxt (stack, stack_ty) =
  (* We drop the gas limit as this function is only used for debugging/errors. *)
  let ctxt = Gas.set_unlimited ctxt in
  let rec unparse_stack
    : type a. a stack * a stack_ty -> Script.expr list
    = function
      | Empty, Empty_t -> []
      | Item (v, rest), Item_t (ty, rest_ty, _) ->
          match unparse_data ctxt ty v with
          | Ok (data, _ctxt) -> Micheline.strip_locations data :: unparse_stack (rest, rest_ty)
          | Error _ -> assert false in
  unparse_stack (stack, stack_ty)

module Interp_costs = Michelson_v1_gas.Cost_of

type execution_trace =
  (Script.location * Gas.t * Script.expr list) list

let rec interp
  : type p r.
    (?log: execution_trace ref ->
     context ->
     source: Contract.t -> payer:Contract.t -> self: Contract.t -> Tez.t ->
     (p, r) lambda -> p ->
     (r * context) tzresult Lwt.t)
  = fun ?log ctxt ~source ~payer ~self amount (Lam (code, _)) arg ->
    let rec step
      : type b a.
        context -> (b, a) descr -> b stack ->
        (a stack * context) tzresult Lwt.t =
      fun ctxt ({ instr ; loc ; _ } as descr) stack ->
        Lwt.return (Gas.consume ctxt Interp_costs.cycle) >>=? fun ctxt ->
        let logged_return : type a b.
          (b, a) descr ->
          a stack * context ->
          (a stack * context) tzresult Lwt.t =
          fun descr (ret, ctxt) ->
            match log with
            | None -> return (ret, ctxt)
            | Some log ->
                log := (descr.loc, Gas.level ctxt, unparse_stack ctxt (ret, descr.aft)) :: !log ;
                return (ret, ctxt) in
        let consume_gas_terop : type ret arg1 arg2 arg3 rest.
          (_ * (_ * (_ * rest)), ret * rest) descr ->
          ((arg1 -> arg2 -> arg3 -> ret) * arg1 * arg2 * arg3) ->
          (arg1 -> arg2 -> arg3 -> Gas.cost) ->
          rest stack ->
          ((ret * rest) stack * context) tzresult Lwt.t =
          fun descr (op, x1, x2, x3) cost_func rest ->
            Lwt.return (Gas.consume ctxt (cost_func x1 x2 x3)) >>=? fun ctxt ->
            logged_return descr (Item (op x1 x2 x3, rest), ctxt) in
        let consume_gas_binop : type ret arg1 arg2 rest.
          (_ * (_ * rest), ret * rest) descr ->
          ((arg1 -> arg2 -> ret) * arg1 * arg2) ->
          (arg1 -> arg2 -> Gas.cost) ->
          rest stack ->
          context ->
          ((ret * rest) stack * context) tzresult Lwt.t =
          fun descr (op, x1, x2) cost_func rest ctxt ->
            Lwt.return (Gas.consume ctxt (cost_func x1 x2)) >>=? fun ctxt ->
            logged_return descr (Item (op x1 x2, rest), ctxt) in
        let consume_gas_unop : type ret arg rest.
          (_ * rest, ret * rest) descr ->
          ((arg -> ret) * arg) ->
          (arg -> Gas.cost) ->
          rest stack ->
          context ->
          ((ret * rest) stack * context) tzresult Lwt.t =
          fun descr (op, arg) cost_func rest ctxt ->
            Lwt.return (Gas.consume ctxt (cost_func arg)) >>=? fun ctxt ->
            logged_return descr (Item (op arg, rest), ctxt) in
        let consume_gaz_comparison :
          type t rest.
          (t * (t * rest), Script_int.z Script_int.num * rest) descr ->
          (t -> t -> int) ->
          (t -> t -> Gas.cost) ->
          t -> t ->
          rest stack ->
          ((Script_int.z Script_int.num * rest) stack * context) tzresult Lwt.t =
          fun descr op cost x1 x2 rest ->
            Lwt.return (Gas.consume ctxt (cost x1 x2)) >>=? fun ctxt ->
            logged_return descr (Item (Script_int.of_int @@ op x1 x2, rest), ctxt) in
        let create_contract :
          type param rest storage.
          (_, internal_operation * (Contract.t * rest)) descr ->
          manager:public_key_hash -> delegate:public_key_hash option -> spendable:bool ->
          delegatable:bool -> credit:Tez.t -> code:prim Micheline.canonical ->
          init:storage -> param_type:param ty -> storage_type:storage ty ->
          rest:rest stack ->
          ((internal_operation * (Contract.t * rest)) stack * context) tzresult Lwt.t =
          fun descr ~manager ~delegate ~spendable ~delegatable
            ~credit ~code ~init ~param_type ~storage_type ~rest ->
            Lwt.return (Gas.consume ctxt Interp_costs.create_contract) >>=? fun ctxt ->
            let code =
              Micheline.strip_locations
                (Seq (0, [ Prim (0, K_parameter, [ unparse_ty None param_type ], None) ;
                           Prim (0, K_storage, [ unparse_ty None storage_type ], None) ;
                           Prim (0, K_code, [ Micheline.root code ], None) ], None)) in
            Lwt.return @@ unparse_data ctxt storage_type init >>=? fun (storage, ctxt) ->
            let storage = Micheline.strip_locations storage in
            Contract.spend_from_script ctxt self credit >>=? fun ctxt ->
            Contract.fresh_contract_from_current_nonce ctxt >>=? fun (ctxt, contract) ->
            let operation =
              Origination
                { credit ; manager ; delegate ; preorigination = Some contract ;
                  delegatable ; script = Some { code ; storage } ; spendable } in
            logged_return descr (Item ({ source = self ; operation ; signature = None },
                                       Item (contract, rest)), ctxt) in
        let logged_return :
          a stack * context ->
          (a stack * context) tzresult Lwt.t =
          logged_return descr in
        match instr, stack with
        (* stack ops *)
        | Drop, Item (_, rest) ->
            Lwt.return (Gas.consume ctxt Interp_costs.stack_op) >>=? fun ctxt ->
            logged_return (rest, ctxt)
        | Dup, Item (v, rest) ->
            Lwt.return (Gas.consume ctxt Interp_costs.stack_op) >>=? fun ctxt ->
            logged_return (Item (v, Item (v, rest)), ctxt)
        | Swap, Item (vi, Item (vo, rest)) ->
            Lwt.return (Gas.consume ctxt Interp_costs.stack_op) >>=? fun ctxt ->
            logged_return (Item (vo, Item (vi, rest)), ctxt)
        | Const v, rest ->
            Lwt.return (Gas.consume ctxt Interp_costs.push) >>=? fun ctxt ->
            logged_return (Item (v, rest), ctxt)
        (* options *)
        | Cons_some, Item (v, rest) ->
            Lwt.return (Gas.consume ctxt Interp_costs.wrap) >>=? fun ctxt ->
            logged_return (Item (Some v, rest), ctxt)
        | Cons_none _, rest ->
            Lwt.return (Gas.consume ctxt Interp_costs.variant_no_data) >>=? fun ctxt ->
            logged_return (Item (None, rest), ctxt)
        | If_none (bt, _), Item (None, rest) ->
            Lwt.return (Gas.consume ctxt Interp_costs.branch) >>=? fun ctxt ->
            step ctxt bt rest
        | If_none (_, bf), Item (Some v, rest) ->
            Lwt.return (Gas.consume ctxt Interp_costs.branch) >>=? fun ctxt ->
            step ctxt bf (Item (v, rest))
        (* pairs *)
        | Cons_pair, Item (a, Item (b, rest)) ->
            Lwt.return (Gas.consume ctxt Interp_costs.pair) >>=? fun ctxt ->
            logged_return (Item ((a, b), rest), ctxt)
        | Car, Item ((a, _), rest) ->
            Lwt.return (Gas.consume ctxt Interp_costs.pair_access) >>=? fun ctxt ->
            logged_return (Item (a, rest), ctxt)
        | Cdr, Item ((_, b), rest) ->
            Lwt.return (Gas.consume ctxt Interp_costs.pair_access) >>=? fun ctxt ->
            logged_return (Item (b, rest), ctxt)
        (* unions *)
        | Left, Item (v, rest) ->
            Lwt.return (Gas.consume ctxt Interp_costs.wrap) >>=? fun ctxt ->
            logged_return (Item (L v, rest), ctxt)
        | Right, Item (v, rest) ->
            Lwt.return (Gas.consume ctxt Interp_costs.wrap) >>=? fun ctxt ->
            logged_return (Item (R v, rest), ctxt)
        | If_left (bt, _), Item (L v, rest) ->
            Lwt.return (Gas.consume ctxt Interp_costs.branch) >>=? fun ctxt ->
            step ctxt bt (Item (v, rest))
        | If_left (_, bf), Item (R v, rest) ->
            Lwt.return (Gas.consume ctxt Interp_costs.branch) >>=? fun ctxt ->
            step ctxt bf (Item (v, rest))
        (* lists *)
        | Cons_list, Item (hd, Item (tl, rest)) ->
            Lwt.return (Gas.consume ctxt Interp_costs.cons) >>=? fun ctxt ->
            logged_return (Item (hd :: tl, rest), ctxt)
        | Nil, rest ->
            Lwt.return (Gas.consume ctxt Interp_costs.variant_no_data) >>=? fun ctxt ->
            logged_return (Item ([], rest), ctxt)
        | If_cons (_, bf), Item ([], rest) ->
            Lwt.return (Gas.consume ctxt Interp_costs.branch) >>=? fun ctxt ->
            step ctxt bf rest
        | If_cons (bt, _), Item (hd :: tl, rest) ->
            Lwt.return (Gas.consume ctxt Interp_costs.branch) >>=? fun ctxt ->
            step ctxt bt (Item (hd, Item (tl, rest)))
        | List_map, Item (lam, Item (l, rest)) ->
            let rec loop rest ctxt l acc =
              Lwt.return (Gas.consume ctxt Interp_costs.loop_cycle) >>=? fun ctxt ->
              match l with
              | [] -> return (List.rev acc, ctxt)
              | hd :: tl ->
                  interp ?log ctxt ~source ~payer ~self amount lam hd
                  >>=? fun (hd, ctxt) ->
                  loop rest ctxt tl (hd :: acc)
            in loop rest ctxt l [] >>=? fun (res, ctxt) ->
            logged_return (Item (res, rest), ctxt)
        | List_map_body body, Item (l, rest) ->
            let rec loop rest ctxt l acc =
              Lwt.return (Gas.consume ctxt Interp_costs.loop_cycle) >>=? fun ctxt ->
              match l with
              | [] -> return (Item (List.rev acc, rest), ctxt)
              | hd :: tl ->
                  step ctxt body (Item (hd, rest))
                  >>=? fun (Item (hd, rest), ctxt) ->
                  loop rest ctxt tl (hd :: acc)
            in loop rest ctxt l [] >>=? fun (res, ctxt) ->
            logged_return (res, ctxt)
        | List_reduce, Item (lam, Item (l, Item (init, rest))) ->
            let rec loop rest ctxt l acc =
              Lwt.return (Gas.consume ctxt Interp_costs.loop_cycle) >>=? fun ctxt ->
              match l with
              | [] -> return (acc, ctxt)
              | hd :: tl ->
                  interp ?log ctxt ~source ~payer ~self amount lam (hd, acc)
                  >>=? fun (acc, ctxt) ->
                  loop rest ctxt tl acc
            in loop rest ctxt l init >>=? fun (res, ctxt) ->
            logged_return (Item (res, rest), ctxt)
        | List_size, Item (list, rest) ->
            Lwt.return
              (List.fold_left
                 (fun acc _ ->
                    acc >>? fun (size, ctxt) ->
                    Gas.consume ctxt Interp_costs.list_size >>? fun ctxt ->
                    ok (size + 1 (* FIXME: overflow *), ctxt))
                 (ok (0, ctxt)) list) >>=? fun (len, ctxt) ->
            logged_return (Item (Script_int.(abs (of_int len)), rest), ctxt)
        | List_iter body, Item (l, init) ->
            let rec loop ctxt l stack =
              Lwt.return (Gas.consume ctxt Interp_costs.loop_cycle) >>=? fun ctxt ->
              match l with
              | [] -> return (stack, ctxt)
              | hd :: tl ->
                  step ctxt body (Item (hd, stack))
                  >>=? fun (stack, ctxt) ->
                  loop ctxt tl stack
            in loop ctxt l init >>=? fun (res, ctxt) ->
            logged_return (res, ctxt)
        (* sets *)
        | Empty_set t, rest ->
            Lwt.return (Gas.consume ctxt Interp_costs.empty_set) >>=? fun ctxt ->
            logged_return (Item (empty_set t, rest), ctxt)
        | Set_reduce, Item (lam, Item (set, Item (init, rest))) ->
            Lwt.return (Gas.consume ctxt (Interp_costs.set_to_list set)) >>=? fun ctxt ->
            let l = List.rev (set_fold (fun e acc -> e :: acc) set []) in
            let rec loop rest ctxt l acc =
              Lwt.return (Gas.consume ctxt Interp_costs.loop_cycle) >>=? fun ctxt ->
              match l with
              | [] -> return (acc, ctxt)
              | hd :: tl ->
                  interp ?log ctxt ~source ~payer ~self amount lam (hd, acc)
                  >>=? fun (acc, ctxt) ->
                  loop rest ctxt tl acc
            in loop rest ctxt l init >>=? fun (res, ctxt) ->
            logged_return (Item (res, rest), ctxt)
        | Set_iter body, Item (set, init) ->
            Lwt.return (Gas.consume ctxt (Interp_costs.set_to_list set)) >>=? fun ctxt ->
            let l = List.rev (set_fold (fun e acc -> e :: acc) set []) in
            let rec loop ctxt l stack =
              Lwt.return (Gas.consume ctxt Interp_costs.loop_cycle) >>=? fun ctxt ->
              match l with
              | [] -> return (stack, ctxt)
              | hd :: tl ->
                  step ctxt body (Item (hd, stack))
                  >>=? fun (stack, ctxt) ->
                  loop ctxt tl stack
            in loop ctxt l init >>=? fun (res, ctxt) ->
            logged_return (res, ctxt)
        | Set_mem, Item (v, Item (set, rest)) ->
            consume_gas_binop descr (set_mem, v, set) Interp_costs.set_mem rest ctxt
        | Set_update, Item (v, Item (presence, Item (set, rest))) ->
            consume_gas_terop descr (set_update, v, presence, set) Interp_costs.set_update rest
        | Set_size, Item (set, rest) ->
            consume_gas_unop descr (set_size, set) (fun _ -> Interp_costs.set_size) rest ctxt
        (* maps *)
        | Empty_map (t, _), rest ->
            Lwt.return (Gas.consume ctxt Interp_costs.empty_map) >>=? fun ctxt ->
            logged_return (Item (empty_map t, rest), ctxt)
        | Map_map, Item (lam, Item (map, rest)) ->
            Lwt.return (Gas.consume ctxt (Interp_costs.map_to_list map)) >>=? fun ctxt ->
            let l = List.rev (map_fold (fun k v acc -> (k, v) :: acc) map []) in
            let rec loop rest ctxt l acc =
              Lwt.return (Gas.consume ctxt Interp_costs.loop_cycle) >>=? fun ctxt ->
              match l with
              | [] -> return (acc, ctxt)
              | (k, _) as hd :: tl ->
                  interp ?log ctxt ~source ~payer ~self amount lam hd
                  >>=? fun (hd, ctxt) ->
                  loop rest ctxt tl (map_update k (Some hd) acc)
            in loop rest ctxt l (empty_map (map_key_ty map)) >>=? fun (res, ctxt) ->
            logged_return (Item (res, rest), ctxt)
        | Map_reduce, Item (lam, Item (map, Item (init, rest))) ->
            Lwt.return (Gas.consume ctxt (Interp_costs.map_to_list map)) >>=? fun ctxt ->
            let l = List.rev (map_fold (fun k v acc -> (k, v) :: acc) map []) in
            let rec loop rest ctxt l acc =
              Lwt.return (Gas.consume ctxt Interp_costs.loop_cycle) >>=? fun ctxt ->
              match l with
              | [] -> return (acc, ctxt)
              | hd :: tl ->
                  interp ?log ctxt ~source ~payer ~self amount lam (hd, acc)
                  >>=? fun (acc, ctxt) ->
                  loop rest ctxt tl acc
            in loop rest ctxt l init >>=? fun (res, ctxt) ->
            logged_return (Item (res, rest), ctxt)
        | Map_iter body, Item (map, init) ->
            Lwt.return (Gas.consume ctxt (Interp_costs.map_to_list map)) >>=? fun ctxt ->
            let l = List.rev (map_fold (fun k v acc -> (k, v) :: acc) map []) in
            let rec loop ctxt l stack =
              Lwt.return (Gas.consume ctxt Interp_costs.loop_cycle) >>=? fun ctxt ->
              match l with
              | [] -> return (stack, ctxt)
              | hd :: tl ->
                  step ctxt body (Item (hd, stack))
                  >>=? fun (stack, ctxt) ->
                  loop ctxt tl stack
            in loop ctxt l init >>=? fun (res, ctxt) ->
            logged_return (res, ctxt)
        | Map_mem, Item (v, Item (map, rest)) ->
            consume_gas_binop descr (map_mem, v, map) Interp_costs.map_mem rest ctxt
        | Map_get, Item (v, Item (map, rest)) ->
            consume_gas_binop descr (map_get, v, map) Interp_costs.map_get rest ctxt
        | Map_update, Item (k, Item (v, Item (map, rest))) ->
            consume_gas_terop descr (map_update, k, v, map) Interp_costs.map_update rest
        | Map_size, Item (map, rest) ->
            consume_gas_unop descr (map_size, map) (fun _ -> Interp_costs.map_size) rest ctxt
        (* Big map operations *)
        | Big_map_mem, Item (key, Item (map, rest)) ->
            Lwt.return (Gas.consume ctxt (Interp_costs.big_map_mem key map)) >>=? fun ctxt ->
            Script_ir_translator.big_map_mem ctxt self key map >>=? fun (res, ctxt) ->
            logged_return (Item (res, rest), ctxt)
        | Big_map_get, Item (key, Item (map, rest)) ->
            Lwt.return (Gas.consume ctxt (Interp_costs.big_map_get key map)) >>=? fun ctxt ->
            Script_ir_translator.big_map_get ctxt self key map >>=? fun (res, ctxt) ->
            logged_return (Item (res, rest), ctxt)
        | Big_map_update, Item (key, Item (maybe_value, Item (map, rest))) ->
            consume_gas_terop descr
              (Script_ir_translator.big_map_update, key, maybe_value, map)
              Interp_costs.big_map_update rest
        (* timestamp operations *)
        | Add_seconds_to_timestamp, Item (n, Item (t, rest)) ->
            consume_gas_binop descr
              (Script_timestamp.add_delta, t, n)
              Interp_costs.add_timestamp rest ctxt
        | Add_timestamp_to_seconds, Item (t, Item (n, rest)) ->
            consume_gas_binop descr (Script_timestamp.add_delta, t, n)
              Interp_costs.add_timestamp rest ctxt
        | Sub_timestamp_seconds, Item (t, Item (s, rest)) ->
            consume_gas_binop descr (Script_timestamp.sub_delta, t, s)
              Interp_costs.sub_timestamp rest ctxt
        | Diff_timestamps, Item (t1, Item (t2, rest)) ->
            consume_gas_binop descr (Script_timestamp.diff, t1, t2)
              Interp_costs.diff_timestamps rest ctxt
        (* string operations *)
        | Concat, Item (x, Item (y, rest)) ->
            consume_gas_binop descr ((^), x, y) Interp_costs.concat rest ctxt
        (* currency operations *)
        | Add_tez, Item (x, Item (y, rest)) ->
            Lwt.return (Gas.consume ctxt Interp_costs.int64_op) >>=? fun ctxt ->
            Lwt.return Tez.(x +? y) >>=? fun res ->
            logged_return (Item (res, rest), ctxt)
        | Sub_tez, Item (x, Item (y, rest)) ->
            Lwt.return (Gas.consume ctxt Interp_costs.int64_op) >>=? fun ctxt ->
            Lwt.return Tez.(x -? y) >>=? fun res ->
            logged_return (Item (res, rest), ctxt)
        | Mul_teznat, Item (x, Item (y, rest)) ->
            Lwt.return (Gas.consume ctxt Interp_costs.int64_op) >>=? fun ctxt ->
            Lwt.return (Gas.consume ctxt Interp_costs.z_to_int64) >>=? fun ctxt ->
            begin
              match Script_int.to_int64 y with
              | None -> fail (Overflow loc)
              | Some y ->
                  Lwt.return Tez.(x *? y) >>=? fun res ->
                  logged_return (Item (res, rest), ctxt)
            end
        | Mul_nattez, Item (y, Item (x, rest)) ->
            Lwt.return (Gas.consume ctxt Interp_costs.int64_op) >>=? fun ctxt ->
            Lwt.return (Gas.consume ctxt Interp_costs.z_to_int64) >>=? fun ctxt ->
            begin
              match Script_int.to_int64 y with
              | None -> fail (Overflow loc)
              | Some y ->
                  Lwt.return Tez.(x *? y) >>=? fun res ->
                  logged_return (Item (res, rest), ctxt)
            end
        (* boolean operations *)
        | Or, Item (x, Item (y, rest)) ->
            consume_gas_binop descr ((||), x, y) Interp_costs.bool_binop rest ctxt
        | And, Item (x, Item (y, rest)) ->
            consume_gas_binop descr ((&&), x, y) Interp_costs.bool_binop rest ctxt
        | Xor, Item (x, Item (y, rest)) ->
            consume_gas_binop descr (Compare.Bool.(<>), x, y) Interp_costs.bool_binop rest ctxt
        | Not, Item (x, rest) ->
            consume_gas_unop descr (not, x) Interp_costs.bool_unop rest ctxt
        (* integer operations *)
        | Is_nat, Item (x, rest) ->
            consume_gas_unop descr (Script_int.is_nat, x) Interp_costs.abs rest ctxt
        | Abs_int, Item (x, rest) ->
            consume_gas_unop descr (Script_int.abs, x) Interp_costs.abs rest ctxt
        | Int_nat, Item (x, rest) ->
            consume_gas_unop descr (Script_int.int, x) Interp_costs.int rest ctxt
        | Neg_int, Item (x, rest) ->
            consume_gas_unop descr (Script_int.neg, x) Interp_costs.neg rest ctxt
        | Neg_nat, Item (x, rest) ->
            consume_gas_unop descr (Script_int.neg, x) Interp_costs.neg rest ctxt
        | Add_intint, Item (x, Item (y, rest)) ->
            consume_gas_binop descr (Script_int.add, x, y) Interp_costs.add rest ctxt
        | Add_intnat, Item (x, Item (y, rest)) ->
            consume_gas_binop descr (Script_int.add, x, y) Interp_costs.add rest ctxt
        | Add_natint, Item (x, Item (y, rest)) ->
            consume_gas_binop descr (Script_int.add, x, y) Interp_costs.add rest ctxt
        | Add_natnat, Item (x, Item (y, rest)) ->
            consume_gas_binop descr (Script_int.add_n, x, y) Interp_costs.add rest ctxt
        | Sub_int, Item (x, Item (y, rest)) ->
            consume_gas_binop descr (Script_int.sub, x, y) Interp_costs.sub rest ctxt
        | Mul_intint, Item (x, Item (y, rest)) ->
            consume_gas_binop descr (Script_int.mul, x, y) Interp_costs.mul rest ctxt
        | Mul_intnat, Item (x, Item (y, rest)) ->
            consume_gas_binop descr (Script_int.mul, x, y) Interp_costs.mul rest ctxt
        | Mul_natint, Item (x, Item (y, rest)) ->
            consume_gas_binop descr (Script_int.mul, x, y) Interp_costs.mul rest ctxt
        | Mul_natnat, Item (x, Item (y, rest)) ->
            consume_gas_binop descr (Script_int.mul_n, x, y) Interp_costs.mul rest ctxt
        | Ediv_teznat, Item (x, Item (y, rest)) ->
            Lwt.return (Gas.consume ctxt Interp_costs.int64_to_z) >>=? fun ctxt ->
            let x = Script_int.of_int64 (Tez.to_mutez x) in
            consume_gas_binop descr
              ((fun x y ->
                  match Script_int.ediv x y with
                  | None -> None
                  | Some (q, r) ->
                      match Script_int.to_int64 q,
                            Script_int.to_int64 r with
                      | Some q, Some r ->
                          begin
                            match Tez.of_mutez q, Tez.of_mutez r with
                            | Some q, Some r -> Some (q,r)
                            (* Cannot overflow *)
                            | _ -> assert false
                          end
                      (* Cannot overflow *)
                      | _ -> assert false),
               x, y)
              Interp_costs.div
              rest
              ctxt
        | Ediv_tez, Item (x, Item (y, rest)) ->
            Lwt.return (Gas.consume ctxt Interp_costs.int64_to_z) >>=? fun ctxt ->
            Lwt.return (Gas.consume ctxt Interp_costs.int64_to_z) >>=? fun ctxt ->
            let x = Script_int.abs (Script_int.of_int64 (Tez.to_mutez x)) in
            let y = Script_int.abs (Script_int.of_int64 (Tez.to_mutez y)) in
            consume_gas_binop descr
              ((fun x y -> match Script_int.ediv_n x y with
                  | None -> None
                  | Some (q, r) ->
                      match Script_int.to_int64 r with
                      | None -> assert false (* Cannot overflow *)
                      | Some r ->
                          match Tez.of_mutez r with
                          | None -> assert false (* Cannot overflow *)
                          | Some r -> Some (q, r)),
               x, y)
              Interp_costs.div
              rest
              ctxt
        | Ediv_intint, Item (x, Item (y, rest)) ->
            consume_gas_binop descr (Script_int.ediv, x, y) Interp_costs.div rest ctxt
        | Ediv_intnat, Item (x, Item (y, rest)) ->
            consume_gas_binop descr (Script_int.ediv, x, y) Interp_costs.div rest ctxt
        | Ediv_natint, Item (x, Item (y, rest)) ->
            consume_gas_binop descr (Script_int.ediv, x, y) Interp_costs.div rest ctxt
        | Ediv_natnat, Item (x, Item (y, rest)) ->
            consume_gas_binop descr (Script_int.ediv_n, x, y) Interp_costs.div rest ctxt
        | Lsl_nat, Item (x, Item (y, rest)) ->
            Lwt.return (Gas.consume ctxt (Interp_costs.shift_left x y)) >>=? fun ctxt ->
            begin
              match Script_int.shift_left_n x y with
              | None -> fail (Overflow loc)
              | Some x -> logged_return (Item (x, rest), ctxt)
            end
        | Lsr_nat, Item (x, Item (y, rest)) ->
            Lwt.return (Gas.consume ctxt (Interp_costs.shift_right x y)) >>=? fun ctxt ->
            begin
              match Script_int.shift_right_n x y with
              | None -> fail (Overflow loc)
              | Some r -> logged_return (Item (r, rest), ctxt)
            end
        | Or_nat, Item (x, Item (y, rest)) ->
            consume_gas_binop descr (Script_int.logor, x, y) Interp_costs.logor rest ctxt
        | And_nat, Item (x, Item (y, rest)) ->
            consume_gas_binop descr (Script_int.logand, x, y) Interp_costs.logand rest ctxt
        | And_int_nat, Item (x, Item (y, rest)) ->
            consume_gas_binop descr (Script_int.logand, x, y) Interp_costs.logand rest ctxt
        | Xor_nat, Item (x, Item (y, rest)) ->
            consume_gas_binop descr (Script_int.logxor, x, y) Interp_costs.logxor rest ctxt
        | Not_int, Item (x, rest) ->
            consume_gas_unop descr (Script_int.lognot, x) Interp_costs.lognot rest ctxt
        | Not_nat, Item (x, rest) ->
            consume_gas_unop descr (Script_int.lognot, x) Interp_costs.lognot rest ctxt
        (* control *)
        | Seq (hd, tl), stack ->
            step ctxt hd stack >>=? fun (trans, ctxt) ->
            step ctxt tl trans
        | If (bt, _), Item (true, rest) ->
            Lwt.return (Gas.consume ctxt Interp_costs.branch) >>=? fun ctxt ->
            step ctxt bt rest
        | If (_, bf), Item (false, rest) ->
            Lwt.return (Gas.consume ctxt Interp_costs.branch) >>=? fun ctxt ->
            step ctxt bf rest
        | Loop body, Item (true, rest) ->
            Lwt.return (Gas.consume ctxt Interp_costs.loop_cycle) >>=? fun ctxt ->
            step ctxt body rest >>=? fun (trans, ctxt) ->
            step ctxt descr trans
        | Loop _, Item (false, rest) ->
            logged_return (rest, ctxt)
        | Loop_left body, Item (L v, rest) ->
            Lwt.return (Gas.consume ctxt Interp_costs.loop_cycle) >>=? fun ctxt ->
            step ctxt body (Item (v, rest)) >>=? fun (trans, ctxt) ->
            step ctxt descr trans
        | Loop_left _, Item (R v, rest) ->
            Lwt.return (Gas.consume ctxt Interp_costs.loop_cycle) >>=? fun ctxt ->
            logged_return (Item (v, rest), ctxt)
        | Dip b, Item (ign, rest) ->
            Lwt.return (Gas.consume ctxt Interp_costs.stack_op) >>=? fun ctxt ->
            step ctxt b rest >>=? fun (res, ctxt) ->
            logged_return (Item (ign, res), ctxt)
        | Exec, Item (arg, Item (lam, rest)) ->
            Lwt.return (Gas.consume ctxt Interp_costs.exec) >>=? fun ctxt ->
            interp ?log ctxt ~source ~payer ~self amount lam arg >>=? fun (res, ctxt) ->
            logged_return (Item (res, rest), ctxt)
        | Lambda lam, rest ->
            Lwt.return (Gas.consume ctxt Interp_costs.push) >>=? fun ctxt ->
            logged_return (Item (lam, rest), ctxt)
        | Fail, _ ->
            fail (Reject loc)
        | Nop, stack ->
            logged_return (stack, ctxt)
        (* comparison *)
        | Compare Bool_key, Item (a, Item (b, rest)) ->
            consume_gaz_comparison descr Compare.Bool.compare Interp_costs.compare_bool a b rest
        | Compare String_key, Item (a, Item (b, rest)) ->
            consume_gaz_comparison descr Compare.String.compare Interp_costs.compare_string a b rest
        | Compare Tez_key, Item (a, Item (b, rest)) ->
            consume_gaz_comparison descr Tez.compare Interp_costs.compare_tez a b rest
        | Compare Int_key, Item (a, Item (b, rest)) ->
            consume_gaz_comparison descr Script_int.compare Interp_costs.compare_int a b rest
        | Compare Nat_key, Item (a, Item (b, rest)) ->
            consume_gaz_comparison descr Script_int.compare Interp_costs.compare_nat a b rest
        | Compare Key_hash_key, Item (a, Item (b, rest)) ->
            consume_gaz_comparison descr Signature.Public_key_hash.compare
              Interp_costs.compare_key_hash a b rest
        | Compare Timestamp_key, Item (a, Item (b, rest)) ->
            consume_gaz_comparison descr Script_timestamp.compare Interp_costs.compare_timestamp a b rest
        | Compare Address_key, Item (a, Item (b, rest)) ->
            consume_gaz_comparison descr Contract.compare Interp_costs.compare_address a b rest
        (* comparators *)
        | Eq, Item (cmpres, rest) ->
            let cmpres = Script_int.compare cmpres Script_int.zero in
            let cmpres = Compare.Int.(cmpres = 0) in
            Lwt.return (Gas.consume ctxt Interp_costs.compare_res) >>=? fun ctxt ->
            logged_return (Item (cmpres, rest), ctxt)
        | Neq, Item (cmpres, rest) ->
            let cmpres = Script_int.compare cmpres Script_int.zero in
            let cmpres = Compare.Int.(cmpres <> 0) in
            Lwt.return (Gas.consume ctxt Interp_costs.compare_res) >>=? fun ctxt ->
            logged_return (Item (cmpres, rest), ctxt)
        | Lt, Item (cmpres, rest) ->
            let cmpres = Script_int.compare cmpres Script_int.zero in
            let cmpres = Compare.Int.(cmpres < 0) in
            Lwt.return (Gas.consume ctxt Interp_costs.compare_res) >>=? fun ctxt ->
            logged_return (Item (cmpres, rest), ctxt)
        | Le, Item (cmpres, rest) ->
            let cmpres = Script_int.compare cmpres Script_int.zero in
            let cmpres = Compare.Int.(cmpres <= 0) in
            Lwt.return (Gas.consume ctxt Interp_costs.compare_res) >>=? fun ctxt ->
            logged_return (Item (cmpres, rest), ctxt)
        | Gt, Item (cmpres, rest) ->
            let cmpres = Script_int.compare cmpres Script_int.zero in
            let cmpres = Compare.Int.(cmpres > 0) in
            Lwt.return (Gas.consume ctxt Interp_costs.compare_res) >>=? fun ctxt ->
            logged_return (Item (cmpres, rest), ctxt)
        | Ge, Item (cmpres, rest) ->
            let cmpres = Script_int.compare cmpres Script_int.zero in
            let cmpres = Compare.Int.(cmpres >= 0) in
            Lwt.return (Gas.consume ctxt Interp_costs.compare_res) >>=? fun ctxt ->
            logged_return (Item (cmpres, rest), ctxt)
        (* protocol *)
        | Address, Item ((_, contract), rest) ->
            Lwt.return (Gas.consume ctxt Interp_costs.address) >>=? fun ctxt ->
            logged_return (Item (contract, rest), ctxt)
        | Contract t, Item (contract, rest) ->
            Lwt.return (Gas.consume ctxt Interp_costs.contract) >>=? fun ctxt ->
            Contract.exists ctxt contract >>=? fun exists ->
            if exists then
              Script_ir_translator.parse_contract ctxt loc t contract >>=? fun (ctxt, contract) ->
              logged_return (Item (Some contract, rest), ctxt)
            else
              logged_return (Item (None, rest), ctxt)
        | Manager, Item ((_, contract), rest) ->
            Lwt.return (Gas.consume ctxt Interp_costs.manager) >>=? fun ctxt ->
            Contract.get_manager ctxt contract >>=? fun manager ->
            logged_return (Item (manager, rest), ctxt)
        | Address_manager, Item (contract, rest) ->
            Lwt.return (Gas.consume ctxt Interp_costs.manager) >>=? fun ctxt ->
            Contract.exists ctxt contract >>=? fun exists ->
            if exists then
              Contract.get_manager ctxt contract >>=? fun manager ->
              logged_return (Item (Some manager, rest), ctxt)
            else
              logged_return (Item (None, rest), ctxt)
        | Transfer_tokens,
          Item (p, Item (amount, Item ((tp, destination), rest))) ->
            Lwt.return (Gas.consume ctxt Interp_costs.transfer) >>=? fun ctxt ->
            Lwt.return @@ unparse_data ctxt tp p >>=? fun (p, ctxt) ->
            let operation =
              Transaction
                { amount ; destination ;
                  parameters = Some (Micheline.strip_locations p) } in
            logged_return (Item ({ source = self ; operation ; signature = None }, rest), ctxt)
        | Create_account,
          Item (manager, Item (delegate, Item (delegatable, Item (credit, rest)))) ->
            Lwt.return (Gas.consume ctxt Interp_costs.create_account) >>=? fun ctxt ->
            Contract.fresh_contract_from_current_nonce ctxt >>=? fun (ctxt, contract) ->
            let operation =
              Origination
                { credit ; manager ; delegate ; preorigination = Some contract ;
                  delegatable ; script = None ; spendable = true } in
            logged_return (Item ({ source = self ; operation ; signature = None },
                                 Item (contract, rest)), ctxt)
        | Implicit_account, Item (key, rest) ->
            Lwt.return (Gas.consume ctxt Interp_costs.implicit_account) >>=? fun ctxt ->
            let contract = Contract.implicit_contract key in
            logged_return (Item ((Unit_t, contract), rest), ctxt)
        | Create_contract (storage_type, param_type),
          Item (manager, Item
                  (delegate, Item
                     (spendable, Item
                        (delegatable, Item
                           (credit, Item
                              (Lam (_, code), Item
                                 (init, rest))))))) ->
            create_contract descr ~manager ~delegate ~spendable ~delegatable ~credit ~code ~init
              ~param_type ~storage_type ~rest
        | Create_contract_literal (storage_type, param_type, Lam (_, code)),
          Item (manager, Item
                  (delegate, Item
                     (spendable, Item
                        (delegatable, Item
                           (credit, Item
                              (init, rest)))))) ->
            create_contract descr ~manager ~delegate ~spendable ~delegatable ~credit ~code ~init
              ~param_type ~storage_type ~rest
        | Set_delegate,
          Item (delegate, rest) ->
            Lwt.return (Gas.consume ctxt Interp_costs.create_account) >>=? fun ctxt ->
            let operation = Delegation delegate in
            logged_return (Item ({ source = self ; operation ; signature = None }, rest), ctxt)
        | Balance, rest ->
            Lwt.return (Gas.consume ctxt Interp_costs.balance) >>=? fun ctxt ->
            Contract.get_balance ctxt self >>=? fun balance ->
            logged_return (Item (balance, rest), ctxt)
        | Now, rest ->
            Lwt.return (Gas.consume ctxt Interp_costs.now) >>=? fun ctxt ->
            let now = Script_timestamp.now ctxt in
            logged_return (Item (now, rest), ctxt)
        | Check_signature, Item (key, Item ((signature, message), rest)) ->
            Lwt.return (Gas.consume ctxt Interp_costs.check_signature) >>=? fun ctxt ->
            let message = MBytes.of_string message in
            let res = Signature.check key signature message in
            logged_return (Item (res, rest), ctxt)
        | Hash_key, Item (key, rest) ->
            Lwt.return (Gas.consume ctxt Interp_costs.hash_key) >>=? fun ctxt ->
            logged_return (Item (Signature.Public_key.hash key, rest), ctxt)
        | H ty, Item (v, rest) ->
            Lwt.return (Gas.consume ctxt (Interp_costs.hash v)) >>=? fun ctxt ->
            Lwt.return @@ hash_data ctxt ty v >>=? fun (hash, ctxt) ->
            logged_return (Item (hash, rest), ctxt)
        | Steps_to_quota, rest ->
            Lwt.return (Gas.consume ctxt Interp_costs.steps_to_quota) >>=? fun ctxt ->
            let steps = match Gas.level ctxt with
              | Limited { remaining } -> remaining
              | Unaccounted -> Z.of_string "99999999" in
            logged_return (Item (Script_int.(abs (of_zint steps)), rest), ctxt)
        | Source, rest ->
            Lwt.return (Gas.consume ctxt Interp_costs.source) >>=? fun ctxt ->
            logged_return (Item (source, rest), ctxt)
        | Self t, rest ->
            Lwt.return (Gas.consume ctxt Interp_costs.self) >>=? fun ctxt ->
            logged_return (Item ((t,self), rest), ctxt)
        | Amount, rest ->
            Lwt.return (Gas.consume ctxt Interp_costs.amount) >>=? fun ctxt ->
            logged_return (Item (amount, rest), ctxt) in
    let stack = (Item (arg, Empty)) in
    begin match log with
      | None -> ()
      | Some log ->
          log := (code.loc, Gas.level ctxt, unparse_stack ctxt (stack, code.bef)) :: !log
    end ;
    step ctxt code stack >>=? fun (Item (ret, Empty), ctxt) ->
    return (ret, ctxt)

(* ---- contract handling ---------------------------------------------------*)

and execute ?log ctxt ~check_operations ~source ~payer ~self script amount arg :
  (Script.expr * internal_operation list * context *
   Script_typed_ir.ex_big_map option) tzresult Lwt.t =
  parse_script ctxt ~check_operations script
  >>=? fun ((Ex_script { code ; arg_type ; storage ; storage_type }), ctxt) ->
  parse_data ctxt ~check_operations arg_type arg >>=? fun (arg, ctxt) ->
  trace
    (Runtime_contract_error (self, script.code))
    (interp ?log ctxt ~source ~payer ~self amount code (arg, storage))
  >>=? fun ((ops, sto), ctxt) ->
  Lwt.return @@ unparse_data ctxt storage_type sto >>=? fun (storage, ctxt) ->
  return (Micheline.strip_locations storage, ops, ctxt,
          Script_ir_translator.extract_big_map storage_type sto)

type execution_result =
  { ctxt : context ;
    storage : Script.expr ;
    big_map_diff : Contract.big_map_diff option ;
    operations : internal_operation list }

let trace ctxt ~check_operations ~source ~payer ~self:(self, script) ~parameter ~amount =
  let log = ref [] in
  execute ~log ctxt ~check_operations ~source ~payer ~self script amount (Micheline.root parameter)
  >>=? fun (storage, operations, ctxt, big_map_diff) ->
  begin match big_map_diff with
    | None -> return (None, ctxt)
    | Some big_map_diff ->
        Script_ir_translator.to_serializable_big_map ctxt big_map_diff >>=? fun (big_map_diff, ctxt) ->
        return (Some big_map_diff, ctxt)
  end >>=? fun (big_map_diff, ctxt) ->
  let trace = List.rev !log in
  return ({ ctxt ; storage ; big_map_diff ; operations }, trace)

let execute ctxt ~check_operations ~source ~payer ~self:(self, script) ~parameter ~amount =
  execute ctxt ~check_operations ~source ~payer ~self script amount (Micheline.root parameter)
  >>=? fun (storage, operations, ctxt, big_map_diff) ->
  begin match big_map_diff with
    | None -> return (None, ctxt)
    | Some big_map_diff ->
        Script_ir_translator.to_serializable_big_map ctxt big_map_diff >>=? fun (big_map_diff, ctxt) ->
        return (Some big_map_diff, ctxt)
  end >>=? fun (big_map_diff, ctxt) ->
  return { ctxt ; storage ; big_map_diff ; operations }
