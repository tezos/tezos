(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Tezos_context
open Script_int
open Script
open Script_typed_ir
open Script_ir_translator

(* ---- Run-time errors -----------------------------------------------------*)

type error += Quota_exceeded
type error += Overflow of Script.location
type error += Reject of Script.location
type error += Division_by_zero of Script.location

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
  register_error_kind
    `Permanent
    ~id:"overflowRuntimeError"
    ~title: "Value overflow (runtime script error)"
    ~description:
      "An integer or currency overflow happened \
       during the execution of a script"
    (obj1 (req "location" Script.location_encoding))
    (function Overflow loc -> Some loc | _ -> None)
    (fun loc -> Overflow loc) ;
  register_error_kind
    `Permanent
    ~id:"divisionByZeroRuntimeError"
    ~title: "Division by zero (runtime script error)"
    ~description: ""
    (obj1 (req "location" Script.location_encoding))
    (function Division_by_zero loc -> Some loc | _ -> None)
    (fun loc -> Division_by_zero loc) ;
  register_error_kind
    `Temporary
    ~id:"scriptRejectedRuntimeError"
    ~title: "Script rejected (runtime script error)"
    ~description: ""
    (obj1 (req "location" Script.location_encoding))
    (function Reject loc -> Some loc | _ -> None)
    (fun loc -> Reject loc)

(* ---- interpreter ---------------------------------------------------------*)

type 'tys stack =
  | Item : 'ty * 'rest stack -> ('ty * 'rest) stack
  | Empty : end_of_stack stack

let rec interp
  : type p r.
    int -> Contract.t -> Contract.t -> Tez.t ->
    context -> (p, r) lambda -> p -> (r * int * context) tzresult Lwt.t
  = fun qta orig source amount ctxt (Lam (code, _)) arg ->
    let rec step
      : type b a.
        int -> context -> (b, a) instr -> b stack ->
        (a stack * int * context) tzresult Lwt.t =
      fun qta ctxt instr stack ->
        if Compare.Int.(qta <= 0) then
          fail Quota_exceeded
        else match instr, stack with
          (* stack ops *)
          | Drop, Item (_, rest) ->
              return (rest, qta - 1, ctxt)
          | Dup, Item (v, rest) ->
              return (Item (v, Item (v, rest)), qta - 1, ctxt)
          | Swap, Item (vi, Item (vo, rest)) ->
              return (Item (vo, Item (vi, rest)), qta - 1, ctxt)
          | Const v, rest ->
              return (Item (v, rest), qta - 1, ctxt)
          (* options *)
          | Cons_some, Item (v, rest) ->
              return (Item (Some v, rest), qta - 1, ctxt)
          | Cons_none _, rest ->
              return (Item (None, rest), qta - 1, ctxt)
          | If_none (bt, _), Item (None, rest) ->
              step qta ctxt bt rest
          | If_none (_, bf), Item (Some v, rest) ->
              step qta ctxt bf (Item (v, rest))
          (* pairs *)
          | Cons_pair, Item (a, Item (b, rest)) ->
              return (Item ((a, b), rest), qta - 1, ctxt)
          | Car, Item ((a, _), rest) ->
              return (Item (a, rest), qta - 1, ctxt)
          | Cdr, Item ((_, b), rest) ->
              return (Item (b, rest), qta - 1, ctxt)
          (* unions *)
          | Left, Item (v, rest) ->
              return (Item (L v, rest), qta - 1, ctxt)
          | Right, Item (v, rest) ->
              return (Item (R v, rest), qta - 1, ctxt)
          | If_left (bt, _), Item (L v, rest) ->
              step qta ctxt bt (Item (v, rest))
          | If_left (_, bf), Item (R v, rest) ->
              step qta ctxt bf (Item (v, rest))
          (* lists *)
          | Cons_list, Item (hd, Item (tl, rest)) ->
              return (Item (hd :: tl, rest), qta - 1, ctxt)
          | Nil, rest ->
              return (Item ([], rest), qta - 1, ctxt)
          | If_cons (_, bf), Item ([], rest) ->
              step qta ctxt bf rest
          | If_cons (bt, _), Item (hd :: tl, rest) ->
              step qta ctxt bt (Item (hd, Item (tl, rest)))
          | List_map, Item (lam, Item (l, rest)) ->
              fold_left_s (fun (tail, qta, ctxt) arg ->
                  interp qta orig source amount ctxt lam arg
                  >>=? fun (ret, qta, ctxt) ->
                  return (ret :: tail, qta, ctxt))
                ([], qta, ctxt) l >>=? fun (res, qta, ctxt) ->
              return (Item (res, rest), qta, ctxt)
          | List_reduce, Item (lam, Item (l, Item (init, rest))) ->
              fold_left_s
                (fun (partial, qta, ctxt) arg ->
                   interp qta orig source amount ctxt lam (arg, partial)
                   >>=? fun (partial, qta, ctxt) ->
                   return (partial, qta, ctxt))
                (init, qta, ctxt) l >>=? fun (res, qta, ctxt) ->
              return (Item (res, rest), qta, ctxt)
          (* sets *)
          | Empty_set t, rest ->
              return (Item (empty_set t, rest), qta - 1, ctxt)
          | Set_map t, Item (lam, Item (set, rest)) ->
              let items =
                List.rev (set_fold (fun e acc -> e :: acc) set []) in
              fold_left_s
                (fun (res, qta, ctxt) arg ->
                   interp qta orig source amount ctxt lam arg >>=?
                   fun (ret, qta, ctxt) ->
                   return (set_update ret true res, qta, ctxt))
                (empty_set t, qta, ctxt) items >>=? fun (res, qta, ctxt) ->
              return (Item (res, rest), qta, ctxt)
          | Set_reduce, Item (lam, Item (set, Item (init, rest))) ->
              let items =
                List.rev (set_fold (fun e acc -> e :: acc) set []) in
              fold_left_s
                (fun (partial, qta, ctxt) arg ->
                   interp qta orig source amount ctxt lam (arg, partial)
                   >>=? fun (partial, qta, ctxt) ->
                   return (partial, qta, ctxt))
                (init, qta, ctxt) items >>=? fun (res, qta, ctxt) ->
              return (Item (res, rest), qta, ctxt)
          | Set_mem, Item (v, Item (set, rest)) ->
              return (Item (set_mem v set, rest), qta - 1, ctxt)
          | Set_update, Item (v, Item (presence, Item (set, rest))) ->
              return (Item (set_update v presence set, rest), qta - 1, ctxt)
          (* maps *)
          | Empty_map (t, _), rest ->
              return (Item (empty_map t, rest), qta - 1, ctxt)
          | Map_map, Item (lam, Item (map, rest)) ->
              let items =
                List.rev (map_fold (fun k v acc -> (k, v) :: acc) map []) in
              fold_left_s
                (fun (acc, qta, ctxt) (k, v) ->
                   interp qta orig source amount ctxt lam (k, v)
                   >>=? fun (ret, qta, ctxt) ->
                   return (map_update k (Some ret) acc, qta, ctxt))
                (empty_map (map_key_ty map), qta, ctxt) items >>=? fun (res, qta, ctxt) ->
              return (Item (res, rest), qta, ctxt)
          | Map_reduce, Item (lam, Item (map, Item (init, rest))) ->
              let items =
                List.rev (map_fold (fun k v acc -> (k, v) :: acc) map []) in
              fold_left_s
                (fun (partial, qta, ctxt) arg ->
                   interp qta orig source amount ctxt lam (arg, partial)
                   >>=? fun (partial, qta, ctxt) ->
                   return (partial, qta, ctxt))
                (init, qta, ctxt) items >>=? fun (res, qta, ctxt) ->
              return (Item (res, rest), qta, ctxt)
          | Map_mem, Item (v, Item (map, rest)) ->
              return (Item (map_mem v map, rest), qta - 1, ctxt)
          | Map_get, Item (v, Item (map, rest)) ->
              return (Item (map_get v map, rest), qta - 1, ctxt)
          | Map_update, Item (k, Item (v, Item (map, rest))) ->
              return (Item (map_update k v map, rest), qta - 1, ctxt)
          (* timestamp operations *)
          | Add_seconds_to_timestamp (kind, _pos), Item (n, Item (t, rest)) ->
              let n = Script_int.to_int64 kind n in
              Lwt.return
                (Period.of_seconds n >>? fun p ->
                 Timestamp.(t +? p) >>? fun res ->
                 Ok (Item (res, rest), qta - 1, ctxt))
          | Add_timestamp_to_seconds (kind, _pos), Item (t, Item (n, rest)) ->
              let n = Script_int.to_int64 kind n in
              Lwt.return
                (Period.of_seconds n >>? fun p ->
                 Timestamp.(t +? p) >>? fun res ->
                 Ok (Item (res, rest), qta - 1, ctxt))
          (* string operations *)
          | Concat, Item (x, Item (y, rest)) ->
              return (Item (x ^ y, rest), qta - 1, ctxt)
          (* currency operations *)
          | Add_tez, Item (x, Item (y, rest)) ->
              Lwt.return Tez.(x +? y) >>=? fun res ->
              return (Item (res, rest), qta - 1, ctxt)
          | Sub_tez, Item (x, Item (y, rest)) ->
              Lwt.return Tez.(x -? y) >>=? fun res ->
              return (Item (res, rest), qta - 1, ctxt)
          | Mul_tez kind, Item (x, Item (y, rest)) ->
              Lwt.return Tez.(x *? Script_int.to_int64 kind y) >>=? fun res ->
              return (Item (res, rest), qta - 1, ctxt)
          | Mul_tez' kind, Item (y, Item (x, rest)) ->
              Lwt.return Tez.(x *? Script_int.to_int64 kind y) >>=? fun res ->
              return (Item (res, rest), qta - 1, ctxt)
          (* boolean operations *)
          | Or, Item (x, Item (y, rest)) ->
              return (Item (x || y, rest), qta - 1, ctxt)
          | And, Item (x, Item (y, rest)) ->
              return (Item (x && y, rest), qta - 1, ctxt)
          | Xor, Item (x, Item (y, rest)) ->
              return (Item (not x && y || x && not y, rest), qta - 1, ctxt)
          | Not, Item (x, rest) ->
              return (Item (not x, rest), qta - 1, ctxt)
          (* integer operations *)
          | Checked_abs_int (kind, pos), Item (x, rest) ->
              begin match Script_int.checked_abs kind x with
                | None -> fail (Overflow pos)
                | Some res -> return (Item (res, rest), qta - 1, ctxt)
              end
          | Checked_neg_int (kind, pos), Item (x, rest) ->
              begin match Script_int.checked_neg kind x with
                | None -> fail (Overflow pos)
                | Some res -> return (Item (res, rest), qta - 1, ctxt)
              end
          | Checked_add_int (kind, pos), Item (x, Item (y, rest)) ->
              begin match Script_int.checked_add kind x y with
                | None -> fail (Overflow pos)
                | Some res -> return (Item (res, rest), qta - 1, ctxt)
              end
          | Checked_sub_int (kind, pos), Item (x, Item (y, rest)) ->
              begin match Script_int.checked_sub kind x y with
                | None -> fail (Overflow pos)
                | Some res -> return (Item (res, rest), qta - 1, ctxt)
              end
          | Checked_mul_int (kind, pos), Item (x, Item (y, rest)) ->
              begin match Script_int.checked_mul kind x y with
                | None -> fail (Overflow pos)
                | Some res -> return (Item (res, rest), qta - 1, ctxt)
              end
          | Abs_int kind, Item (x, rest) ->
              return (Item (Script_int.abs kind x, rest), qta - 1, ctxt)
          | Neg_int kind, Item (x, rest) ->
              return (Item (Script_int.neg kind x, rest), qta - 1, ctxt)
          | Add_int kind, Item (x, Item (y, rest)) ->
              return (Item (Script_int.add kind x y, rest), qta - 1, ctxt)
          | Sub_int kind, Item (x, Item (y, rest)) ->
              return (Item (Script_int.sub kind x y, rest), qta - 1, ctxt)
          | Mul_int kind, Item (x, Item (y, rest)) ->
              return (Item (Script_int.mul kind x y, rest), qta - 1, ctxt)
          | Div_int (kind, pos), Item (x, Item (y, rest)) ->
              if Compare.Int64.(Script_int.to_int64 kind y = 0L) then
                fail (Division_by_zero pos)
              else
                return (Item (Script_int.div kind x y, rest), qta - 1, ctxt)
          | Mod_int (kind, pos), Item (x, Item (y, rest)) ->
              if Compare.Int64.(Script_int.to_int64 kind y = 0L) then
                fail (Division_by_zero pos)
              else
                return (Item (Script_int.rem kind x y, rest), qta - 1, ctxt)
          | Lsl_int kind, Item (x, Item (y, rest)) ->
              return (Item (Script_int.logsl kind x y, rest), qta - 1, ctxt)
          | Lsr_int kind, Item (x, Item (y, rest)) ->
              return (Item (Script_int.logsr kind x y, rest), qta - 1, ctxt)
          | Or_int kind, Item (x, Item (y, rest)) ->
              return (Item (Script_int.logor kind x y, rest), qta - 1, ctxt)
          | And_int kind, Item (x, Item (y, rest)) ->
              return (Item (Script_int.logand kind x y, rest), qta - 1, ctxt)
          | Xor_int kind, Item (x, Item (y, rest)) ->
              return (Item (Script_int.logxor kind x y, rest), qta - 1, ctxt)
          | Not_int kind, Item (x, rest) ->
              return (Item (Script_int.lognot kind x, rest), qta - 1, ctxt)
          (* control *)
          | Seq (hd, tl), stack ->
              step qta ctxt hd stack >>=? fun (trans, qta, ctxt) ->
              step qta ctxt tl trans
          | If (bt, _), Item (true, rest) ->
              step qta ctxt bt rest
          | If (_, bf), Item (false, rest) ->
              step qta ctxt bf rest
          | Loop body, Item (true, rest) ->
              step qta ctxt body rest >>=? fun (trans, qta, ctxt) ->
              step (qta - 1) ctxt (Loop body) trans
          | Loop _, Item (false, rest) ->
              return (rest, qta, ctxt)
          | Dip b, Item (ign, rest) ->
              step qta ctxt b rest >>=? fun (res, qta, ctxt) ->
              return (Item (ign, res), qta, ctxt)
          | Exec, Item (arg, Item (lam, rest)) ->
              interp qta orig source amount ctxt lam arg >>=? fun (res, qta, ctxt) ->
              return (Item (res, rest), qta - 1, ctxt)
          | Lambda lam, rest ->
              return (Item (lam, rest), qta - 1, ctxt)
          | Fail pos, _ ->
              fail (Reject pos)
          | Nop, stack ->
              return (stack, qta - 1, ctxt)
          (* comparison *)
          | Compare Bool_key, Item (a, Item (b, rest)) ->
              let cmpres = Compare.Bool.compare a b in
              let cmpres = Script_int.of_int64 Int64 (Int64.of_int cmpres) in
              return (Item (cmpres, rest), qta - 1, ctxt)
          | Compare String_key, Item (a, Item (b, rest)) ->
              let cmpres = Compare.String.compare a b in
              let cmpres = Script_int.of_int64 Int64 (Int64.of_int cmpres) in
              return (Item (cmpres, rest), qta - 1, ctxt)
          | Compare Tez_key, Item (a, Item (b, rest)) ->
              let cmpres = Tez.compare a b in
              let cmpres = Script_int.of_int64 Int64 (Int64.of_int cmpres) in
              return (Item (cmpres, rest), qta - 1, ctxt)
          | Compare (Int_key kind), Item (a, Item (b, rest)) ->
              let cmpres = Script_int.compare kind a b in
              return (Item (cmpres, rest), qta - 1, ctxt)
          | Compare Key_key, Item (a, Item (b, rest)) ->
              let cmpres = Ed25519.Public_key_hash.compare a b in
              let cmpres = Script_int.of_int64 Int64 (Int64.of_int cmpres) in
              return (Item (cmpres, rest), qta - 1, ctxt)
          | Compare Timestamp_key, Item (a, Item (b, rest)) ->
              let cmpres = Timestamp.compare a b in
              let cmpres = Script_int.of_int64 Int64 (Int64.of_int cmpres) in
              return (Item (cmpres, rest), qta - 1, ctxt)
          (* comparators *)
          | Eq, Item (cmpres, rest) ->
              let cmpres = Script_int.to_int64 Int64 cmpres in
              let cmpres = Compare.Int64.(cmpres = 0L) in
              return (Item (cmpres, rest), qta - 1, ctxt)
          | Neq, Item (cmpres, rest) ->
              let cmpres = Script_int.to_int64 Int64 cmpres in
              let cmpres = Compare.Int64.(cmpres <> 0L) in
              return (Item (cmpres, rest), qta - 1, ctxt)
          | Lt, Item (cmpres, rest) ->
              let cmpres = Script_int.to_int64 Int64 cmpres in
              let cmpres = Compare.Int64.(cmpres < 0L) in
              return (Item (cmpres, rest), qta - 1, ctxt)
          | Gt, Item (cmpres, rest) ->
              let cmpres = Script_int.to_int64 Int64 cmpres in
              let cmpres = Compare.Int64.(cmpres > 0L) in
              return (Item (cmpres, rest), qta - 1, ctxt)
          | Le, Item (cmpres, rest) ->
              let cmpres = Script_int.to_int64 Int64 cmpres in
              let cmpres = Compare.Int64.(cmpres <= 0L) in
              return (Item (cmpres, rest), qta - 1, ctxt)
          | Ge, Item (cmpres, rest) ->
              let cmpres = Script_int.to_int64 Int64 cmpres in
              let cmpres = Compare.Int64.(cmpres >= 0L) in
              return (Item (cmpres, rest), qta - 1, ctxt)
          (* casts *)
          | Checked_int_of_int (_, kt, pos), Item (v, rest) ->
              begin match Script_int.checked_cast kt v with
                | None -> fail (Overflow pos)
                | Some res -> return (Item (res, rest), qta - 1, ctxt)
              end
          | Int_of_int (_, kt), Item (v, rest) ->
              return (Item (Script_int.cast kt v, rest), qta - 1, ctxt)
          (* protocol *)
          | Manager, Item ((_, _, contract), rest) ->
              Contract.get_manager ctxt contract >>=? fun manager ->
              return (Item (manager, rest), qta - 1, ctxt)
          | Transfer_tokens (storage_type, loc),
            Item (p, Item (amount, Item ((tp, Void_t, destination), Item (sto, Empty)))) -> begin
              Contract.unconditional_spend ctxt source amount >>=? fun ctxt ->
              Contract.credit ctxt destination amount >>=? fun ctxt ->
              Contract.get_script ctxt destination >>=? fun destination_script ->
              let sto = unparse_untagged_data storage_type sto in
              Contract.update_script_storage ctxt source sto >>=? fun ctxt ->
              begin match destination_script with
                | No_script ->
                    (* we see non scripted contracts as (void, void) contract *)
                    Lwt.return (ty_eq tp Void_t |>
                                record_trace (Invalid_contract (loc, destination))) >>=? fun (Eq _) ->
                    return (ctxt, qta)
                | Script { code ; storage } ->
                    let p = unparse_untagged_data tp p in
                    execute source destination ctxt storage code amount p qta
                    >>=? fun (csto, ret, qta, ctxt) ->
                    Contract.update_script_storage
                      ctxt destination csto >>=? fun ctxt ->
                    trace
                      (Invalid_contract (loc, destination))
                      (parse_untagged_data ctxt Void_t ret) >>=? fun () ->
                    return (ctxt, qta)
              end >>=? fun (ctxt, qta) ->
              Contract.get_script ctxt source >>=? (function
                  | No_script -> assert false
                  | Script { storage = { storage } } ->
                      parse_untagged_data ctxt storage_type storage >>=? fun sto ->
                      return (Item ((), Item (sto, Empty)), qta - 1, ctxt))
              end
            | Transfer_tokens (storage_type, loc),
              Item (p, Item (amount, Item ((tp, tr, destination), Item (sto, Empty)))) -> begin
                Contract.unconditional_spend ctxt source amount >>=? fun ctxt ->
                Contract.credit ctxt destination amount >>=? fun ctxt ->
                Contract.get_script ctxt destination >>=? function
                | No_script -> fail (Invalid_contract (loc, destination))
                | Script { code ; storage } ->
                    let sto = unparse_untagged_data storage_type sto in
                    Contract.update_script_storage ctxt source sto >>=? fun ctxt ->
                    let p = unparse_untagged_data tp p in
                    execute source destination ctxt storage code amount p qta
                    >>=? fun (sto, ret, qta, ctxt) ->
                    Contract.update_script_storage
                      ctxt destination sto >>=? fun ctxt ->
                    trace
                      (Invalid_contract (loc, destination))
                      (parse_untagged_data ctxt tr ret) >>=? fun v ->
                    Contract.get_script ctxt source >>=? (function
                        | No_script -> assert false
                        | Script { storage = { storage } } ->
                            parse_untagged_data ctxt storage_type storage >>=? fun sto ->
                            return (Item (v, Item (sto, Empty)), qta - 1, ctxt))
              end
            | Create_account,
              Item (manager, Item (delegate, Item (delegatable, Item (credit, rest)))) ->
              Contract.unconditional_spend ctxt source credit >>=? fun ctxt ->
              Lwt.return Tez.(credit -? Constants.origination_burn) >>=? fun balance ->
              Contract.originate ctxt
                ~manager ~delegate ~balance
                ~script:No_script ~spendable:true ~delegatable >>=? fun (ctxt, contract) ->
              return (Item ((Void_t, Void_t, contract), rest), qta - 1, ctxt)
          | Create_contract (g, p, r),
            Item (manager, Item (delegate, Item (delegatable, Item (credit,
                                                                    Item (Lam (_, code), Item (init, rest)))))) ->
              let code, storage =
                { code; arg_type = unparse_ty p; ret_type = unparse_ty r; storage_type =  unparse_ty g },
                { storage = unparse_untagged_data g init; storage_type =  unparse_ty g } in
              let storage_fee = Script.storage_cost storage in
              let code_fee = Script.code_cost code in
              Lwt.return Tez.(code_fee +? storage_fee) >>=? fun script_fee ->
              Lwt.return Tez.(script_fee +?
                              Constants.origination_burn) >>=? fun total_fee ->
              fail_unless Tez.(credit > total_fee)
                Contract.Initial_amount_too_low >>=? fun () ->
              Contract.unconditional_spend ctxt source credit >>=? fun ctxt ->
              Lwt.return Tez.(credit -? Constants.origination_burn) >>=? fun balance ->
              Contract.originate ctxt
                ~manager ~delegate ~balance
                ~script:(Script { code ; storage }) ~spendable:true ~delegatable
              >>=? fun (ctxt, contract) ->
              return (Item ((p, r, contract), rest), qta - 1, ctxt)
          | Balance, rest ->
              Contract.get_balance ctxt source >>=? fun balance ->
              return (Item (balance, rest), qta - 1, ctxt)
          | Now, rest ->
              Timestamp.get_current ctxt >>=? fun now ->
              return (Item (now, rest), qta - 1, ctxt)
          | Check_signature, Item (key, Item ((signature, message), rest)) ->
              Public_key.get ctxt key >>=? fun key ->
              let message = MBytes.of_string message in
              let res = Ed25519.check_signature key signature message in
              return (Item (res, rest), qta - 1, ctxt)
          | H ty, Item (v, rest) ->
              let hash = Script.hash_expr (unparse_untagged_data ty v) in
              return (Item (hash, rest), qta - 1, ctxt)
          | Steps_to_quota, rest ->
              let steps = Script_int.of_int64 Uint32 (Int64.of_int qta) in
              return (Item (steps, rest), qta - 1, ctxt)
          | Source (ta, tb), rest ->
              return (Item ((ta, tb, orig), rest), qta - 1, ctxt)
          | Amount, rest ->
              return (Item (amount, rest), qta - 1, ctxt)
    in
    step qta ctxt code (Item (arg, Empty)) >>=? fun (Item (ret, Empty), qta, ctxt) ->
    return (ret, qta, ctxt)

(* ---- contract handling ---------------------------------------------------*)

and execute orig source ctxt { storage; storage_type } { code; arg_type; ret_type } amount arg qta =
  parse_ty arg_type >>=? fun (Ex arg_type) ->
  parse_ty ret_type >>=? fun (Ex ret_type) ->
  parse_ty storage_type >>=? fun (Ex storage_type) ->
  let arg_type_full = Pair_t (Pair_t (Tez_t, arg_type), storage_type) in
  let ret_type_full = Pair_t (ret_type, storage_type) in
  parse_lambda ctxt arg_type_full ret_type_full code >>=? fun lambda ->
  parse_untagged_data ctxt arg_type arg >>=? fun arg ->
  parse_untagged_data ctxt storage_type storage >>=? fun storage ->
  interp qta orig source amount ctxt lambda ((amount, arg), storage) >>=? fun (ret, qta, ctxt) ->
              let ret, storage = ret in
              return (unparse_untagged_data storage_type storage,
                      unparse_untagged_data ret_type ret,
                      qta, ctxt)
