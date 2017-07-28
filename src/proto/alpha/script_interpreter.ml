(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Tezos_context
open Script
open Script_typed_ir
open Script_ir_translator

let dummy_code_fee = Tez.fifty_cents
let dummy_storage_fee = Tez.fifty_cents

(* ---- Run-time errors -----------------------------------------------------*)

type error += Quota_exceeded
type error += Overflow of Script.location
type error += Reject of Script.location
type error += Runtime_contract_error : Contract.t * Script.expr * _ ty * _ ty * _ ty -> error

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
    `Temporary
    ~id:"scriptRejectedRuntimeError"
    ~title: "Script failed (runtime script error)"
    ~description: "A FAIL instruction was reached"
    (obj1 (req "location" Script.location_encoding))
    (function Reject loc -> Some loc | _ -> None)
    (fun loc -> Reject loc);
  register_error_kind
    `Temporary
    ~id:"scriptRuntimeError"
    ~title: "Script runtime error"
    ~description: "Toplevel error for all runtime script errors"
    (obj5
       (req "contractHandle" Contract.encoding)
       (req "contractCode" Script.expr_encoding)
       (req "contractParameterType" ex_ty_enc)
       (req "contractReturnType" ex_ty_enc)
       (req "contractStorageType" ex_ty_enc))
    (function
      | Runtime_contract_error (contract, expr, arg_ty, ret_ty, storage_ty) ->
          Some (contract, expr, Ex_ty arg_ty, Ex_ty ret_ty, Ex_ty storage_ty)
      | _ -> None)
    (fun (contract, expr, Ex_ty arg_ty, Ex_ty ret_ty, Ex_ty storage_ty) ->
       Runtime_contract_error (contract, expr, arg_ty, ret_ty, storage_ty));

(* ---- interpreter ---------------------------------------------------------*)

type 'tys stack =
  | Item : 'ty * 'rest stack -> ('ty * 'rest) stack
  | Empty : end_of_stack stack

let rec unparse_stack
  : type a. a stack * a stack_ty -> Script.expr list
  = function
    | Empty, Empty_t -> []
    | Item (v, rest), Item_t (ty, rest_ty) ->
        unparse_data ty v :: unparse_stack (rest, rest_ty)

let rec interp
  : type p r.
    ?log: (Script.location * int * Script.expr list) list ref ->
    Contract.origination_nonce -> int -> Contract.t -> Contract.t -> Tez.t ->
    context -> (p, r) lambda -> p ->
    (r * int * context * Contract.origination_nonce) tzresult Lwt.t
  = fun ?log origination qta orig source amount ctxt (Lam (code, _)) arg ->
    let rec step
      : type b a.
        Contract.origination_nonce -> int -> context -> (b, a) descr -> b stack ->
        (a stack * int * context * Contract.origination_nonce) tzresult Lwt.t =
      fun origination qta ctxt ({ instr ; loc } as descr) stack ->
        if Compare.Int.(qta <= 0) then
          fail Quota_exceeded
        else
          let logged_return ?(origination = origination) (ret, qta, ctxt) =
            match log with
            | None -> return (ret, qta, ctxt, origination)
            | Some log ->
                log := (descr.loc, qta, unparse_stack (ret, descr.aft)) :: !log ;
                return (ret, qta, ctxt, origination) in
          match instr, stack with
          (* stack ops *)
          | Drop, Item (_, rest) ->
              logged_return (rest, qta - 1, ctxt)
          | Dup, Item (v, rest) ->
              logged_return (Item (v, Item (v, rest)), qta - 1, ctxt)
          | Swap, Item (vi, Item (vo, rest)) ->
              logged_return (Item (vo, Item (vi, rest)), qta - 1, ctxt)
          | Const v, rest ->
              logged_return (Item (v, rest), qta - 1, ctxt)
          (* options *)
          | Cons_some, Item (v, rest) ->
              logged_return (Item (Some v, rest), qta - 1, ctxt)
          | Cons_none _, rest ->
              logged_return (Item (None, rest), qta - 1, ctxt)
          | If_none (bt, _), Item (None, rest) ->
              step origination qta ctxt bt rest
          | If_none (_, bf), Item (Some v, rest) ->
              step origination qta ctxt bf (Item (v, rest))
          (* pairs *)
          | Cons_pair, Item (a, Item (b, rest)) ->
              logged_return (Item ((a, b), rest), qta - 1, ctxt)
          | Car, Item ((a, _), rest) ->
              logged_return (Item (a, rest), qta - 1, ctxt)
          | Cdr, Item ((_, b), rest) ->
              logged_return (Item (b, rest), qta - 1, ctxt)
          (* unions *)
          | Left, Item (v, rest) ->
              logged_return (Item (L v, rest), qta - 1, ctxt)
          | Right, Item (v, rest) ->
              logged_return (Item (R v, rest), qta - 1, ctxt)
          | If_left (bt, _), Item (L v, rest) ->
              step origination qta ctxt bt (Item (v, rest))
          | If_left (_, bf), Item (R v, rest) ->
              step origination qta ctxt bf (Item (v, rest))
          (* lists *)
          | Cons_list, Item (hd, Item (tl, rest)) ->
              logged_return (Item (hd :: tl, rest), qta - 1, ctxt)
          | Nil, rest ->
              logged_return (Item ([], rest), qta - 1, ctxt)
          | If_cons (_, bf), Item ([], rest) ->
              step origination qta ctxt bf rest
          | If_cons (bt, _), Item (hd :: tl, rest) ->
              step origination qta ctxt bt (Item (hd, Item (tl, rest)))
          | List_map, Item (lam, Item (l, rest)) ->
              fold_left_s (fun (tail, qta, ctxt, origination) arg ->
                  interp ?log origination qta orig source amount ctxt lam arg
                  >>=? fun (ret, qta, ctxt, origination) ->
                  return (ret :: tail, qta, ctxt, origination))
                ([], qta, ctxt, origination) l >>=? fun (res, qta, ctxt, origination) ->
              logged_return ~origination (Item (res, rest), qta, ctxt)
          | List_reduce, Item (lam, Item (l, Item (init, rest))) ->
              fold_left_s
                (fun (partial, qta, ctxt, origination) arg ->
                   interp ?log origination qta orig source amount ctxt lam (arg, partial)
                   >>=? fun (partial, qta, ctxt, origination) ->
                   return (partial, qta, ctxt, origination))
                (init, qta, ctxt, origination) l >>=? fun (res, qta, ctxt, origination) ->
              logged_return ~origination (Item (res, rest), qta, ctxt)
          (* sets *)
          | Empty_set t, rest ->
              logged_return (Item (empty_set t, rest), qta - 1, ctxt)
          | Set_map t, Item (lam, Item (set, rest)) ->
              let items =
                List.rev (set_fold (fun e acc -> e :: acc) set []) in
              fold_left_s
                (fun (res, qta, ctxt, origination) arg ->
                   interp ?log origination qta orig source amount ctxt lam arg >>=?
                   fun (ret, qta, ctxt, origination) ->
                   return (set_update ret true res, qta, ctxt, origination))
                (empty_set t, qta, ctxt, origination) items >>=? fun (res, qta, ctxt, origination) ->
              logged_return ~origination (Item (res, rest), qta, ctxt)
          | Set_reduce, Item (lam, Item (set, Item (init, rest))) ->
              let items =
                List.rev (set_fold (fun e acc -> e :: acc) set []) in
              fold_left_s
                (fun (partial, qta, ctxt, origination) arg ->
                   interp ?log origination qta orig source amount ctxt lam (arg, partial)
                   >>=? fun (partial, qta, ctxt, origination) ->
                   return (partial, qta, ctxt, origination))
                (init, qta, ctxt, origination) items >>=? fun (res, qta, ctxt, origination) ->
              logged_return ~origination (Item (res, rest), qta, ctxt)
          | Set_mem, Item (v, Item (set, rest)) ->
              logged_return (Item (set_mem v set, rest), qta - 1, ctxt)
          | Set_update, Item (v, Item (presence, Item (set, rest))) ->
              logged_return (Item (set_update v presence set, rest), qta - 1, ctxt)
          (* maps *)
          | Empty_map (t, _), rest ->
              logged_return (Item (empty_map t, rest), qta - 1, ctxt)
          | Map_map, Item (lam, Item (map, rest)) ->
              let items =
                List.rev (map_fold (fun k v acc -> (k, v) :: acc) map []) in
              fold_left_s
                (fun (acc, qta, ctxt, origination) (k, v) ->
                   interp ?log origination qta orig source amount ctxt lam (k, v)
                   >>=? fun (ret, qta, ctxt, origination) ->
                   return (map_update k (Some ret) acc, qta, ctxt, origination))
                (empty_map (map_key_ty map), qta, ctxt, origination) items >>=? fun (res, qta, ctxt, origination) ->
              logged_return ~origination (Item (res, rest), qta, ctxt)
          | Map_reduce, Item (lam, Item (map, Item (init, rest))) ->
              let items =
                List.rev (map_fold (fun k v acc -> (k, v) :: acc) map []) in
              fold_left_s
                (fun (partial, qta, ctxt, origination) arg ->
                   interp ?log origination qta orig source amount ctxt lam (arg, partial)
                   >>=? fun (partial, qta, ctxt, origination) ->
                   return (partial, qta, ctxt, origination))
                (init, qta, ctxt, origination) items >>=? fun (res, qta, ctxt, origination) ->
              logged_return ~origination (Item (res, rest), qta, ctxt)
          | Map_mem, Item (v, Item (map, rest)) ->
              logged_return (Item (map_mem v map, rest), qta - 1, ctxt)
          | Map_get, Item (v, Item (map, rest)) ->
              logged_return (Item (map_get v map, rest), qta - 1, ctxt)
          | Map_update, Item (k, Item (v, Item (map, rest))) ->
              logged_return (Item (map_update k v map, rest), qta - 1, ctxt)
          (* timestamp operations *)
          | Add_seconds_to_timestamp, Item (n, Item (t, rest)) ->
              begin match Script_int.to_int64 n with
                | None -> fail (Overflow loc)
                | Some n ->
                    Lwt.return
                      (Period.of_seconds n >>? fun p ->
                       Timestamp.(t +? p) >>? fun res ->
                       Ok (Item (res, rest), qta - 1, ctxt)) >>=? fun res ->
                    logged_return res
              end
          | Add_timestamp_to_seconds, Item (t, Item (n, rest)) ->
              begin match Script_int.to_int64 n with
                | None -> fail (Overflow loc)
                | Some n ->
                    Lwt.return
                      (Period.of_seconds n >>? fun p ->
                       Timestamp.(t +? p) >>? fun res ->
                       Ok (Item (res, rest), qta - 1, ctxt)) >>=? fun res ->
                    logged_return res
              end
          (* string operations *)
          | Concat, Item (x, Item (y, rest)) ->
              logged_return (Item (x ^ y, rest), qta - 1, ctxt)
          (* currency operations *)
          | Add_tez, Item (x, Item (y, rest)) ->
              Lwt.return Tez.(x +? y) >>=? fun res ->
              logged_return (Item (res, rest), qta - 1, ctxt)
          | Sub_tez, Item (x, Item (y, rest)) ->
              Lwt.return Tez.(x -? y) >>=? fun res ->
              logged_return (Item (res, rest), qta - 1, ctxt)
          | Mul_teznat, Item (x, Item (y, rest)) ->
             begin
               match Script_int.to_int64 y with
               | None -> fail (Overflow loc)
               | Some y ->
                  Lwt.return Tez.(x *? y) >>=? fun res ->
                  logged_return (Item (res, rest), qta - 1, ctxt)
             end
          | Mul_nattez, Item (y, Item (x, rest)) ->
             begin
               match Script_int.to_int64 y with
               | None -> fail (Overflow loc)
               | Some y ->
                  Lwt.return Tez.(x *? y) >>=? fun res ->
                  logged_return (Item (res, rest), qta - 1, ctxt)
             end
          (* boolean operations *)
          | Or, Item (x, Item (y, rest)) ->
              logged_return (Item (x || y, rest), qta - 1, ctxt)
          | And, Item (x, Item (y, rest)) ->
              logged_return (Item (x && y, rest), qta - 1, ctxt)
          | Xor, Item (x, Item (y, rest)) ->
              logged_return (Item (not x && y || x && not y, rest), qta - 1, ctxt)
          | Not, Item (x, rest) ->
              logged_return (Item (not x, rest), qta - 1, ctxt)
          (* integer operations *)
          | Abs_int, Item (x, rest) ->
              logged_return (Item (Script_int.abs x, rest), qta - 1, ctxt)
          | Int_nat, Item (x, rest) ->
              logged_return (Item (Script_int.int x, rest), qta - 1, ctxt)
          | Neg_int, Item (x, rest) ->
              logged_return (Item (Script_int.neg x, rest), qta - 1, ctxt)
          | Neg_nat, Item (x, rest) ->
              logged_return (Item (Script_int.neg x, rest), qta - 1, ctxt)
          | Add_intint, Item (x, Item (y, rest)) ->
              logged_return (Item (Script_int.add x y, rest), qta - 1, ctxt)
          | Add_intnat, Item (x, Item (y, rest)) ->
              logged_return (Item (Script_int.add x y, rest), qta - 1, ctxt)
          | Add_natint, Item (x, Item (y, rest)) ->
              logged_return (Item (Script_int.add x y, rest), qta - 1, ctxt)
          | Add_natnat, Item (x, Item (y, rest)) ->
              logged_return (Item (Script_int.add_n x y, rest), qta - 1, ctxt)
          | Sub_int, Item (x, Item (y, rest)) ->
              logged_return (Item (Script_int.sub x y, rest), qta - 1, ctxt)
          | Mul_intint, Item (x, Item (y, rest)) ->
              logged_return (Item (Script_int.mul x y, rest), qta - 1, ctxt)
          | Mul_intnat, Item (x, Item (y, rest)) ->
              logged_return (Item (Script_int.mul x y, rest), qta - 1, ctxt)
          | Mul_natint, Item (x, Item (y, rest)) ->
              logged_return (Item (Script_int.mul x y, rest), qta - 1, ctxt)
          | Mul_natnat, Item (x, Item (y, rest)) ->
             logged_return (Item (Script_int.mul_n x y, rest), qta - 1, ctxt)

          | Ediv_teznat, Item (x, Item (y, rest)) ->
             let x = Script_int.of_int64 (Tez.to_cents x) in
             let result =
               match Script_int.ediv x y with
               | None -> None
               | Some (q, r) ->
                  match Script_int.to_int64 q,
                        Script_int.to_int64 r with
                  | Some q, Some r ->
                     begin
                       match Tez.of_cents q, Tez.of_cents r with
                       | Some q, Some r -> Some (q,r)
                       (* Cannot overflow *)
                       | _ -> assert false
                     end
                  (* Cannot overflow *)
                  | _ -> assert false
              in
              logged_return (Item (result, rest), qta -1, ctxt)

          | Ediv_tez, Item (x, Item (y, rest)) ->
             let x = Script_int.abs (Script_int.of_int64 (Tez.to_cents x)) in
             let y = Script_int.abs (Script_int.of_int64 (Tez.to_cents y)) in
             begin match Script_int.ediv_n x y with
               | None ->
                   logged_return (Item (None, rest), qta -1, ctxt)
               | Some (q, r) ->
                   let r =
                     match Script_int.to_int64 r with
                     | None -> assert false (* Cannot overflow *)
                     | Some r ->
                         match Tez.of_cents r with
                         | None -> assert false (* Cannot overflow *)
                         | Some r -> r in
                   logged_return (Item (Some (q, r), rest), qta -1, ctxt)
             end

          | Ediv_intint, Item (x, Item (y, rest)) ->
               logged_return (Item (Script_int.ediv x y, rest), qta -1, ctxt)
          | Ediv_intnat, Item (x, Item (y, rest)) ->
               logged_return (Item (Script_int.ediv x y, rest), qta -1, ctxt)
          | Ediv_natint, Item (x, Item (y, rest)) ->
               logged_return (Item (Script_int.ediv x y, rest), qta -1, ctxt)
          | Ediv_natnat, Item (x, Item (y, rest)) ->
               logged_return (Item (Script_int.ediv_n x y, rest), qta -1, ctxt)
          | Lsl_nat, Item (x, Item (y, rest)) ->
              begin match Script_int.shift_left_n x y with
                | None -> fail (Overflow loc)
                | Some r -> logged_return (Item (r, rest), qta - 1, ctxt)
              end
          | Lsr_nat, Item (x, Item (y, rest)) ->
              begin match Script_int.shift_right_n x y with
                | None -> fail (Overflow loc)
                | Some r -> logged_return (Item (r, rest), qta - 1, ctxt)
              end
          | Or_nat, Item (x, Item (y, rest)) ->
              logged_return (Item (Script_int.logor x y, rest), qta - 1, ctxt)
          | And_nat, Item (x, Item (y, rest)) ->
              logged_return (Item (Script_int.logand x y, rest), qta - 1, ctxt)
          | Xor_nat, Item (x, Item (y, rest)) ->
              logged_return (Item (Script_int.logxor x y, rest), qta - 1, ctxt)
          | Not_int, Item (x, rest) ->
              logged_return (Item (Script_int.lognot x, rest), qta - 1, ctxt)
          | Not_nat, Item (x, rest) ->
              logged_return (Item (Script_int.lognot x, rest), qta - 1, ctxt)
          (* control *)
          | Seq (hd, tl), stack ->
              step origination qta ctxt hd stack >>=? fun (trans, qta, ctxt, origination) ->
              step origination qta ctxt tl trans
          | If (bt, _), Item (true, rest) ->
              step origination qta ctxt bt rest
          | If (_, bf), Item (false, rest) ->
              step origination qta ctxt bf rest
          | Loop body, Item (true, rest) ->
              step origination qta ctxt body rest >>=? fun (trans, qta, ctxt, origination) ->
              step origination (qta - 1) ctxt descr trans
          | Loop _, Item (false, rest) ->
              logged_return (rest, qta, ctxt)
          | Dip b, Item (ign, rest) ->
              step origination qta ctxt b rest >>=? fun (res, qta, ctxt, origination) ->
              logged_return ~origination (Item (ign, res), qta, ctxt)
          | Exec, Item (arg, Item (lam, rest)) ->
              interp ?log origination qta orig source amount ctxt lam arg >>=? fun (res, qta, ctxt, origination) ->
              logged_return ~origination (Item (res, rest), qta - 1, ctxt)
          | Lambda lam, rest ->
              logged_return ~origination (Item (lam, rest), qta - 1, ctxt)
          | Fail, _ ->
              fail (Reject loc)
          | Nop, stack ->
              logged_return (stack, qta, ctxt)
          (* comparison *)
          | Compare Bool_key, Item (a, Item (b, rest)) ->
              let cmpres = Compare.Bool.compare a b in
              let cmpres = Script_int.of_int cmpres in
              logged_return (Item (cmpres, rest), qta - 1, ctxt)
          | Compare String_key, Item (a, Item (b, rest)) ->
              let cmpres = Compare.String.compare a b in
              let cmpres = Script_int.of_int cmpres in
              logged_return (Item (cmpres, rest), qta - 1, ctxt)
          | Compare Tez_key, Item (a, Item (b, rest)) ->
              let cmpres = Tez.compare a b in
              let cmpres = Script_int.of_int cmpres in
              logged_return (Item (cmpres, rest), qta - 1, ctxt)
          | Compare Int_key, Item (a, Item (b, rest)) ->
              let cmpres = Script_int.compare a b in
              let cmpres = Script_int.of_int cmpres in
              logged_return (Item (cmpres, rest), qta - 1, ctxt)
          | Compare Nat_key, Item (a, Item (b, rest)) ->
              let cmpres = Script_int.compare a b in
              let cmpres = Script_int.of_int cmpres in
              logged_return (Item (cmpres, rest), qta - 1, ctxt)
          | Compare Key_key, Item (a, Item (b, rest)) ->
              let cmpres = Ed25519.Public_key_hash.compare a b in
              let cmpres = Script_int.of_int cmpres in
              logged_return (Item (cmpres, rest), qta - 1, ctxt)
          | Compare Timestamp_key, Item (a, Item (b, rest)) ->
              let cmpres = Timestamp.compare a b in
              let cmpres = Script_int.of_int cmpres in
              logged_return (Item (cmpres, rest), qta - 1, ctxt)
          (* comparators *)
          | Eq, Item (cmpres, rest) ->
              let cmpres = Script_int.compare cmpres Script_int.zero in
              let cmpres = Compare.Int.(cmpres = 0) in
              logged_return (Item (cmpres, rest), qta - 1, ctxt)
          | Neq, Item (cmpres, rest) ->
              let cmpres = Script_int.compare cmpres Script_int.zero in
              let cmpres = Compare.Int.(cmpres <> 0) in
              logged_return (Item (cmpres, rest), qta - 1, ctxt)
          | Lt, Item (cmpres, rest) ->
              let cmpres = Script_int.compare cmpres Script_int.zero in
              let cmpres = Compare.Int.(cmpres < 0) in
              logged_return (Item (cmpres, rest), qta - 1, ctxt)
          | Le, Item (cmpres, rest) ->
              let cmpres = Script_int.compare cmpres Script_int.zero in
              let cmpres = Compare.Int.(cmpres <= 0) in
              logged_return (Item (cmpres, rest), qta - 1, ctxt)
          | Gt, Item (cmpres, rest) ->
              let cmpres = Script_int.compare cmpres Script_int.zero in
              let cmpres = Compare.Int.(cmpres > 0) in
              logged_return (Item (cmpres, rest), qta - 1, ctxt)
          | Ge, Item (cmpres, rest) ->
              let cmpres = Script_int.compare cmpres Script_int.zero in
              let cmpres = Compare.Int.(cmpres >= 0) in
              logged_return (Item (cmpres, rest), qta - 1, ctxt)
          (* protocol *)
          | Manager, Item ((_, _, contract), rest) ->
              Contract.get_manager ctxt contract >>=? fun manager ->
              logged_return (Item (manager, rest), qta - 1, ctxt)
          | Transfer_tokens storage_type,
            Item (p, Item (amount, Item ((tp, Unit_t, destination), Item (sto, Empty)))) -> begin
              Contract.spend_from_script ctxt source amount >>=? fun ctxt ->
              Contract.credit ctxt destination amount >>=? fun ctxt ->
              Contract.get_script ctxt destination >>=? fun destination_script ->
              let sto = unparse_data storage_type sto in
              Contract.update_script_storage_and_fees ctxt source dummy_storage_fee sto >>=? fun ctxt ->
              begin match destination_script with
                | None ->
                    (* we see non scripted contracts as (unit, unit) contract *)
                    Lwt.return (ty_eq tp Unit_t |>
                                record_trace (Invalid_contract (loc, destination))) >>=? fun (Eq _) ->
                    return (ctxt, qta, origination)
                | Some { code ; storage } ->
                    let p = unparse_data tp p in
                    execute origination source destination ctxt storage code amount p qta
                    >>=? fun (csto, ret, qta, ctxt, origination) ->
                    Contract.update_script_storage_and_fees ctxt destination dummy_storage_fee csto >>=? fun ctxt ->
                    trace
                      (Invalid_contract (loc, destination))
                      (parse_data ctxt Unit_t ret) >>=? fun () ->
                    return (ctxt, qta, origination)
              end >>=? fun (ctxt, qta, origination) ->
              Contract.get_script ctxt source >>=? (function
                  | None -> assert false
                  | Some { storage = { storage } } ->
                      parse_data ctxt storage_type storage >>=? fun sto ->
                      logged_return ~origination (Item ((), Item (sto, Empty)), qta - 1, ctxt))
            end
          | Transfer_tokens storage_type,
            Item (p, Item (amount, Item ((tp, tr, destination), Item (sto, Empty)))) -> begin
              Contract.spend_from_script ctxt source amount >>=? fun ctxt ->
              Contract.credit ctxt destination amount >>=? fun ctxt ->
              Contract.get_script ctxt destination >>=? function
              | None -> fail (Invalid_contract (loc, destination))
              | Some { code ; storage } ->
                  let sto = unparse_data storage_type sto in
                  Contract.update_script_storage_and_fees ctxt source dummy_storage_fee sto >>=? fun ctxt ->
                  let p = unparse_data tp p in
                  execute origination source destination ctxt storage code amount p qta
                  >>=? fun (sto, ret, qta, ctxt, origination) ->
                  Contract.update_script_storage_and_fees ctxt destination dummy_storage_fee sto >>=? fun ctxt ->
                  trace
                    (Invalid_contract (loc, destination))
                    (parse_data ctxt tr ret) >>=? fun v ->
                  Contract.get_script ctxt source >>=? (function
                      | None -> assert false
                      | Some { storage = { storage } } ->
                          parse_data ctxt storage_type storage >>=? fun sto ->
                          logged_return ~origination (Item (v, Item (sto, Empty)), qta - 1, ctxt))
            end
          | Create_account,
            Item (manager, Item (delegate, Item (delegatable, Item (credit, rest)))) ->
              Contract.spend_from_script ctxt source credit >>=? fun ctxt ->
              Lwt.return Tez.(credit -? Constants.origination_burn) >>=? fun balance ->
              Contract.originate ctxt
                origination
                ~manager ~delegate ~balance
                ?script:None ~spendable:true ~delegatable >>=? fun (ctxt, contract, origination) ->
              logged_return ~origination (Item ((Unit_t, Unit_t, contract), rest), qta - 1, ctxt)
          | Default_account, Item (key, rest) ->
              let contract = Contract.default_contract key in
              logged_return (Item ((Unit_t, Unit_t, contract), rest), qta - 1, ctxt)
          | Create_contract (g, p, r),
            Item (manager, Item (delegate, Item (spendable, Item (delegatable, Item (credit,
                                                                    Item (Lam (_, code), Item (init, rest))))))) ->
              let code, storage =
                { code; arg_type = unparse_ty p; ret_type = unparse_ty r; storage_type =  unparse_ty g },
                { storage = unparse_data g init; storage_type =  unparse_ty g } in
              Contract.spend_from_script ctxt source credit >>=? fun ctxt ->
              Lwt.return Tez.(credit -? Constants.origination_burn) >>=? fun balance ->
              Contract.originate ctxt
                origination
                ~manager ~delegate ~balance
                ~script:({ code ; storage }, (dummy_code_fee, dummy_storage_fee))
                ~spendable ~delegatable
              >>=? fun (ctxt, contract, origination) ->
              logged_return ~origination (Item ((p, r, contract), rest), qta - 1, ctxt)
          | Balance, rest ->
              Contract.get_balance ctxt source >>=? fun balance ->
              logged_return (Item (balance, rest), qta - 1, ctxt)
          | Now, rest ->
              let now = Timestamp.current ctxt in
              logged_return (Item (now, rest), qta - 1, ctxt)
          | Check_signature, Item (key, Item ((signature, message), rest)) ->
              Public_key.get ctxt key >>=? fun key ->
              let message = MBytes.of_string message in
              let res = Ed25519.Signature.check key signature message in
              logged_return (Item (res, rest), qta - 1, ctxt)
          | H ty, Item (v, rest) ->
              let hash = Script.hash_expr (unparse_data ty v) in
              logged_return (Item (hash, rest), qta - 1, ctxt)
          | Steps_to_quota, rest ->
              let steps = Script_int.abs (Script_int.of_int qta) in
              logged_return (Item (steps, rest), qta - 1, ctxt)
          | Source (ta, tb), rest ->
              logged_return (Item ((ta, tb, orig), rest), qta - 1, ctxt)
          | Amount, rest ->
              logged_return (Item (amount, rest), qta - 1, ctxt)
    in
    let stack = (Item (arg, Empty)) in
    begin match log with
      | None -> ()
      | Some log ->
          log := (code.loc, qta, unparse_stack (stack, code.bef)) :: !log
    end ;
    step origination qta ctxt code stack >>=? fun (Item (ret, Empty), qta, ctxt, origination) ->
    return (ret, qta, ctxt, origination)

(* ---- contract handling ---------------------------------------------------*)

and execute ?log origination orig source ctxt storage script amount arg qta =
  let { Script.storage ; storage_type } = storage in
  let { Script.code ; arg_type ; ret_type } = script in
  (Lwt.return (parse_ty arg_type)) >>=? fun (Ex_ty arg_type) ->
  (Lwt.return (parse_ty ret_type)) >>=? fun (Ex_ty ret_type) ->
  (Lwt.return (parse_ty storage_type)) >>=? fun (Ex_ty storage_type) ->
  let arg_type_full = Pair_t (arg_type, storage_type) in
  let ret_type_full = Pair_t (ret_type, storage_type) in
  trace
    (Ill_typed_contract (code, arg_type, ret_type, storage_type, []))
    (parse_lambda ~storage_type ctxt arg_type_full ret_type_full code) >>=? fun lambda ->
  parse_data ctxt arg_type arg >>=? fun arg ->
  parse_data ctxt storage_type storage >>=? fun storage ->
  trace
    (Runtime_contract_error (source, code, arg_type, ret_type_full, storage_type))
    (interp ?log origination qta orig source amount ctxt lambda (arg, storage))
  >>=? fun (ret, qta, ctxt, origination) ->
  let ret, storage = ret in
  return (unparse_data storage_type storage,
          unparse_data ret_type ret,
          qta, ctxt, origination)

let trace origination orig source ctxt storage script amount arg qta =
  let log = ref [] in
  execute ~log origination orig source ctxt storage script amount arg qta >>=? fun res ->
  return (res, List.rev !log)

let execute orig source ctxt storage script amount arg qta =
  execute orig source ctxt storage script amount arg qta
