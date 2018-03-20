(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Proto_alpha
open Alpha_context

module Assert = struct
  let fail expected given msg =
    Format.kasprintf Pervasives.failwith
      "@[%s@ expected: %s@ got: %s@]" msg expected given
  let fail_msg fmt = Format.kasprintf (fail "" "") fmt

  let default_printer _ = ""

  let equal ?(eq=(=)) ?(prn=default_printer) ?(msg="") x y =
    if not (eq x y) then fail (prn x) (prn y) msg

  let is_true ?(msg="") x =
    if not x then fail "true" "false" msg

  let is_false ?(msg="") x =
    if x then fail "false" "true" msg

  let is_some ?(msg="") x =
    if x = None then fail "Some _" "None" msg

  let is_none ?(msg="") x =
    if x <> None then fail "None" "Some _" msg

  let make_equal e p = equal ~eq:e ~prn:p
  let string_of_string s = Printf.sprintf "%S" s
  let equal_string = make_equal (=) string_of_string


end

let wrap_result = Alpha_environment.wrap_error

let wrap = fun x -> Lwt.return @@ wrap_result x

let (>>=??) x y = x >>= wrap >>=? y

let (>>??) x y = wrap_result x >>? y

let (>>?=) x y = x >>= wrap >>= y

open Proto_alpha.Error_monad

let tmp_map f lst =
  let rec aux acc = function
    | [] -> ok acc
    | hd :: tl ->
        f hd >>? fun fhd -> (aux (fhd :: acc) tl)
  in
  aux [] lst


let ok ?msg = function
  | Ok x -> return x
  | Error errs ->
      Helpers_logger.log_error "Error : %a" pp @@ List.hd errs ;
      Assert.is_true ~msg:(Option.unopt ~default:"not ok" msg) false ;
      fail @@ List.hd errs


let ok_contract ?msg x =
  ok x >>=? fun (((_, errs), _) as x) ->
  Assert.is_none ?msg errs ;
  return x


exception No_error

let no_error ?msg = function
  | Ok x -> x
  | Error _ ->
      Assert.is_true
        ~msg: (Option.unopt ~default:"yes error" msg)
        false ;
      raise No_error


let equal_pkh ?msg pkh1 pkh2 =
  let eq pkh1 pkh2 =
    match pkh1, pkh2 with
    | None, None -> true
    | Some pkh1, Some pkh2 ->
        Ed25519.Public_key_hash.equal pkh1 pkh2
    | _ -> false in
  let prn = function
    | None -> "none"
    | Some pkh -> Ed25519.Public_key_hash.to_hex pkh in
  Assert.equal ?msg ~prn ~eq pkh1 pkh2

let equal_int64 ?msg =
  Assert.equal
    ~eq: Int64.equal
    ~prn: Int64.to_string
    ~msg: (Option.unopt ~default:"int64_equal" msg)


let equal_int ?msg =
  Assert.equal
    ~eq: (=)
    ~prn: string_of_int
    ~msg: (Option.unopt ~default:"int_equal" msg)



let equal_tez ?msg =
  Assert.equal
    ~eq: Tez .(=)
    ~prn: Tez.to_string
    ~msg: (Option.unopt ~default:"tez_equal" msg)


let equal_balance ~tc ?msg (contract, expected_balance) =
  Contract.get_balance tc contract >>=? fun balance ->
  return @@
  equal_tez
    expected_balance balance
    ~msg: (Option.unopt ~default:"balance_equal" msg)


let equal_cents_balance ~tc ?msg (contract, cents_balance) =
  equal_balance
    ~tc
    ~msg: (Option.unopt ~default:"equal_cents_balance" msg)
    (contract, Helpers_cast.cents_of_int cents_balance)

let ecoproto_error f = function
  | Alpha_environment.Ecoproto_error errors ->
      List.exists f errors
  | _ -> false

let contain_error ?(msg="") ~f = function
  | Ok _ -> Assert.fail "Error _" "Ok _" msg
  | Error error when not (List.exists f error) ->
      let error_str = Format.asprintf "%a" Tezos_error_monad.Error_monad.pp_print_error error in
      Assert.fail "" error_str msg
  | _ -> ()

let generic_economic_error ~msg =
  contain_error ~msg ~f: (ecoproto_error (fun _ -> true))

let economic_error ~msg f =
  contain_error ~msg ~f: (ecoproto_error f)

let ill_typed_data_error ~msg =
  let aux = function
    | Proto_alpha.Script_tc_errors.Ill_typed_data _ -> true
    | _ -> false in
  economic_error ~msg aux

let ill_typed_return_error ~msg =
  let aux = function
    | Proto_alpha.Script_tc_errors.Bad_return _ -> true
    | _ -> false in
  economic_error ~msg aux

let double_endorsement_evidence ~msg =
  let aux = function
    | Proto_alpha.Apply.Duplicate_endorsement(_) -> true
    | _ -> false
  in
  economic_error ~msg aux

let contain_error_alpha ?msg ~f = function
  | Ok _ -> ()
  | Error errs ->
      if (not @@ List.exists f errs)
      then Assert.is_true
          ~msg:(Option.unopt ~default:"yes error" msg) false


let unknown_contract ~msg =
  let f = function
    | Proto_alpha.Raw_context.Storage_error _ -> true
    | _ -> false
  in
  contain_error_alpha ~msg ~f


let non_existing_contract ~msg =
  contain_error_alpha ~msg ~f: (function
      | Proto_alpha.Contract_storage.Non_existing_contract _ -> true
      | _ -> false)


let balance_too_low ~msg =
  contain_error_alpha ~msg ~f: (function
      | Contract.Balance_too_low _ -> true
      | _ -> false)


let non_spendable ~msg =
  contain_error ~msg ~f: begin ecoproto_error (function
      | Proto_alpha.Contract_storage.Unspendable_contract _ -> true
      | error ->
          Helpers_logger.debug "Actual error: %a" pp error ;
          false)
  end

let inconsistent_pkh ~msg =
  contain_error ~msg ~f: begin ecoproto_error (function
      | Proto_alpha.Contract_storage.Inconsistent_hash _ -> true
      | _ -> false)
  end

let non_delegatable ~msg =
  contain_error ~msg ~f: begin ecoproto_error (function
      | Proto_alpha.Delegate_storage.Non_delegatable_contract _ -> true
      | _ -> false)
  end

include Assert
