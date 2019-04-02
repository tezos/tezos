(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Nomadic Development. <contact@tezcore.com>             *)
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

type nanotez = Z.t
let nanotez_enc =
  Data_encoding.def "nanotez"
    ~title:"Thousandths of tez"
    ~description:"One thousand nanotez make a tez"
    Data_encoding.z

type config =
  { minimal_fees : Tez.t ;
    minimal_nanotez_per_gas_unit : nanotez ;
    minimal_nanotez_per_byte : nanotez ;
    allow_script_failure : bool }

let default_minimal_fees = match Tez.of_mutez 100L with None -> assert false | Some t -> t
let default_minimal_nanotez_per_gas_unit = Z.of_int 100
let default_minimal_nanotez_per_byte = Z.of_int 1000

let config_encoding : config Data_encoding.t =
  let open Data_encoding in
  conv
    (fun { minimal_fees ;
           minimal_nanotez_per_gas_unit ;
           minimal_nanotez_per_byte ;
           allow_script_failure } ->
      (minimal_fees,
       minimal_nanotez_per_gas_unit,
       minimal_nanotez_per_byte,
       allow_script_failure))
    (fun (minimal_fees,
          minimal_nanotez_per_gas_unit,
          minimal_nanotez_per_byte,
          allow_script_failure) ->
      { minimal_fees ;
        minimal_nanotez_per_gas_unit ;
        minimal_nanotez_per_byte ;
        allow_script_failure })
    (obj4
       (dft "minimal_fees" Tez.encoding default_minimal_fees)
       (dft "minimal_nanotez_per_gas_unit" nanotez_enc default_minimal_nanotez_per_gas_unit)
       (dft "minimal_nanotez_per_byte" nanotez_enc default_minimal_nanotez_per_byte)
       (dft "allow_script_failure" bool true))

let default_config =
  { minimal_fees = default_minimal_fees ;
    minimal_nanotez_per_gas_unit = default_minimal_nanotez_per_gas_unit ;
    minimal_nanotez_per_byte = default_minimal_nanotez_per_byte ;
    allow_script_failure = true ;
  }

module Proto = Tezos_embedded_protocol_003_PsddFKi3.Registerer.Registered

let get_manager_operation_gas_and_fee contents =
  let open Operation in
  let l = to_list (Contents_list contents) in
  List.fold_left
    (fun acc -> function
       | Contents (Manager_operation { fee ; gas_limit ; _ }) -> begin
           match acc with
           | Error _ as e -> e
           | Ok (total_fee, total_gas) ->
               match Tez.(total_fee +? fee) with
               | Ok total_fee -> Ok (total_fee, (Z.add total_gas gas_limit))
               | Error _ as e -> e
         end
       | _ -> acc)
    (Ok (Tez.zero, Z.zero))
    l

let pre_filter_manager
  : type t. config -> t Kind.manager contents_list -> int -> bool
  = fun config op size ->
    match get_manager_operation_gas_and_fee op with
    | Error _ -> false
    | Ok (fee, gas) ->
        let fees_in_nanotez =
          Z.mul (Z.of_int64 (Tez.to_mutez fee)) (Z.of_int 1000) in
        let minimal_fees_in_nanotez =
          Z.mul (Z.of_int64 (Tez.to_mutez config.minimal_fees)) (Z.of_int 1000) in
        let minimal_fees_for_gas_in_nanotez =
          Z.mul config.minimal_nanotez_per_gas_unit gas in
        let minimal_fees_for_size_in_nanotez =
          Z.mul config.minimal_nanotez_per_byte (Z.of_int size) in
        Z.compare
          fees_in_nanotez
          (Z.add
             minimal_fees_in_nanotez
             (Z.add minimal_fees_for_gas_in_nanotez minimal_fees_for_size_in_nanotez))
        >= 0

let pre_filter config (Operation_data { contents } as op : Operation.packed_protocol_data) =
  let bytes =
    Data_encoding.Binary.fixed_length_exn
      Tezos_base.Operation.shell_header_encoding +
    Data_encoding.Binary.length
      Operation.protocol_data_encoding
      op in
  match contents with
  | Single (Endorsement _) -> true
  | Single (Seed_nonce_revelation _) -> true
  | Single (Double_endorsement_evidence _) -> true
  | Single (Double_baking_evidence _) -> true
  | Single (Activate_account _) -> true
  | Single (Proposals _) -> true
  | Single (Ballot _) -> true
  | Single (Manager_operation _) as op -> pre_filter_manager config op bytes
  | Cons (Manager_operation _, _) as op -> pre_filter_manager config op bytes

open Apply_results

let rec post_filter_manager
  : type t. Alpha_context.t -> t Kind.manager contents_result_list -> config -> bool Lwt.t
  = fun ctxt op config -> match op with
    | Single_result (Manager_operation_result { operation_result }) ->
        begin match operation_result with
          | Applied _ -> Lwt.return_true
          | Skipped _ | Failed _ | Backtracked _ ->
              Lwt.return config.allow_script_failure
        end
    | Cons_result (Manager_operation_result res, rest) ->
        post_filter_manager ctxt (Single_result (Manager_operation_result res)) config >>= function
        | false -> Lwt.return_false
        | true -> post_filter_manager ctxt rest config

let post_filter config
    ~validation_state_before:_
    ~validation_state_after: ({ Main.ctxt } : Proto.validation_state)
    (_op, receipt) =
  match receipt with
  | No_operation_metadata -> assert false (* only for multipass validator *)
  | Operation_metadata { contents } ->
      match contents with
      | Single_result (Endorsement_result _) -> Lwt.return_true
      | Single_result (Seed_nonce_revelation_result _) -> Lwt.return_true
      | Single_result (Double_endorsement_evidence_result _) -> Lwt.return_true
      | Single_result (Double_baking_evidence_result _) -> Lwt.return_true
      | Single_result (Activate_account_result _) -> Lwt.return_true
      | Single_result (Proposals_result) -> Lwt.return_true
      | Single_result (Ballot_result) -> Lwt.return_true
      | Single_result (Manager_operation_result _) as op -> post_filter_manager ctxt op config
      | Cons_result (Manager_operation_result _, _) as op -> post_filter_manager ctxt op config
