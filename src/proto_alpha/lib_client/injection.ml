(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2018 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

open Proto_alpha
open Alpha_context
open Apply_results

let get_branch (rpc_config: #Proto_alpha.full)
    ~chain ~(block : Block_services.block) branch =
  let branch = Option.unopt ~default:0 branch in (* TODO export parameter *)
  begin
    match block with
    | `Head n -> return (`Head (n+branch))
    | `Hash (h,n) -> return (`Hash (h,n+branch))
    | `Genesis -> return `Genesis
    | `Level i -> return (`Level i)
  end >>=? fun block ->
  Shell_services.Blocks.hash rpc_config ~chain ~block () >>=? fun hash ->
  Shell_services.Chain.chain_id rpc_config ~chain () >>=? fun chain_id ->
  return (chain_id, hash)

type 'kind preapply_result =
  Operation_hash.t * 'kind operation * 'kind operation_metadata

type 'kind result_list =
  Operation_hash.t * 'kind contents_list * 'kind contents_result_list

type 'kind result =
  Operation_hash.t * 'kind contents * 'kind contents_result

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

type fee_parameter = {
  minimal_fees: Tez.t ;
  minimal_nanotez_per_byte: Z.t ;
  minimal_nanotez_per_gas_unit: Z.t ;
  force_low_fee: bool ;
  fee_cap: Tez.t ;
  burn_cap: Tez.t ;
}

let dummy_fee_parameter = {
  minimal_fees = Tez.zero ;
  minimal_nanotez_per_byte = Z.zero ;
  minimal_nanotez_per_gas_unit = Z.zero ;
  force_low_fee = false ;
  fee_cap = Tez.one ;
  burn_cap = Tez.zero ;
}

let check_fees
  : type t. #Proto_alpha.full -> fee_parameter -> t contents_list -> int -> unit Lwt.t
  = fun cctxt config op size ->
    match get_manager_operation_gas_and_fee op with
    | Error _ -> assert false (* FIXME *)
    | Ok (fee, gas) ->
        if Tez.compare fee config.fee_cap > 0 then
          cctxt#error "The proposed fee (%s%a) are higher than the configured fee cap (%s%a).@\n\
                      \ Use `--fee-cap %a` to emit this operation anyway."
            Client_proto_args.tez_sym Tez.pp fee
            Client_proto_args.tez_sym Tez.pp config.fee_cap
            Tez.pp fee >>= fun () ->
          exit 1
        else begin (* *)
          let fees_in_nanotez =
            Z.mul (Z.of_int64 (Tez.to_mutez fee)) (Z.of_int 1000) in
          let minimal_fees_in_nanotez =
            Z.mul (Z.of_int64 (Tez.to_mutez config.minimal_fees)) (Z.of_int 1000) in
          let minimal_fees_for_gas_in_nanotez =
            Z.mul config.minimal_nanotez_per_gas_unit gas in
          let minimal_fees_for_size_in_nanotez =
            Z.mul config.minimal_nanotez_per_byte (Z.of_int size) in
          let estimated_fees_in_nanotez =
            Z.add
              minimal_fees_in_nanotez
              (Z.add minimal_fees_for_gas_in_nanotez minimal_fees_for_size_in_nanotez) in
          let estimated_fees =
            match Tez.of_mutez (Z.to_int64 (Z.div (Z.add (Z.of_int 999) estimated_fees_in_nanotez) (Z.of_int 1000))) with
            | None -> assert false
            | Some fee -> fee in
          if not config.force_low_fee &&
             Z.compare fees_in_nanotez estimated_fees_in_nanotez < 0 then begin
            cctxt#error "The proposed fee (%s%a) are lower than the fee that baker \
                         expect by default (%s%a).@\n\
                        \ Use `--force-low-fee` to emit this operation anyway."
              Client_proto_args.tez_sym Tez.pp fee
              Client_proto_args.tez_sym Tez.pp estimated_fees >>= fun () ->
            exit 1
          end else
            Lwt.return_unit
        end

let print_for_verbose_signing ppf ~watermark ~bytes ~branch ~contents =
  let open Format in
  pp_open_vbox ppf 0 ;
  let item f =
    pp_open_hovbox ppf 4 ;
    pp_print_string ppf "  * ";
    f ppf () ;
    pp_close_box ppf () ;
    pp_print_cut ppf () in
  let hash_pp l =
    fprintf ppf "%s"
      (Base58.raw_encode Blake2B.(hash_bytes l |> to_string)) in
  item (fun ppf () ->
      pp_print_text ppf "Branch: " ;
      Block_hash.pp ppf branch ) ;
  item (fun ppf () ->
      fprintf ppf "Watermark: `%a` (0x%s)" 
        Signature.pp_watermark watermark
        (MBytes.to_hex (Signature.bytes_of_watermark watermark) |> Hex.show) ) ;
  item (fun ppf () ->
      pp_print_text ppf "Operation bytes: " ;
      TzString.fold_left (* We split the bytes into lines for display: *)
        (fun n c ->
           pp_print_char ppf c ;
           if n < 72 (* is the email-body standard width, ideal for copy-pasting. *)
           then  n + 1
           else (pp_print_space ppf () ; 0) )
        0
        (MBytes.to_hex bytes |> Hex.show) |> ignore ) ;
  item (fun ppf () ->
      pp_print_text ppf "Blake 2B Hash (raw): " ;
      hash_pp [bytes] ) ;
  item (fun ppf () ->
      pp_print_text ppf
        "Blake 2B Hash (ledger-style, with operation watermark): " ;
      hash_pp [Signature.bytes_of_watermark watermark ; bytes] ) ;
  let json =
    Data_encoding.Json.construct
      Operation.unsigned_encoding
      ({ branch }, Contents_list contents) in
  item (fun ppf () ->
      pp_print_text ppf "JSON encoding: " ;
      Data_encoding.Json.pp ppf json ) ;
  pp_close_box ppf ()

let preapply (type t)
    (cctxt: #Proto_alpha.full) ~chain ~block
    ?(verbose_signing = false)
    ?fee_parameter
    ?branch ?src_sk (contents : t contents_list) =
  get_branch cctxt ~chain ~block branch >>=? fun (chain_id, branch) ->
  let bytes =
    Data_encoding.Binary.to_bytes_exn
      Operation.unsigned_encoding
      ({ branch }, Contents_list contents) in
  begin
    match src_sk with
    | None -> return_none
    | Some src_sk ->
        let watermark =
          match contents with
          | Single (Endorsement _) -> Signature.(Endorsement chain_id)
          | _ -> Signature.Generic_operation in
        begin
          if verbose_signing then
            cctxt#message "Pre-signature information (verbose signing):@.%t%!"
              (print_for_verbose_signing ~watermark ~bytes ~branch ~contents)
          else Lwt.return_unit
        end >>= fun () ->
        Client_keys.sign cctxt ~watermark src_sk bytes
        >>=? fun signature ->
        return_some signature
  end >>=? fun signature ->
  let op : _ Operation.t =
    { shell = { branch } ;
      protocol_data = { contents ; signature } } in
  let oph = Operation.hash op in
  let size = MBytes.length bytes + Signature.size in
  begin
    match fee_parameter with
    | Some fee_parameter ->
        check_fees cctxt fee_parameter contents size
    | None -> Lwt.return_unit
  end >>= fun () ->
  Alpha_block_services.Helpers.Preapply.operations
    cctxt ~chain ~block [Operation.pack op] >>=? function
  | [(Operation_data op', Operation_metadata result)] -> begin
      match Operation.equal
              op { shell = { branch } ; protocol_data = op' },
            Apply_results.kind_equal_list contents result.contents with
      | Some Operation.Eq, Some Apply_results.Eq ->
          return ((oph, op, result) : t preapply_result)
      | _ -> failwith "Unexpected result"
    end
  | _ -> failwith "Unexpected result"

let simulate (type t)
    (cctxt: #Proto_alpha.full) ~chain ~block
    ?branch (contents : t contents_list) =
  get_branch cctxt ~chain ~block branch >>=? fun (_chain_id, branch) ->
  let op : _ Operation.t =
    { shell = { branch } ;
      protocol_data = { contents ; signature = None } } in
  let oph = Operation.hash op in
  Alpha_services.Helpers.Scripts.run_operation
    cctxt (chain, block) (Operation.pack op) >>=? function
  | (Operation_data op', Operation_metadata result) -> begin
      match Operation.equal
              op { shell = { branch } ; protocol_data = op' },
            Apply_results.kind_equal_list contents result.contents with
      | Some Operation.Eq, Some Apply_results.Eq ->
          return ((oph, op, result) : t preapply_result)
      | _ -> failwith "Unexpected result"
    end
  | _ -> failwith "Unexpected result"

let estimated_gas_single
    (type kind)
    (Manager_operation_result { operation_result ;
                                internal_operation_results ; _ }
     : kind Kind.manager contents_result) =
  let consumed_gas (type kind) (result : kind manager_operation_result) =
    match result with
    | Applied (Transaction_result { consumed_gas ; _ }) -> Ok consumed_gas
    | Applied (Origination_result { consumed_gas ; _ }) -> Ok consumed_gas
    | Applied (Reveal_result { consumed_gas }) -> Ok consumed_gas
    | Applied (Delegation_result { consumed_gas }) -> Ok consumed_gas
    | Skipped _ -> assert false
    | Backtracked (_, None) -> Ok Z.zero (* there must be another error for this to happen *)
    | Backtracked (_, Some errs) -> Alpha_environment.wrap_error (Error errs)
    | Failed (_, errs) -> Alpha_environment.wrap_error (Error errs) in
  List.fold_left
    (fun acc (Internal_operation_result (_, r)) ->
       acc >>? fun acc ->
       consumed_gas r >>? fun gas ->
       Ok (Z.add acc gas))
    (consumed_gas operation_result) internal_operation_results

let estimated_storage_single
    (type kind)
    origination_size
    (Manager_operation_result { operation_result ;
                                internal_operation_results ;
                                _ }
     : kind Kind.manager contents_result) =
  let storage_size_diff (type kind) (result : kind manager_operation_result) =
    match result with
    | Applied (Transaction_result { paid_storage_size_diff ; allocated_destination_contract ; _ }) ->
        if allocated_destination_contract then
          Ok (Z.add paid_storage_size_diff origination_size)
        else
          Ok paid_storage_size_diff
    | Applied (Origination_result { paid_storage_size_diff ; _ }) ->
        Ok (Z.add paid_storage_size_diff origination_size)
    | Applied (Reveal_result _)-> Ok Z.zero
    | Applied (Delegation_result _) -> Ok Z.zero
    | Skipped _ -> assert false
    | Backtracked (_, None) -> Ok Z.zero (* there must be another error for this to happen *)
    | Backtracked (_, Some errs) -> Alpha_environment.wrap_error (Error errs)
    | Failed (_, errs) -> Alpha_environment.wrap_error (Error errs) in
  List.fold_left
    (fun acc (Internal_operation_result (_, r)) ->
       acc >>? fun acc ->
       storage_size_diff r >>? fun storage ->
       Ok (Z.add acc storage))
    (storage_size_diff operation_result) internal_operation_results

let estimated_storage origination_size res =
  let rec estimated_storage :
    type kind. kind contents_result_list -> _ =
    function
    | Single_result (Manager_operation_result _ as res) -> estimated_storage_single origination_size res
    | Single_result _ -> Ok Z.zero
    | Cons_result (res, rest) ->
        estimated_storage_single origination_size res >>? fun storage1 ->
        estimated_storage rest >>? fun storage2 ->
        Ok (Z.add storage1 storage2) in
  estimated_storage res >>? fun diff ->
  Ok (Z.max Z.zero diff)

let originated_contracts_single
    (type kind)
    (Manager_operation_result { operation_result ;
                                internal_operation_results ;
                                _ }
     : kind Kind.manager contents_result) =
  let originated_contracts (type kind) (result : kind manager_operation_result) =
    match result with
    | Applied (Transaction_result { originated_contracts ; _ }) -> Ok originated_contracts
    | Applied (Origination_result { originated_contracts ; _ }) -> Ok originated_contracts
    | Applied (Reveal_result _) -> Ok []
    | Applied (Delegation_result _) -> Ok []
    | Skipped _ -> assert false
    | Backtracked (_, None) -> Ok [] (* there must be another error for this to happen *)
    | Backtracked (_, Some errs) -> Alpha_environment.wrap_error (Error errs)
    | Failed (_, errs) -> Alpha_environment.wrap_error (Error errs) in
  List.fold_left
    (fun acc (Internal_operation_result (_, r)) ->
       acc >>? fun acc ->
       originated_contracts r >>? fun contracts ->
       Ok (List.rev_append contracts acc))
    (originated_contracts operation_result >|? List.rev)
    internal_operation_results

let rec originated_contracts :
  type kind. kind contents_result_list -> _ =
  function
  | Single_result (Manager_operation_result _ as res) ->
      originated_contracts_single res >|? List.rev
  | Single_result _ -> Ok []
  | Cons_result (res, rest) ->
      originated_contracts_single res >>? fun contracts1 ->
      originated_contracts rest >>? fun contracts2 ->
      Ok (List.rev_append contracts1 contracts2)

let detect_script_failure :
  type kind. kind operation_metadata -> _ =
  let rec detect_script_failure :
    type kind. kind contents_result_list -> _ =
    let detect_script_failure_single
        (type kind)
        (Manager_operation_result { operation_result ;
                                    internal_operation_results ; _ }
         : kind Kind.manager contents_result) =
      let detect_script_failure (type kind) (result : kind manager_operation_result) =
        match result with
        | Applied _ -> Ok ()
        | Skipped _ -> assert false
        | Backtracked (_, None) -> (* there must be another error for this to happen *)
            Ok ()
        | Backtracked (_, Some errs) ->
            record_trace
              (failure "The transfer simulation failed.")
              (Alpha_environment.wrap_error (Error errs))
        | Failed (_, errs) ->
            record_trace
              (failure "The transfer simulation failed.")
              (Alpha_environment.wrap_error (Error errs)) in
      List.fold_left
        (fun acc (Internal_operation_result (_, r)) ->
           acc >>? fun () ->
           detect_script_failure r)
        (detect_script_failure operation_result)
        internal_operation_results in
    function
    | Single_result (Manager_operation_result _ as res) ->
        detect_script_failure_single res
    | Single_result _ ->
        Ok ()
    | Cons_result (res, rest) ->
        detect_script_failure_single res >>? fun () ->
        detect_script_failure rest in
  fun { contents } -> detect_script_failure contents

let may_patch_limits
    (type kind) (cctxt : #Proto_alpha.full)
    ~fee_parameter
    ~chain ~block ?branch ?(compute_fee = false)
    (contents: kind contents_list) : kind contents_list tzresult Lwt.t =
  Alpha_services.Constants.all cctxt
    (chain, block) >>=? fun {
    parametric = {
      hard_gas_limit_per_operation = gas_limit ;
      hard_storage_limit_per_operation = storage_limit ;
      origination_size ;
      cost_per_byte ;
      _ ;
    } ;
    _ ; } ->
  let may_need_patching_single
    : type kind. kind contents -> kind contents option = function
    | Manager_operation c
      when compute_fee || c.gas_limit < Z.zero || gas_limit <= c.gas_limit
           || c.storage_limit < Z.zero || storage_limit <= c.storage_limit ->
        let gas_limit =
          if c.gas_limit < Z.zero || gas_limit <= c.gas_limit then
            gas_limit
          else
            c.gas_limit in
        let storage_limit =
          if c.storage_limit < Z.zero || storage_limit <= c.storage_limit then
            storage_limit
          else
            c.storage_limit in
        Some (Manager_operation { c with gas_limit ; storage_limit })
    | _ -> None in
  let rec may_need_patching
    : type kind. kind contents_list -> kind contents_list option =
    function
    | Single (Manager_operation _ as c) -> begin
        match may_need_patching_single c with
        | None -> None
        | Some op -> Some (Single op)
      end
    | Single _ -> None
    | Cons (Manager_operation _ as c, rest) -> begin
        match may_need_patching_single c, may_need_patching rest with
        | None, None -> None
        | Some c, None -> Some (Cons (c, rest))
        | None, Some rest -> Some (Cons (c, rest))
        | Some c, Some rest -> Some (Cons (c, rest))
      end in

  let rec patch_fee :
    type kind. bool -> kind contents -> kind contents = fun first -> function
    | Manager_operation c as op ->
        let gas_limit = c.gas_limit in
        let size =
          if first then
            Data_encoding.Binary.fixed_length_exn
              Tezos_base.Operation.shell_header_encoding +
            Data_encoding.Binary.length
              Operation.contents_encoding
              (Contents op) +
            Signature.size
          else
            Data_encoding.Binary.length
              Operation.contents_encoding
              (Contents op)
        in
        let minimal_fees_in_nanotez =
          Z.mul (Z.of_int64 (Tez.to_mutez fee_parameter.minimal_fees)) (Z.of_int 1000) in
        let minimal_fees_for_gas_in_nanotez =
          Z.mul fee_parameter.minimal_nanotez_per_gas_unit gas_limit in
        let minimal_fees_for_size_in_nanotez =
          Z.mul fee_parameter.minimal_nanotez_per_byte (Z.of_int size) in
        let fees_in_nanotez =
          Z.add minimal_fees_in_nanotez @@
          Z.add minimal_fees_for_gas_in_nanotez minimal_fees_for_size_in_nanotez in
        begin match Tez.of_mutez (Z.to_int64 (Z.div (Z.add (Z.of_int 999) fees_in_nanotez) (Z.of_int 1000))) with
          | None -> assert false
          | Some fee ->
              if fee <= c.fee then
                op
              else
                patch_fee first (Manager_operation { c with fee })
        end
    | c -> c in

  let patch :
    type kind. bool -> kind contents * kind contents_result -> kind contents tzresult Lwt.t = fun first -> function
    | Manager_operation c, (Manager_operation_result _ as result) ->
        begin
          if c.gas_limit < Z.zero || gas_limit <= c.gas_limit then
            Lwt.return (estimated_gas_single result) >>=? fun gas ->
            begin
              if Z.equal gas Z.zero then
                cctxt#message "Estimated gas: none" >>= fun () ->
                return Z.zero
              else
                cctxt#message
                  "Estimated gas: %s units (will add 100 for safety)"
                  (Z.to_string gas) >>= fun () ->
                return (Z.min (Z.add gas (Z.of_int 100)) gas_limit)
            end
          else return c.gas_limit
        end >>=? fun gas_limit ->
        begin
          if c.storage_limit < Z.zero || storage_limit <= c.storage_limit then
            Lwt.return (estimated_storage_single (Z.of_int origination_size) result) >>=? fun storage ->
            begin
              if Z.equal storage Z.zero then
                cctxt#message "Estimated storage: no bytes added" >>= fun () ->
                return Z.zero
              else
                cctxt#message
                  "Estimated storage: %s bytes added (will add 20 for safety)"
                  (Z.to_string storage) >>= fun () ->
                return (Z.min (Z.add storage (Z.of_int 20)) storage_limit)
            end
          else return c.storage_limit
        end >>=? fun storage_limit ->
        let c = Manager_operation { c with gas_limit ; storage_limit } in
        if compute_fee then
          return (patch_fee first c)
        else
          return c
    | (c, _) -> return c in
  let rec patch_list :
    type kind. bool -> kind contents_and_result_list -> kind contents_list tzresult Lwt.t =
    fun first -> function
      | Single_and_result
          ((Manager_operation _ as op), (Manager_operation_result _ as res)) ->
          patch first (op, res) >>=? fun op -> return (Single op)
      | Single_and_result (op, _) -> return (Single op)
      | Cons_and_result ((Manager_operation _ as op),
                         (Manager_operation_result _ as res), rest) -> begin
          patch first (op, res) >>=? fun op ->
          patch_list false rest >>=? fun rest ->
          return (Cons (op, rest))
        end in
  match may_need_patching contents with
  | Some contents ->
      simulate cctxt ~chain ~block ?branch contents >>=? fun (_, _, result) ->
      begin match detect_script_failure result with
        | Ok () -> return_unit
        | Error _ ->
            cctxt#message
              "@[<v 2>This simulation failed:@,%a@]"
              Operation_result.pp_operation_result
              (contents, result.contents) >>= fun () ->
            return_unit
      end >>=? fun () ->
      begin
        Lwt.return (estimated_storage (Z.of_int origination_size) result.contents) >>=? fun storage ->
        Lwt.return (Alpha_environment.wrap_error Tez.(cost_per_byte *? Z.to_int64 storage)) >>=? fun burn ->
        if Tez.(burn > fee_parameter.burn_cap) then
          cctxt#error "The operation will burn %s%a which is higher than the configured burn cap (%s%a).@\n\
                      \ Use `--burn-cap %a` to emit this operation."
            Client_proto_args.tez_sym Tez.pp burn
            Client_proto_args.tez_sym Tez.pp fee_parameter.burn_cap
            Tez.pp burn >>= fun () ->
          exit 1
        else
          return_unit
      end >>=? fun () ->
      let res = pack_contents_list contents result.contents in
      patch_list true res
  | None -> return contents

let inject_operation
    (type kind) cctxt ~chain ~block
    ?confirmations
    ?(dry_run = false)
    ?branch ?src_sk
    ?verbose_signing
    ~fee_parameter
    ?compute_fee
    (contents: kind contents_list)  =
  Tezos_client_base_unix.Client_confirmations_unix.wait_for_bootstrapped cctxt >>=? fun () ->
  may_patch_limits
    cctxt ~chain ~block ?branch
    ~fee_parameter
    ?compute_fee
    contents >>=? fun contents ->
  preapply cctxt ~chain ~block ~fee_parameter
    ?verbose_signing ?branch ?src_sk contents >>=? fun (_oph, op, result) ->
  begin match detect_script_failure result with
    | Ok () -> return_unit
    | Error _ as res ->
        cctxt#message
          "@[<v 2>This simulation failed:@,%a@]"
          Operation_result.pp_operation_result
          (op.protocol_data.contents, result.contents) >>= fun () ->
        Lwt.return res
  end >>=? fun () ->
  let bytes =
    Data_encoding.Binary.to_bytes_exn
      Operation.encoding (Operation.pack op) in
  if dry_run then
    let oph = Operation_hash.hash_bytes [bytes] in
    cctxt#message
      "@[<v 0>Operation: 0x%a@,\
       Operation hash is '%a'@]"
      MBytes.pp_hex bytes
      Operation_hash.pp oph >>= fun () ->
    cctxt#message
      "@[<v 2>Simulation result:@,%a@]"
      Operation_result.pp_operation_result
      (op.protocol_data.contents, result.contents) >>= fun () ->
    return (oph, op.protocol_data.contents, result.contents)
  else
    Shell_services.Injection.operation cctxt ~chain bytes >>=? fun oph ->
    cctxt#message "Operation successfully injected in the node." >>= fun () ->
    cctxt#message "Operation hash is '%a'" Operation_hash.pp oph >>= fun () ->
    begin
      match confirmations with
      | None ->
          cctxt#message "@[<v 0>NOT waiting for the operation to be included.@,\
                         Use command@,\
                        \  tezos-client wait for %a to be included --confirmations 30 --branch %a@,\
                         and/or an external block explorer to make sure that it has been included.@]"
            Operation_hash.pp oph Block_hash.pp op.shell.branch >>= fun () ->
          return result
      | Some confirmations ->
          cctxt#message "Waiting for the operation to be included..." >>= fun () ->
          Client_confirmations.wait_for_operation_inclusion
            ~branch:op.shell.branch ~confirmations cctxt ~chain oph >>=? fun (h, i , j) ->
          Alpha_block_services.Operations.operation
            cctxt ~chain ~block:(`Hash (h, 0)) i j >>=? fun op' ->
          match op'.receipt with
          | No_operation_metadata ->
              failwith "Internal error: unexpected receipt."
          | Operation_metadata receipt ->
              match Apply_results.kind_equal_list contents receipt.contents
              with
              | Some Apply_results.Eq ->
                  return (receipt : kind operation_metadata)
              | None -> failwith "Internal error: unexpected receipt."
    end >>=? fun result ->
    cctxt#message
      "@[<v 2>This sequence of operations was run:@,%a@]"
      Operation_result.pp_operation_result
      (op.protocol_data.contents, result.contents) >>= fun () ->
    Lwt.return (originated_contracts result.contents) >>=? fun contracts ->
    Lwt_list.iter_s
      (fun c ->
         cctxt#message
           "New contract %a originated."
           Contract.pp c)
      contracts >>= fun () ->
    begin match confirmations with
      | None -> Lwt.return_unit
      | Some number ->
          if number >= 30 then
            cctxt#message
              "The operation was included in a block %d blocks ago."
              number
          else
            cctxt#message
              "@[<v 0>The operation has only been included %d blocks ago.@,\
               We recommend to wait more.@,\
               Use command@,\
              \  tezos-client wait for %a to be included --confirmations 30 \
               --branch %a@,\
               and/or an external block explorer.@]"
              number Operation_hash.pp oph Block_hash.pp op.shell.branch
    end >>= fun () ->
    return (oph, op.protocol_data.contents, result.contents)


let inject_manager_operation
    cctxt ~chain ~block ?branch ?confirmations ?dry_run ?verbose_signing
    ~source ~src_pk ~src_sk ?fee ?(gas_limit = Z.minus_one) ?(storage_limit = (Z.of_int (-1))) ?counter ~fee_parameter
    (type kind) (operation : kind manager_operation)
  : (Operation_hash.t * kind Kind.manager contents *  kind Kind.manager contents_result) tzresult Lwt.t =
  begin
    match counter with
    | None ->
        Alpha_services.Contract.counter
          cctxt (chain, block) source >>=? fun pcounter ->
        let counter = Z.succ pcounter in
        return counter
    | Some counter ->
        return counter
  end >>=? fun counter ->
  Alpha_services.Contract.manager_key
    cctxt (chain, block) source >>=? fun (_, key) ->
  let is_reveal : type kind. kind manager_operation -> bool = function
    | Reveal _ -> true
    | _ -> false in
  let compute_fee, fee =
    match fee with
    | None -> true, Tez.zero
    | Some fee -> false, fee in
  match key with
  | None when not (is_reveal operation) -> begin
      let contents =
        Cons
          (Manager_operation { source ; fee = Tez.zero ; counter ;
                               gas_limit = Z.of_int 10_000 ; storage_limit = Z.zero ;
                               operation = Reveal src_pk },
           Single (Manager_operation { source ; fee ; counter = Z.succ counter ;
                                       gas_limit ; storage_limit ; operation })) in
      inject_operation cctxt ~chain ~block ?confirmations ?dry_run
        ~fee_parameter ~compute_fee
        ?verbose_signing ?branch ~src_sk contents >>=? fun (oph, op, result) ->
      match pack_contents_list op result with
      | Cons_and_result (_, _, Single_and_result (op, result)) ->
          return (oph, op, result)
      | Single_and_result (Manager_operation _, _) -> .
      | _ -> assert false (* Grrr... *)
    end
  | _ ->
      let contents =
        Single (Manager_operation { source ; fee ; counter ;
                                    gas_limit ; storage_limit ; operation }) in
      inject_operation cctxt ~chain ~block ?confirmations ?dry_run ?verbose_signing
        ~compute_fee ~fee_parameter ?branch ~src_sk contents >>=? fun (oph, op, result) ->
      match pack_contents_list op result with
      | Single_and_result (Manager_operation _ as op, result) ->
          return (oph, op, result)
      | _ -> assert false (* Grrr... *)
