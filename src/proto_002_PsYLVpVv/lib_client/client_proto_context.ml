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

open Proto_alpha
open Alpha_context
open Tezos_micheline
open Client_proto_contracts
open Client_keys

let get_balance (rpc : #Proto_alpha.rpc_context) ~chain ~block contract =
  Alpha_services.Contract.balance rpc (chain, block) contract

let get_storage (rpc : #Proto_alpha.rpc_context) ~chain ~block contract =
  Alpha_services.Contract.storage_opt rpc (chain, block) contract

let get_big_map_value (rpc : #Proto_alpha.rpc_context) ~chain ~block contract key =
  Alpha_services.Contract.big_map_get_opt rpc (chain, block) contract key

let get_script (rpc : #Proto_alpha.rpc_context) ~chain ~block contract =
  Alpha_services.Contract.script_opt rpc (chain, block) contract

let parse_expression arg =
  Lwt.return
    (Micheline_parser.no_parsing_error
       (Michelson_v1_parser.parse_expression arg))

let transfer (cctxt : #Proto_alpha.full)
    ~chain ~block ?confirmations
    ?dry_run
    ?branch ~source ~src_pk ~src_sk ~destination ?arg
    ~amount ~fee ?gas_limit ?storage_limit ?counter () =
  begin match arg with
    | Some arg ->
        parse_expression arg >>=? fun { expanded = arg } ->
        return_some arg
    | None -> return_none
  end >>=? fun parameters ->
  let parameters = Option.map ~f:Script.lazy_expr parameters in
  let contents = Transaction { amount ; parameters ; destination } in
  Injection.inject_manager_operation
    cctxt ~chain ~block ?confirmations
    ?dry_run
    ?branch ~source ~fee ?gas_limit ?storage_limit ?counter
    ~src_pk ~src_sk contents >>=? fun (_oph, _op, result as res) ->
  Lwt.return
    (Injection.originated_contracts (Single_result result)) >>=? fun contracts ->
  return (res, contracts)

let reveal cctxt
    ~chain ~block ?confirmations
    ?dry_run
    ?branch ~source ~src_pk ~src_sk ~fee () =
  Alpha_services.Contract.counter
    cctxt (chain, block) source >>=? fun pcounter ->
  let counter = Z.succ pcounter in
  Alpha_services.Contract.manager_key
    cctxt (chain, block) source >>=? fun (_, key) ->
  match key with
  | Some _ ->
      failwith "The manager key was previously revealed."
  | None -> begin
      let contents =
        Single
          (Manager_operation { source ; fee ; counter ;
                               gas_limit = Z.zero ; storage_limit = Z.zero ;
                               operation = Reveal src_pk }) in
      Injection.inject_operation cctxt ~chain ~block ?confirmations
        ?dry_run
        ?branch ~src_sk contents >>=? fun (oph, op, result) ->
      match Apply_results.pack_contents_list op result with
      | Apply_results.Single_and_result
          (Manager_operation _ as op, result) ->
          return (oph, op, result)
    end

let originate
    cctxt ~chain ~block ?confirmations
    ?dry_run
    ?branch ~source ~src_pk ~src_sk ~fee
    ?gas_limit ?storage_limit contents =
  Injection.inject_manager_operation
    cctxt ~chain ~block ?confirmations
    ?dry_run
    ?branch ~source ~fee ?gas_limit ?storage_limit
    ~src_pk ~src_sk contents >>=? fun (_oph, _op, result as res) ->
  Lwt.return
    (Injection.originated_contracts (Single_result result)) >>=? function
  | [ contract ] -> return (res, contract)
  | contracts ->
      failwith
        "The origination introduced %d contracts instead of one."
        (List.length contracts)

let originate_account
    cctxt ~chain ~block ?confirmations
    ?dry_run
    ?branch ~source ~src_pk ~src_sk ~manager_pkh
    ?(delegatable = false) ?delegate ~balance ~fee () =
  let origination =
    Origination { manager = manager_pkh ;
                  delegate ;
                  script = None ;
                  spendable = true ;
                  delegatable ;
                  credit = balance ;
                  preorigination = None } in
  originate
    cctxt ~chain ~block ?confirmations
    ?dry_run
    ?branch ~source ~gas_limit:Z.zero ~src_pk ~src_sk ~fee origination

let delegate_contract cctxt
    ~chain ~block ?branch ?confirmations
    ?dry_run
    ~source ~src_pk ~src_sk
    ~fee delegate_opt =
  let operation = Delegation delegate_opt in
  Injection.inject_manager_operation
    cctxt ~chain ~block ?confirmations
    ?dry_run
    ?branch ~source ~fee ~gas_limit:Z.zero ~storage_limit:Z.zero
    ~src_pk ~src_sk operation >>=? fun res ->
  return res

let list_contract_labels
    (cctxt : #Proto_alpha.full)
    ~chain ~block =
  Alpha_services.Contract.list cctxt (chain, block) >>=? fun contracts ->
  map_s (fun h ->
      begin match Contract.is_implicit h with
        | Some m -> begin
            Public_key_hash.rev_find cctxt m >>=? function
            | None -> return ""
            | Some nm ->
                RawContractAlias.find_opt cctxt nm >>=? function
                | None -> return (" (known as " ^ nm ^ ")")
                | Some _ -> return (" (known as key:" ^ nm ^ ")")
          end
        | None -> begin
            RawContractAlias.rev_find cctxt h >>=? function
            | None -> return ""
            | Some nm ->  return (" (known as " ^ nm ^ ")")
          end
      end >>=? fun nm ->
      let kind = match Contract.is_implicit h with
        | Some _ -> " (implicit)"
        | None -> "" in
      let h_b58 = Contract.to_b58check h in
      return (nm, h_b58, kind))
    contracts

let message_added_contract (cctxt : #Proto_alpha.full) name =
  cctxt#message "Contract memorized as %s." name

let get_manager
    (cctxt : #Proto_alpha.full)
    ~chain ~block source =
  Client_proto_contracts.get_manager
    cctxt ~chain ~block source >>=? fun src_pkh ->
  Client_keys.get_key cctxt src_pkh >>=? fun (src_name, src_pk, src_sk) ->
  return (src_name, src_pkh, src_pk, src_sk)

let set_delegate
    cctxt ~chain ~block ?confirmations
    ?dry_run
    ~fee contract ~src_pk ~manager_sk opt_delegate =
  delegate_contract
    cctxt ~chain ~block ?confirmations
    ?dry_run
    ~source:contract ~src_pk ~src_sk:manager_sk ~fee opt_delegate

let register_as_delegate
    cctxt ~chain ~block ?confirmations
    ?dry_run
    ~fee ~manager_sk src_pk =
  let source = Signature.Public_key.hash src_pk in
  delegate_contract
    cctxt ~chain ~block ?confirmations
    ?dry_run
    ~source:(Contract.implicit_contract source) ~src_pk ~src_sk:manager_sk ~fee
    (Some source)

let source_to_keys (wallet : #Proto_alpha.full) ~chain ~block source =
  get_manager
    wallet ~chain ~block
    source >>=? fun (_src_name, _src_pkh, src_pk, src_sk) ->
  return (src_pk, src_sk)

let save_contract ~force cctxt alias_name contract =
  RawContractAlias.add ~force cctxt alias_name contract >>=? fun () ->
  message_added_contract cctxt alias_name >>= fun () ->
  return_unit

let originate_contract
    (cctxt : #Proto_alpha.full)
    ~chain ~block ?confirmations
    ?dry_run
    ?branch
    ~fee
    ?gas_limit
    ?storage_limit
    ~delegate
    ?(delegatable=true)
    ?(spendable=false)
    ~initial_storage
    ~manager
    ~balance
    ~source
    ~src_pk
    ~src_sk
    ~code
    () =
  Lwt.return (Michelson_v1_parser.parse_expression initial_storage) >>= fun result ->
  Lwt.return (Micheline_parser.no_parsing_error result) >>=?
  fun { Michelson_v1_parser.expanded = storage } ->
  let code = Script.lazy_expr code and storage = Script.lazy_expr storage in
  let origination =
    Origination { manager ;
                  delegate ;
                  script = Some { code ; storage } ;
                  spendable ;
                  delegatable ;
                  credit = balance ;
                  preorigination = None } in
  originate cctxt ~chain ~block ?confirmations
    ?dry_run
    ?branch ~source ~src_pk ~src_sk ~fee ?gas_limit ?storage_limit origination

type activation_key =
  { pkh : Ed25519.Public_key_hash.t ;
    amount : Tez.t ;
    activation_code : Blinded_public_key_hash.activation_code ;
    mnemonic : string list ;
    password : string ;
    email : string ;
  }

let raw_activation_key_encoding =
  let open Data_encoding in
  obj6
    (req "pkh" Ed25519.Public_key_hash.encoding)
    (req "amount" Tez.encoding)
    (req "activation_code" Blinded_public_key_hash.activation_code_encoding)
    (req "mnemonic" (list string))
    (req "password" string)
    (req "email" string)

let activation_key_encoding =
  (* Hack: allow compatibility with older encoding *)
  let open Data_encoding in
  conv
    (fun { pkh ; amount ; activation_code ; mnemonic ; password ; email } ->
       ( pkh, amount, activation_code, mnemonic, password, email ))
    (fun ( pkh, amount, activation_code, mnemonic, password, email ) ->
       { pkh ; amount ; activation_code ; mnemonic ; password ; email }) @@
  splitted
    ~binary:raw_activation_key_encoding
    ~json:
      (union [
          case
            ~title:"Activation"
            Json_only
            raw_activation_key_encoding
            (fun x -> Some x)
            (fun x -> x) ;
          case
            ~title:"Deprecated_activation"
            Json_only
            (obj6
               (req "pkh" Ed25519.Public_key_hash.encoding)
               (req "amount" Tez.encoding)
               (req "secret" Blinded_public_key_hash.activation_code_encoding)
               (req "mnemonic" (list string))
               (req "password" string)
               (req "email" string))
            (fun _ -> None)
            (fun x -> x) ;
        ])

let read_key key =
  match Bip39.of_words key.mnemonic with
  | None ->
      failwith ""
  | Some t ->
      (* TODO: unicode normalization (NFKD)... *)
      let passphrase = MBytes.(concat "" [of_string key.email ; of_string key.password]) in
      let sk = Bip39.to_seed ~passphrase t in
      let sk = MBytes.sub sk 0 32 in
      let sk : Signature.Secret_key.t =
        Ed25519 (Data_encoding.Binary.of_bytes_exn Ed25519.Secret_key.encoding sk) in
      let pk = Signature.Secret_key.to_public_key sk in
      let pkh = Signature.Public_key.hash pk in
      return (pkh, pk, sk)

let inject_activate_operation
    cctxt ~chain ~block ?confirmations
    ?dry_run
    alias pkh activation_code =
  let contents =
    Single ( Activate_account { id = pkh ; activation_code } ) in
  Injection.inject_operation
    cctxt ?confirmations
    ?dry_run
    ~chain ~block
    contents >>=? fun (oph, op, result) ->
  begin
    match confirmations with
    | None ->
        return_unit
    | Some _confirmations ->
        Alpha_services.Contract.balance
          cctxt (`Main, `Head 0)
          (Contract.implicit_contract (Ed25519 pkh)) >>=? fun balance ->
        cctxt#message "Account %s (%a) activated with %s%a."
          alias
          Ed25519.Public_key_hash.pp pkh
          Client_proto_args.tez_sym
          Tez.pp balance >>= fun () ->
        return_unit
  end >>=? fun () ->
  match Apply_results.pack_contents_list op result with
  | Apply_results.Single_and_result
      (Activate_account _ as op, result) ->
      return (oph, op, result)

let activate_account
    (cctxt : #Proto_alpha.full)
    ~chain ~block ?confirmations
    ?dry_run
    ?(encrypted = false) ?force key name =
  read_key key >>=? fun (pkh, pk, sk) ->
  fail_unless (Signature.Public_key_hash.equal pkh (Ed25519 key.pkh))
    (failure "@[<v 2>Inconsistent activation key:@ \
              Computed pkh: %a@ \
              Embedded pkh: %a @]"
       Signature.Public_key_hash.pp pkh
       Ed25519.Public_key_hash.pp key.pkh) >>=? fun () ->
  let pk_uri = Tezos_signer_backends.Unencrypted.make_pk pk in
  begin
    if encrypted then
      Tezos_signer_backends.Encrypted.encrypt cctxt sk
    else
      return (Tezos_signer_backends.Unencrypted.make_sk sk)
  end >>=? fun sk_uri ->
  Client_keys.register_key cctxt ?force (pkh, pk_uri, sk_uri) name >>=? fun () ->
  inject_activate_operation cctxt
    ~chain ~block ?confirmations
    ?dry_run
    name key.pkh key.activation_code

let activate_existing_account
    (cctxt : #Proto_alpha.full)
    ~chain ~block ?confirmations
    ?dry_run
    alias activation_code =
  Client_keys.alias_keys cctxt alias >>=? function
  | Some (Ed25519 pkh, _, _) ->
      inject_activate_operation
        cctxt ~chain ~block ?confirmations
        ?dry_run
        alias pkh activation_code
  | Some _ -> failwith "Only Ed25519 accounts can be activated"
  | None -> failwith "Unknown account"

let pp_operation formatter (a : Alpha_block_services.operation) =
  match a.receipt, a.protocol_data with
  | Apply_results.Operation_metadata omd, Operation_data od -> (
      match Apply_results.kind_equal_list od.contents omd.contents
      with
      | Some Apply_results.Eq ->
          Operation_result.pp_operation_result formatter 
            (od.contents, omd.contents) 
      | None -> Pervasives.failwith "Unexpected result.")
  | _ -> Pervasives.failwith "Unexpected result."

let get_operation_from_block
    (cctxt : #Client_context.full)
    ~chain
    predecessors
    operation_hash =
  Client_confirmations.lookup_operation_in_previous_blocks
    cctxt 
    ~chain 
    ~predecessors 
    operation_hash
  >>=? function
  | None -> return_none
  | Some (block, i, j) -> 
      cctxt#message "Operation found in block: %a (pass: %d, offset: %d)"
        Block_hash.pp block i j >>= fun () ->
      Proto_alpha.Alpha_block_services.Operations.operation cctxt 
        ~block:(`Hash (block, 0)) i j >>=? fun op' -> return_some op'

let display_receipt_for_operation
    (cctxt : #Proto_alpha.full)
    ~chain
    ?(predecessors = 10)
    operation_hash =
  get_operation_from_block cctxt ~chain predecessors operation_hash
  >>=? function 
  | None -> 
      cctxt#message "Couldn't find operation" >>= fun () -> 
      return_unit
  | Some op -> 
      cctxt#message "%a" pp_operation op >>= fun () ->
      return_unit
