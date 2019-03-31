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

let () = Random.self_init ()

let rpc_config = ref {
    RPC_client.host = "localhost" ;
    port = 8192 + Random.int 8192 ;
    tls = false ;
    logger = RPC_client.null_logger ;
  }

let build_rpc_context config =
  new Proto_alpha.wrap_proto_context @@
  new RPC_client.http_ctxt config Media_type.all_media_types

let rpc_ctxt = ref (build_rpc_context !rpc_config)

(* Context that does not write to alias files *)
let no_write_context ?(chain = `Main) ?(block = `Head 0) config : #Client_context.full = object
  inherit RPC_client.http_ctxt config Media_type.all_media_types
  inherit Client_context.simple_printer (fun _ _ -> Lwt.return_unit)
  method load : type a. string -> default:a -> a Data_encoding.encoding -> a Error_monad.tzresult Lwt.t =
    fun _ ~default _ -> return default
  method write : type a. string ->
    a ->
    a Data_encoding.encoding -> unit Error_monad.tzresult Lwt.t =
    fun _ _ _ -> return_unit
  method with_lock : type a. (unit -> a Lwt.t) -> a Lwt.t = fun f -> f ()
  method chain = chain
  method block = block
  method confirmations = None
  method prompt : type a. (a, string tzresult) Client_context.lwt_format -> a =
    Format.kasprintf (fun _ -> return "")
  method prompt_password : type a. (a, MBytes.t tzresult) Client_context.lwt_format -> a =
    Format.kasprintf (fun _ -> return (MBytes.of_string ""))
  method load_passwords = None
  method read_file path =
    Lwt.catch
      (fun () ->
         Lwt_io.(with_file ~mode:Input path read) >>= fun content ->
         return content)
      (fun exn ->
         failwith
           "cannot read file (%s)" (Printexc.to_string exn))
  method sleep = Lwt_unix.sleep
end

let sandbox_parameters =
  let json_result =
    Data_encoding.Json.from_string {json|
{ "genesis_pubkey": "edpkuSLWfVU1Vq7Jg9FucPyKmma6otcMHac9zG4oU1KMHSTBpJuGQ2" }
|json} in
  match json_result with
  | Error err -> raise (Failure err)
  | Ok json -> json

let protocol_parameters =
  let json_result =
    Data_encoding.Json.from_string {json|
{ "bootstrap_accounts": [
    [ "edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav", "4000000000000" ],
    [ "edpktzNbDAUjUk697W7gYg2CRuBQjyPxbEg8dLccYYwKSKvkPvjtV9", "4000000000000" ],
    [ "edpkuTXkJDGcFd5nh6VvMz8phXxU3Bi7h6hqgywNFi1vZTfQNnS1RV", "4000000000000" ],
    [ "edpkuFrRoDSEbJYgxRtLx2ps82UdaYc1WwfS9sE11yhauZt5DgCHbU", "4000000000000" ],
    [ "edpkv8EUUH68jmo3f7Um5PezmfGrRF24gnfLpH3sVNwJnV5bVCxL2n", "4000000000000" ]
  ],
  "commitments": [
    [ "btz1bRL4X5BWo2Fj4EsBdUwexXqgTf75uf1qa", "23932454669343" ],
    [ "btz1SxjV1syBgftgKy721czKi3arVkVwYUFSv", "72954577464032" ],
    [ "btz1LtoNCjiW23txBTenALaf5H6NKF1L3c1gw", "217487035428348" ],
    [ "btz1SUd3mMhEBcWudrn8u361MVAec4WYCcFoy", "4092742372031" ],
    [ "btz1MvBXf4orko1tsGmzkjLbpYSgnwUjEe81r", "17590039016550" ],
    [ "btz1LoDZ3zsjgG3k3cqTpUMc9bsXbchu9qMXT", "26322312350555" ],
    [ "btz1RMfq456hFV5AeDiZcQuZhoMv2dMpb9hpP", "244951387881443" ],
    [ "btz1Y9roTh4A7PsMBkp8AgdVFrqUDNaBE59y1", "80065050465525" ],
    [ "btz1Q1N2ePwhVw5ED3aaRVek6EBzYs1GDkSVD", "3569618927693" ],
    [ "btz1VFFVsVMYHd5WfaDTAt92BeQYGK8Ri4eLy", "9034781424478" ]
  ],
  "time_between_blocks" : [ "1", "0" ],
  "blocks_per_cycle" : 4,
  "blocks_per_roll_snapshot" : 2,
  "preserved_cycles" : 1,
  "proof_of_work_threshold": "-1"
}
|json} in
  match json_result with
  | Error err -> raise (Failure err)
  | Ok json ->
      Data_encoding.Binary.to_bytes_exn Data_encoding.json json

let vote_protocol_parameters =
  let json_result =
    Data_encoding.Json.from_string {json|
{ "bootstrap_accounts": [
    [ "edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav", "4000000000000" ],
    [ "edpktzNbDAUjUk697W7gYg2CRuBQjyPxbEg8dLccYYwKSKvkPvjtV9", "4000000000000" ],
    [ "edpkuTXkJDGcFd5nh6VvMz8phXxU3Bi7h6hqgywNFi1vZTfQNnS1RV", "4000000000000" ],
    [ "edpkuFrRoDSEbJYgxRtLx2ps82UdaYc1WwfS9sE11yhauZt5DgCHbU", "4000000000000" ],
    [ "edpkv8EUUH68jmo3f7Um5PezmfGrRF24gnfLpH3sVNwJnV5bVCxL2n", "4000000000000" ]
  ],
  "time_between_blocks" : [ "1", "0" ],
  "blocks_per_cycle" : 4,
  "blocks_per_roll_snapshot" : 2,
  "preserved_cycles" : 1,
  "blocks_per_voting_period": 2,
  "proof_of_work_threshold": "-1"
}
|json} in
  match json_result with
  | Error err -> raise (Failure err)
  | Ok json ->
      Data_encoding.Binary.to_bytes_exn Data_encoding.json json

let activate_alpha ?(vote = false) () =
  let fitness = Fitness_repr.from_int64 0L in
  let activator_sk =
    Tezos_signer_backends.Unencrypted.make_sk
      (Signature.Secret_key.of_b58check_exn
         "edsk31vznjHSSpGExDMHYASz45VZqXN4DPxvsa4hAyY8dHM28cZzp6") in
  let protocol_parameters =
    if vote then vote_protocol_parameters else protocol_parameters in
  Tezos_client_genesis.Client_proto_main.bake
    (no_write_context ~block:(`Head 0) !rpc_config) (`Head 0)
    (Activate  { protocol = Proto_alpha.hash ;
                 fitness ;
                 protocol_parameters ;
               })
    activator_sk

let init ?exe ?vote ?rpc_port () =
  begin
    match rpc_port with
    | None -> ()
    | Some port ->
        rpc_config := { !rpc_config with port } ;
        rpc_ctxt := build_rpc_context !rpc_config ;
  end ;
  let pid =
    Node_helpers.fork_node
      ?exe
      ~port:!rpc_config.port
      ~sandbox:sandbox_parameters
      () in
  activate_alpha ?vote () >>=? fun hash ->
  return (pid, hash)

let level (chain, block) =
  Alpha_block_services.metadata
    !rpc_ctxt ~chain ~block () >>=? fun { protocol_data = { level ; _ } ; _ } ->
  return level

let rpc_raw_context block path depth =
  Shell_services.Blocks.Context.read !rpc_ctxt ~block ~depth path

module Account = struct

  type t = {
    alias : string ;
    sk : Signature.secret_key ;
    pk : Signature.public_key ;
    pkh : Signature.public_key_hash ;
    contract : Contract.t ;
  }

  let encoding =
    let open Data_encoding in
    conv
      (fun { alias ; sk ; pk ; pkh ; contract } ->
         (alias, sk, pk, pkh, contract)
      )
      (fun (alias, sk, pk, pkh, contract) ->
         { alias ; sk ; pk ; pkh ; contract })
      (obj5
         (req "alias" string)
         (req "sk" Signature.Secret_key.encoding)
         (req "pk" Signature.Public_key.encoding)
         (req "pkh" Signature.Public_key_hash.encoding)
         (req "contract" Contract.encoding))

  let pp_account ppf account =
    let json = Data_encoding.Json.construct encoding account in
    Format.fprintf ppf "%s" (Data_encoding.Json.to_string json)

  let create ?keys alias =
    let sk, pk = match keys with
      | Some keys -> keys
      | None -> let _, pk, sk = Signature.generate_key () in sk, pk in
    let pkh = Signature.Public_key.hash pk in
    let contract = Contract.implicit_contract pkh in
    { alias ; contract ; pkh ; pk ; sk }

  type destination = {
    alias : string ;
    contract : Contract.t ;
    pk : public_key ;
    pkh : public_key_hash ;
  }

  let destination_encoding =
    let open Data_encoding in
    conv
      (fun { alias ; pk ; pkh ; contract } ->
         (alias, pk, pkh, contract))
      (fun (alias, pk, pkh, contract) ->
         { alias ; pk ; pkh ; contract })
      (obj4
         (req "alias" string)
         (req "pk" Signature.Public_key.encoding)
         (req "pkh" Signature.Public_key_hash.encoding)
         (req "contract" Contract.encoding))

  let pp_destination ppf destination =
    let json = Data_encoding.Json.construct destination_encoding destination in
    Format.fprintf ppf "%s" (Data_encoding.Json.to_string json)

  let create_destination ~alias ~contract ~pk =
    let pkh = Signature.Public_key.hash pk in
    { alias ; contract ; pk ; pkh }

  type bootstrap_accounts = { b1 : t ; b2 : t ; b3 : t ; b4 : t ;  b5 : t  ; }

  let bootstrap_accounts =
    let bootstrap1_sk =
      "edsk3gUfUPyBSfrS9CCgmCiQsTCHGkviBDusMxDJstFtojtc1zcpsh" in
    let bootstrap2_sk =
      "edsk39qAm1fiMjgmPkw1EgQYkMzkJezLNewd7PLNHTkr6w9XA2zdfo" in
    let bootstrap3_sk =
      "edsk4ArLQgBTLWG5FJmnGnT689VKoqhXwmDPBuGx3z4cvwU9MmrPZZ" in
    let bootstrap4_sk =
      "edsk2uqQB9AY4FvioK2YMdfmyMrer5R8mGFyuaLLFfSRo8EoyNdht3" in
    let bootstrap5_sk =
      "edsk4QLrcijEffxV31gGdN2HU7UpyJjA8drFoNcmnB28n89YjPNRFm" in
    let cpt = ref 0 in
    match List.map begin fun sk ->
        incr cpt ;
        let sk = Signature.Secret_key.of_b58check_exn sk in
        let alias = Printf.sprintf "bootstrap%d" !cpt in
        let pk = Signature.Secret_key.to_public_key sk in
        let pkh = Signature.Public_key.hash pk in
        { alias ; contract = Contract.implicit_contract pkh; pkh ; pk ; sk }
      end [ bootstrap1_sk; bootstrap2_sk; bootstrap3_sk;
            bootstrap4_sk; bootstrap5_sk; ]
    with
    | [ b1 ; b2 ; b3 ; b4 ; b5 ] -> { b1 ; b2 ; b3 ; b4 ; b5 }
    | _ -> assert false

  let transfer
      ?(block = `Head 0)
      ?(fee = Tez.fifty_cents)
      ~(account:t)
      ~destination
      ~amount
      ?(fee_parameter = Injection.dummy_fee_parameter)
      () =
    let src_sk =
      Tezos_signer_backends.Unencrypted.make_sk account.sk in
    Client_proto_context.transfer
      (new wrap_full (no_write_context !rpc_config ~block))
      ~chain:`Main
      ~block
      ~source:account.contract
      ~src_pk:account.pk
      ~src_sk
      ~destination
      ~amount
      ~fee
      ~fee_parameter
      () >>=? fun ((oph, _, _), contracts) ->
    return (oph, contracts)

  let originate
      ?(block = `Head 0)
      ?delegate
      ?(fee = Tez.fifty_cents)
      ~(src:t)
      ~manager_pkh
      ~balance
      ?(fee_parameter = Injection.dummy_fee_parameter)
      () =
    let delegatable, delegate = match delegate with
      | None -> false, None
      | Some delegate -> true, Some delegate in
    let src_sk =
      Tezos_signer_backends.Unencrypted.make_sk src.sk in
    Client_proto_context.originate_account
      (new wrap_full (no_write_context !rpc_config))
      ~chain:`Main
      ~block
      ~source:src.contract
      ~src_pk:src.pk
      ~src_sk
      ~manager_pkh
      ~balance
      ~delegatable
      ?delegate
      ~fee
      ~fee_parameter
      () >>=? fun ((oph, _, _), contracts) ->
    return (oph, contracts)

  let set_delegate
      ?(block = `Head 0)
      ?(fee = Tez.fifty_cents)
      ~contract
      ~manager_sk
      ~src_pk
      ?(fee_parameter = Injection.dummy_fee_parameter)
      delegate_opt =
    Client_proto_context.set_delegate
      (new wrap_full (no_write_context ~block !rpc_config))
      ~chain:`Main
      ~block
      ~fee
      contract
      ~src_pk
      ~manager_sk
      ~fee_parameter
      delegate_opt >>=? fun (oph, _, _) ->
    return oph

  let balance ?(block = `Head 0) (account : t) =
    Alpha_services.Contract.balance !rpc_ctxt
      (`Main, block) account.contract

  (* TODO: gather contract related functions in a Contract module? *)
  let delegate ?(block = `Head 0) (contract : Contract.t) =
    Alpha_services.Contract.delegate_opt !rpc_ctxt (`Main, block) contract

end

let sign ?watermark src_sk shell (Contents_list contents) =
  let bytes =
    Data_encoding.Binary.to_bytes_exn
      Operation.unsigned_encoding
      (shell, (Contents_list contents)) in
  let signature = Some (Signature.sign ?watermark src_sk bytes) in
  let protocol_data = Operation_data { contents ; signature } in
  return { shell ; protocol_data }

module Protocol = struct

  open Account

  let proposals ?(block = `Head 0) ~src:({ pkh; sk ; _ } : Account.t) proposals =
    Shell_services.Blocks.hash !rpc_ctxt ~block () >>=? fun hash ->
    Alpha_services.Helpers.current_level
      !rpc_ctxt ~offset:1l (`Main, block) >>=? fun next_level ->
    let shell = { Tezos_base.Operation.branch = hash } in
    let contents =
      Proposals { source = pkh ;
                  period = next_level.voting_period ;
                  proposals } in
    sign ~watermark:Generic_operation sk shell (Contents_list (Single contents))

  let ballot ?(block = `Head 0) ~src:({ pkh; sk ; _ } : Account.t) ~proposal ballot =
    Shell_services.Blocks.hash !rpc_ctxt ~block () >>=? fun hash ->
    Alpha_services.Helpers.current_level
      !rpc_ctxt ~offset:1l (`Main, block) >>=? fun next_level ->
    let shell = { Tezos_base.Operation.branch = hash } in
    let contents =
      Single
        (Ballot { source = pkh ;
                  period = next_level.voting_period ;
                  proposal ;
                  ballot }) in
    sign ~watermark:Generic_operation sk shell (Contents_list contents)

end

module Assert = struct

  let fail expected given msg =
    Format.kasprintf Pervasives.failwith
      "@[%s@ expected: %s@ got: %s@]" msg expected given
  let fail_msg fmt = Format.kasprintf (fail "" "") fmt

  let default_printer _ = ""

  let equal ?(eq=(=)) ?(prn=default_printer) ?(msg="") x y =
    if not (eq x y) then fail (prn x) (prn y) msg
  let make_equal e p = equal ~eq:e ~prn:p
  let equal_bool = make_equal (=) string_of_bool
  let equal_int = make_equal (=) string_of_int

  let equal_pkh ?msg pkh1 pkh2 =
    let eq pkh1 pkh2 =
      match pkh1, pkh2 with
      | None, None -> true
      | Some pkh1, Some pkh2 ->
          Signature.Public_key_hash.equal pkh1 pkh2
      | _ -> false in
    let prn = function
      | None -> "none"
      | Some pkh -> Signature.Public_key_hash.to_b58check pkh in
    equal ?msg ~prn ~eq pkh1 pkh2

  let equal_tez ?msg tz1 tz2 =
    let eq tz1 tz2 = Int64.equal (Tez.to_mutez tz1) (Tez.to_mutez tz2) in
    let prn = Tez.to_string in
    equal ?msg ~prn ~eq tz1 tz2

  let balance_equal ?block ~msg account expected_balance =
    Account.balance ?block account >>=? fun actual_balance ->
    match Tez.of_mutez expected_balance with
    | None ->
        failwith "invalid tez constant"
    | Some expected_balance ->
        return (equal_tez ~msg expected_balance actual_balance)

  let delegate_equal ?block ~msg contract expected_delegate =
    Account.delegate ?block contract >>|? fun actual_delegate ->
    equal_pkh ~msg expected_delegate actual_delegate

  let ecoproto_error f = function
    | Alpha_environment.Ecoproto_error error -> f error
    | _ -> false

  let hash op = Tezos_base.Operation.hash op

  let contain_error ?(msg="") ~f = function
    | Ok _ -> fail "Error _" "Ok _" msg
    | Error error when not (List.exists f error) ->
        let error_str = Format.asprintf "%a" Error_monad.pp_print_error error in
        fail "" error_str msg
    | _ -> ()

  let failed_to_preapply ~msg ?op f =
    contain_error ~msg ~f:begin function
      | Client_baking_forge.Failed_to_preapply (op', err) ->
          begin
            match op with
            | None -> true
            | Some { shell ; protocol_data = Operation_data protocol_data } ->
                let h = Operation.hash { shell ; protocol_data } and h' = hash op' in
                Operation_hash.equal h h'
          end && List.exists (ecoproto_error f) err
      | _ -> false
    end

  let generic_economic_error ~msg =
    contain_error ~msg ~f:(ecoproto_error (fun _ -> true))

  let unknown_contract ~msg =
    contain_error ~msg ~f:begin ecoproto_error (function
        | Raw_context.Storage_error _ -> true
        | _ -> false)
    end

  let non_existing_contract ~msg =
    contain_error ~msg ~f:begin ecoproto_error (function
        | Contract_storage.Non_existing_contract _ -> true
        | _ -> false)
    end

  let balance_too_low ~msg =
    contain_error ~msg ~f:begin ecoproto_error (function
        | Contract.Balance_too_low _ -> true
        | _ -> false)
    end

  let non_spendable ~msg =
    contain_error ~msg ~f:begin ecoproto_error (function
        | Contract_storage.Unspendable_contract _ -> true
        | _ -> false)
    end

  let inconsistent_pkh ~msg =
    contain_error ~msg ~f:begin ecoproto_error (function
        | Contract_storage.Inconsistent_hash _ -> true
        | _ -> false)
    end

  let inconsistent_public_key ~msg =
    contain_error ~msg ~f:begin ecoproto_error (function
        | Contract_storage.Inconsistent_public_key _ -> true
        | _ -> false)
    end

  let missing_public_key ~msg =
    contain_error ~msg ~f:begin ecoproto_error (function
        | Contract_storage.Unrevealed_manager_key _ -> true
        | _ -> false)
    end

  let non_delegatable ~msg =
    contain_error ~msg ~f:begin ecoproto_error (function
        | Delegate_storage.Non_delegatable_contract _ -> true
        | _ -> false)
    end

  let check_protocol ?msg ~block h =
    Block_services.protocols
      !rpc_ctxt ~block () >>=? fun { next_protocol ; _ } ->
    return @@ equal
      ?msg
      ~prn:Protocol_hash.to_b58check
      ~eq:Protocol_hash.equal
      next_protocol h

  let check_voting_period_kind ?msg ~block kind =
    Alpha_block_services.metadata
      !rpc_ctxt ~chain:`Main ~block () >>=? fun { protocol_data = { voting_period_kind ; _ } ; _ } ->
    return @@ equal
      ?msg
      voting_period_kind
      kind

  let is_none ?(msg="") x =
    if x <> None then fail "None" "Some _" msg
  let is_some ?(msg="") x =
    if x = None then fail "Some _" "None" msg

end

module Baking = struct

  let bake block (contract: Account.t) operations =
    let ctxt = (new wrap_full (no_write_context ~block !rpc_config)) in
    Alpha_services.Helpers.current_level
      ctxt ~offset:1l (`Main, block) >>=? fun level ->
    let seed_nonce_hash =
      if level.Level.expected_commitment then
        let seed_nonce =
          match Nonce.of_bytes @@
            Rand.generate Constants.nonce_length with
          | Error _ -> assert false
          | Ok nonce -> nonce in
        Some (Nonce.hash seed_nonce)
      else
        None in
    let delegate_sk =
      Tezos_signer_backends.Unencrypted.make_sk contract.sk in
    Client_baking_forge.forge_block
      ctxt
      ~operations
      ~force:true
      ~best_effort:false
      ~sort:false
      ~chain:`Main
      ~priority:(`Auto (contract.pkh, Some 1024))
      ?seed_nonce_hash
      ~delegate_sk
      ~delegate_pkh:contract.pkh
      block

end

module Endorse = struct

  let forge_endorsement
      block
      src_sk
    =
    Shell_services.Blocks.hash !rpc_ctxt ~block () >>=? fun hash ->
    Alpha_block_services.metadata
      !rpc_ctxt ~chain:`Main ~block () >>=? fun { protocol_data = { level ; _ } ; _ } ->
    let level = level.level in
    let shell = { Tezos_base.Operation.branch = hash } in
    let contents =
      Single (Endorsement { level }) in
    sign ~watermark:(Endorsement Chain_id.zero) src_sk shell (Contents_list contents)

  let endorse
      (contract : Account.t)
      block =
    forge_endorsement block contract.sk

  (* FIXME @vb: I don't understand this function, copied from @cago. *)
  let endorsers_list block =
    let get_endorser_list result (account : Account.t) level block =
      Alpha_services.Delegate.Endorsing_rights.get
        !rpc_ctxt (`Main, block)
        ~delegates:[account.pkh]
        ~levels:[level] >>|? function
      | [{ slots ; _ }] ->
          List.iter (fun s -> result.(s) <- account) slots
      | _ -> () in
    let { Account.b1 ; b2 ; b3 ; b4 ; b5 } = Account.bootstrap_accounts in
    let result = Array.make 32 b1 in
    Alpha_block_services.metadata
      !rpc_ctxt ~chain:`Main ~block () >>=? fun { protocol_data = { level ; _ } ; _ } ->
    let level = level.level in
    get_endorser_list result b1 level block >>=? fun () ->
    get_endorser_list result b2 level block >>=? fun () ->
    get_endorser_list result b3 level block >>=? fun () ->
    get_endorser_list result b4 level block >>=? fun () ->
    get_endorser_list result b5 level block >>=? fun () ->
    return result

  let endorsement_rights
      (contract : Account.t) block =
    Alpha_block_services.metadata
      !rpc_ctxt ~chain:`Main ~block () >>=? fun { protocol_data = { level ; _ } ; _ } ->
    let level = level.level in
    let delegate = contract.pkh in
    Alpha_services.Delegate.Endorsing_rights.get
      !rpc_ctxt
      ~levels:[level]
      ~delegates:[delegate]
      (`Main, block) >>=? function
    | [{ level ; slots ; _ }] -> return (List.map (fun s -> (level, s)) slots)
    | _ -> return_nil

end

let display_level block =
  Alpha_block_services.metadata
    !rpc_ctxt ~chain:`Main ~block () >>=? fun { protocol_data = { level ; _ } ; _ } ->
  Format.eprintf "Level: %a@." Level.pp_full level ;
  return_unit

let endorsement_security_deposit block =
  Constants_services.all !rpc_ctxt (`Main, block) >>=? fun c ->
  return c.parametric.endorsement_security_deposit

let () =
  Client_keys.register_signer
    (module Tezos_signer_backends.Unencrypted)
