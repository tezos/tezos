(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Client_embedded_proto_alpha
open Tezos_context
open Client_alpha

let (//) = Filename.concat

let () = Random.self_init ()

let rpc_config : Client_rpcs.config = {
  host = "localhost" ;
  port = 8192 + Random.int 8192 ;
  tls = false ;
  logger = Client_rpcs.null_logger ;
}

let dictator_sk =
  Ed25519.Secret_key.of_b58check_exn
    "edskRhxswacLW6jF6ULavDdzwqnKJVS4UcDTNiCyiH6H8ZNnn2pmNviL7\
     pRNz9kRxxaWQFzEQEcZExGHKbwmuaAcoMegj5T99z"

let activate_alpha () =
  let fitness = Fitness_repr.from_int64 0L in
  Client_genesis.Client_proto_main.mine
    rpc_config (`Head 0)
    (Activate Client_proto_main.protocol)
    fitness dictator_sk

let init ?(sandbox = "sandbox.json") () =
  Unix.chdir (Filename.dirname (Filename.dirname Sys.executable_name)) ;
  let pid =
    Node_helpers.fork_node
      ~port:rpc_config.port
      ~sandbox:(Filename.dirname Sys.executable_name // sandbox)
      () in
  activate_alpha () >>=? fun hash ->
  return (pid, hash)

let level block =
  Client_alpha.Client_proto_rpcs.Context.level rpc_config block

module Account = struct

  type t = {
    alias : string ;
    sk : secret_key ;
    pk : public_key ;
    pkh : public_key_hash ;
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
         (req "sk" Ed25519.Secret_key.encoding)
         (req "pk" Ed25519.Public_key.encoding)
         (req "pkh" Ed25519.Public_key_hash.encoding)
         (req "contract" Contract.encoding))

  let pp_account ppf account =
    let json = Data_encoding.Json.construct encoding account in
    Format.fprintf ppf "%s" (Data_encoding_ezjsonm.to_string json)

  let create ?keys alias =
    let sk, pk = match keys with
      | Some keys -> keys
      | None -> Sodium.Sign.random_keypair () in
    let pkh = Ed25519.Public_key.hash pk in
    let contract = Contract.default_contract pkh in
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
         (req "pk" Ed25519.Public_key.encoding)
         (req "pkh" Ed25519.Public_key_hash.encoding)
         (req "contract" Contract.encoding))

  let pp_destination ppf destination =
    let json = Data_encoding.Json.construct destination_encoding destination in
    Format.fprintf ppf "%s" (Data_encoding_ezjsonm.to_string json)

  let create_destination ~alias ~contract ~pk =
    let pkh = Ed25519.Public_key.hash pk in
    { alias ; contract ; pk ; pkh }

  type bootstrap_accounts = { b1 : t ; b2 : t ; b3 : t ; b4 : t ;  b5 : t  ; }

  let bootstrap_accounts =
    let bootstrap1_pk =
      Ed25519.Public_key.of_b58check_exn
        "edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav" in
    let bootstrap2_pk =
      Ed25519.Public_key.of_b58check_exn
        "edpktzNbDAUjUk697W7gYg2CRuBQjyPxbEg8dLccYYwKSKvkPvjtV9" in
    let bootstrap3_pk =
      Ed25519.Public_key.of_b58check_exn
        "edpkuTXkJDGcFd5nh6VvMz8phXxU3Bi7h6hqgywNFi1vZTfQNnS1RV" in
    let bootstrap4_pk =
      Ed25519.Public_key.of_b58check_exn
        "edpkuFrRoDSEbJYgxRtLx2ps82UdaYc1WwfS9sE11yhauZt5DgCHbU" in
    let bootstrap5_pk =
      Ed25519.Public_key.of_b58check_exn
        "edpkv8EUUH68jmo3f7Um5PezmfGrRF24gnfLpH3sVNwJnV5bVCxL2n" in
    let bootstrap1_sk =
      Ed25519.Secret_key.of_b58check_exn
        "edskRuR1azSfboG86YPTyxrQgosh5zChf5bVDmptqLTb5EuXAm9\
         rsnDYfTKhq7rDQujdn5WWzwUMeV3agaZ6J2vPQT58jJAJPi" in
    let bootstrap2_sk =
      Ed25519.Secret_key.of_b58check_exn
        "edskRkJz4Rw2rM5NtabEWMbbg2bF4b1nfFajaqEuEk4SgU7eeDby\
         m9gVQtBTbYo32WUg2zb5sNBkD1whRN7zX43V9bftBbtaKc" in
    let bootstrap3_sk =
      Ed25519.Secret_key.of_b58check_exn
        "edskS3qsqsNgdjUqeMsVcEwBn8dkZ5iDRz6aF21KhcCtRiAkWByp\
         USbicccR4Vgqm9UdW2Vabuos6seezqgbXTrmcbLUG4rdAC" in
    let bootstrap4_sk =
      Ed25519.Secret_key.of_b58check_exn
        "edskRg9qcPqaVQa6jXWNMU5p71tseSuR7NzozgqZ9URsVDi81wTyP\
         JdFSBdeakobyHUi4Xgu61jgKRQvkhXrPmEdEUfiqfiJFL" in
    let bootstrap5_sk =
      Ed25519.Secret_key.of_b58check_exn
        "edskS7rLN2Df3nbS1EYvwJbWo4umD7yPM1SUeX7gp1WhCVpMFXjcC\
         yM58xs6xsnTsVqHQmJQ2RxoAjJGedWfvFmjQy6etA3dgZ" in
    let cpt = ref 0 in
    match List.map begin fun (pk, sk) ->
        incr cpt ;
        let alias = Printf.sprintf "bootstrap%d" !cpt in
        let pkh = Ed25519.Public_key.hash pk in
        { alias ; contract = Contract.default_contract pkh; pkh ; pk ; sk }
      end [
        bootstrap1_pk, bootstrap1_sk;
        bootstrap2_pk, bootstrap2_sk;
        bootstrap3_pk, bootstrap3_sk;
        bootstrap4_pk, bootstrap4_sk;
        bootstrap5_pk, bootstrap5_sk; ]
    with
    | [ b1 ; b2 ; b3 ; b4 ; b5 ] -> { b1 ; b2 ; b3 ; b4 ; b5 }
    | _ -> assert false

  let transfer
      ?(block = `Prevalidation)
      ?(fee = 5L)
      ~(account:t)
      ~destination
      ~amount () =
    let amount = match Tez.of_cents amount with None -> Tez.zero | Some a -> a in
    let fee = match Tez.of_cents fee with None -> Tez.zero | Some a -> a in
    Client_proto_context.transfer rpc_config
      block
      ~source:account.contract
      ~src_pk:account.pk
      ~src_sk:account.sk
      ~destination
      ~amount
      ~fee ()

  let originate
      ?(block = `Prevalidation)
      ?delegate
      ?(fee=5L)
      ~(src:t)
      ~manager_pkh
      ~spendable
      ~balance
       () =
    let fee = match Tez.of_cents fee with
      | None -> Tez.zero
      | Some amount -> amount in
    let balance = match Tez.of_cents balance with
      | None -> Tez.zero
      | Some amount -> amount in
    let delegatable, delegate = match delegate with
      | None -> false, None
      | Some delegate -> true, Some delegate in
    Client_proto_context.originate_account rpc_config block
      ~source:src.contract
      ~src_pk:src.pk
      ~src_sk:src.sk
      ~manager_pkh
      ~spendable
      ~balance
      ~delegatable
      ?delegate
      ~fee ()

  let set_delegate
      ?(block = `Prevalidation)
      ?(fee = 5L)
      ~contract
      ~manager_sk
      delegate_opt =
    let fee = match Tez.of_cents fee with
      | None -> Tez.zero
      | Some amount -> amount in
    Client_proto_context.delegate_contract rpc_config block
      ~source:contract
      ~manager_sk
      ~fee
      delegate_opt

  let balance ?(block = `Prevalidation) (account : t) =
    Client_proto_rpcs.Context.Contract.balance rpc_config
      block account.contract

  (* TODO: gather contract related functions in a Contract module? *)
  let delegate ?(block = `Prevalidation) (contract : Contract.t) =
    Client_proto_rpcs.Context.Contract.delegate rpc_config
      block contract

end

module Protocol = struct

  open Account

  let voting_period_kind ?(block = `Prevalidation) () =
    Client_proto_rpcs.Context.voting_period_kind rpc_config block

  let proposals ?(block = `Prevalidation) ~src:({ pk; sk } : Account.t) proposals =
    Client_node_rpcs.Blocks.info rpc_config block >>=? fun block_info ->
    Client_proto_rpcs.Context.next_level rpc_config block >>=? fun next_level ->
    Client_proto_rpcs.Helpers.Forge.Delegate.proposals rpc_config block
      ~net_id:block_info.net_id
      ~branch:block_info.hash
      ~source:pk
      ~period:next_level.voting_period
      ~proposals
      () >>=? fun bytes ->
    let signed_bytes = Ed25519.Signature.append sk bytes in
    return (Tezos_data.Operation.of_bytes_exn signed_bytes)

  let ballot ?(block = `Prevalidation) ~src:({ pk; sk } : Account.t) ~proposal ballot =
    Client_node_rpcs.Blocks.info rpc_config block >>=? fun block_info ->
    Client_proto_rpcs.Context.next_level rpc_config block >>=? fun next_level ->
    Client_proto_rpcs.Helpers.Forge.Delegate.ballot rpc_config block
      ~net_id:block_info.net_id
      ~branch:block_info.hash
      ~source:pk
      ~period:next_level.voting_period
      ~proposal
      ~ballot
      () >>=? fun bytes ->
    let signed_bytes = Ed25519.Signature.append sk bytes in
    return (Tezos_data.Operation.of_bytes_exn signed_bytes)

end

module Assert = struct

  include Assert

  let equal_pkh ?msg pkh1 pkh2 =
    let msg = Assert.format_msg msg in
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

  let equal_tez ?msg tz1 tz2 =
    let msg = Assert.format_msg msg in
    let eq tz1 tz2 = Int64.equal (Tez.to_cents tz1) (Tez.to_cents tz2) in
    let prn = Tez.to_string in
    Assert.equal ?msg ~prn ~eq tz1 tz2

  let balance_equal ?block ~msg account expected_balance =
    Account.balance ?block account >>=? fun actual_balance ->
    match Tez.of_cents expected_balance with
    | None ->
        failwith "invalid tez constant"
    | Some expected_balance ->
        return (equal_tez ~msg actual_balance expected_balance)

  let delegate_equal ?block ~msg contract expected_delegate =
    Account.delegate ?block contract >>|? fun actual_delegate ->
    equal_pkh ~msg actual_delegate expected_delegate

  let ecoproto_error f = function
    | Register_client_embedded_proto_alpha.Ecoproto_error errors ->
        List.exists f errors
    | _ -> false

  let hash = function
    | Client_node_rpcs.Hash h -> h
    | Blob op -> Tezos_data.Operation.hash op

  let failed_to_preapply ~msg ?op f =
    Assert.contain_error ~msg ~f:begin function
      | Client_mining_forge.Failed_to_preapply (op', err) ->
        begin
          match op with
          | None -> true
          | Some op ->
              let h = hash op and h' = hash op' in
              Operation_hash.equal h h'
        end && List.exists (ecoproto_error f) err
      | _ -> false
    end

  let generic_economic_error ~msg =
    Assert.contain_error ~msg ~f:(ecoproto_error (fun _ -> true))

  let unknown_contract ~msg =
    let open Client_embedded_proto_alpha.Storage_functors in
    Assert.contain_error ~msg ~f:begin ecoproto_error (function
        | Storage_error _ -> true
        | _ -> false)
    end

  let non_existing_contract ~msg =
    Assert.contain_error ~msg ~f:begin ecoproto_error (function
        | Contract_storage.Non_existing_contract _ -> true
        | _ -> false)
    end

  let balance_too_low ~msg =
    Assert.contain_error ~msg ~f:begin ecoproto_error (function
        | Contract.Balance_too_low _ -> true
        | _ -> false)
    end

  let non_spendable ~msg =
    Assert.contain_error ~msg ~f:begin ecoproto_error (function
        | Contract_storage.Unspendable_contract _ -> true
        | _ -> false)
    end

  let inconsistent_pkh ~msg =
    Assert.contain_error ~msg ~f:begin ecoproto_error (function
        | Public_key_storage.Inconsistent_hash _ -> true
        | _ -> false)
    end

  let initial_amount_too_low ~msg =
    Assert.contain_error ~msg ~f:begin ecoproto_error (function
        | Contract.Initial_amount_too_low _ -> true
        | _ -> false)
    end

  let non_delegatable ~msg =
    Assert.contain_error ~msg ~f:begin ecoproto_error (function
        | Contract_storage.Non_delegatable_contract _ -> true
        | _ -> false)
    end

  let wrong_delegate ~msg =
    Assert.contain_error ~msg ~f:begin ecoproto_error (function
        | Mining.Wrong_delegate _ -> true
        | _ -> false)
    end

  let check_protocol ?msg ~block h =
    Client_node_rpcs.Blocks.protocol rpc_config block >>=? fun block_proto ->
    return @@ Assert.equal
      ?msg:(Assert.format_msg msg)
      ~prn:Protocol_hash.to_b58check
      ~eq:Protocol_hash.equal
      block_proto h

  let check_voting_period_kind ?msg ~block kind =
    Client_proto_rpcs.Context.voting_period_kind rpc_config block
    >>=? fun current_kind ->
    return @@ Assert.equal
      ?msg:(Assert.format_msg msg)
      current_kind kind

end

module Mining = struct

  let mine block (contract: Account.t) operations =
    let operations = List.map (fun op -> Client_node_rpcs.Blob op) operations in
    let seed_nonce =
      match Nonce.of_bytes @@
        Sodium.Random.Bigbytes.generate Constants.nonce_length with
      | Error _ -> assert false
      | Ok nonce -> nonce in
    let seed_nonce_hash = Nonce.hash seed_nonce in
    Client_mining_forge.forge_block
      rpc_config
      block
      ~operations
      ~force:true
      ~best_effort:false
      ~sort:false
      ~priority:(`Auto (contract.pkh, Some 1024, false))
      ~seed_nonce_hash
      ~src_sk:contract.sk
      ()

  let endorsement_reward block =
    Client_proto_rpcs.Header.priority rpc_config block >>=? fun prio ->
    Mining.endorsement_reward ~block_priority:prio >|=
    Register_client_embedded_proto_alpha.wrap_error >>|?
    Tez.to_cents

end

module Endorse = struct

  let forge_endorsement
      block
      src_sk
      source
      slot =
    let block = Client_rpcs.last_mined_block block in
    Client_node_rpcs.Blocks.info rpc_config block >>=? fun { hash ; net_id } ->
    Client_proto_rpcs.Helpers.Forge.Delegate.endorsement rpc_config
      block
      ~net_id:net_id
      ~branch:hash
      ~source
      ~block:hash
      ~slot:slot
      () >>=? fun bytes ->
    let signed_bytes = Ed25519.Signature.append src_sk bytes in
    return (Tezos_data.Operation.of_bytes_exn signed_bytes)

  let signing_slots
      ?(max_priority = 1024)
      block
      delegate
      level =
    Client_proto_rpcs.Helpers.Rights.endorsement_rights_for_delegate
      rpc_config ~max_priority ~first_level:level ~last_level:level
      block delegate () >>=? fun possibilities ->
    let slots =
      List.map (fun (_,slot) -> slot)
      @@ List.filter (fun (l, _) -> l = level) possibilities in
    return slots

  let endorse
      ?slot
      (contract : Account.t)
      block =
    Client_proto_rpcs.Context.next_level rpc_config block >>=? fun { level } ->
    begin
      match slot with
      | Some slot -> return slot
      | None -> begin
          signing_slots
            block contract.Account.pkh
            level >>=? function
          | slot::_ -> return slot
          | [] ->
              failwith "No slot found at level %a" Raw_level.pp level
        end
    end >>=? fun slot ->
    forge_endorsement block contract.sk contract.pk slot

  (* FIXME @vb: I don't understand this function, copied from @cago. *)
  let endorsers_list block =
    let get_endorser_list result (account : Account.t) level block =
      Client_proto_rpcs.Helpers.Rights.endorsement_rights_for_delegate
        rpc_config block account.pkh
        ~max_priority:16
        ~first_level:level
        ~last_level:level () >>|? fun slots ->
      List.iter (fun (_,slot) -> result.(slot) <- account) slots
    in
    let { Account.b1 ; b2 ; b3 ; b4 ; b5 } = Account.bootstrap_accounts in
    let result = Array.make 16 b1 in
    Client_proto_rpcs.Context.level rpc_config block >>=? fun level ->
    let level = Raw_level.succ @@ level.level in
    get_endorser_list result b1 level block >>=? fun () ->
    get_endorser_list result b2 level block >>=? fun () ->
    get_endorser_list result b3 level block >>=? fun () ->
    get_endorser_list result b4 level block >>=? fun () ->
    get_endorser_list result b5 level block >>=? fun () ->
    return result

  let endorsement_rights
    ?(max_priority = 1024)
    (contract : Account.t) block =
    Client_proto_rpcs.Context.level rpc_config block >>=? fun level ->
    let delegate = contract.pkh in
    let level = level.level in
    Client_proto_rpcs.Helpers.Rights.endorsement_rights_for_delegate
      rpc_config
      ~max_priority
      ~first_level:level
      ~last_level:level
      block delegate ()

end

let display_level block =
  Client_proto_rpcs.Context.level rpc_config block >>=? fun lvl ->
  Format.eprintf "Level: %a@." Level.pp_full lvl ;
  return ()
