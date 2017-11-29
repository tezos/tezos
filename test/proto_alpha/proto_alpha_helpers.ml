(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

module Ed25519 = Environment.Ed25519

let (//) = Filename.concat

let () = Random.self_init ()

let rpc_config = ref {
    Client_rpcs.host = "localhost" ;
    port = 8192 + Random.int 8192 ;
    tls = false ;
    logger = Client_rpcs.null_logger ;
  }

(* Context that does not write to alias files *)
let no_write_context config block : Client_commands.full_context = object
  inherit Client_rpcs.rpc config
  inherit Client_commands.logger (fun _ _ -> Lwt.return_unit)
  method load : type a. string -> default:a -> a Data_encoding.encoding -> a Error_monad.tzresult Lwt.t =
    fun _ ~default _ -> return default
  method write : type a. string ->
    a ->
    a Data_encoding.encoding -> unit Error_monad.tzresult Lwt.t =
    fun _ _ _ -> return ()
  method block = block
end

let dictator_sk =
  Ed25519.Secret_key.of_b58check_exn
    "edskRhxswacLW6jF6ULavDdzwqnKJVS4UcDTNiCyiH6H8ZNnn2pmNviL7\
     pRNz9kRxxaWQFzEQEcZExGHKbwmuaAcoMegj5T99z"

let activate_alpha () =
  let fitness = Fitness_repr.from_int64 0L in
  Tezos_embedded_client_genesis.Client_proto_main.bake
    (new Client_rpcs.rpc !rpc_config) (`Head 0)
    (Activate  { protocol = Client_proto_main.protocol ; validation_passes = 1})
    fitness dictator_sk

let init ?(sandbox = "sandbox.json") ?rpc_port () =
  begin
    match rpc_port with
    | None -> ()
    | Some port -> rpc_config := { !rpc_config with port }
  end ;
  (* Handles relative path on OSX *)
  let executable_path =
    if Filename.is_relative Sys.argv.(0)
    then Filename.concat (Sys.getcwd ()) Sys.argv.(0)
    else Sys.argv.(0) in
  Unix.chdir (Filename.dirname executable_path) ;
  Unix.chdir ".." ;
  let pid =
    Node_helpers.fork_node
      ~port:!rpc_config.port
      ~sandbox:(Filename.dirname executable_path // sandbox)
      () in
  activate_alpha () >>=? fun hash ->
  return (pid, hash)

let level block =
  Client_proto_rpcs.Context.level (new Client_rpcs.rpc !rpc_config) block

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
      | None -> let _, pk, sk = Ed25519.generate_key () in sk, pk in
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
      ?(fee = Tez.fifty_cents)
      ~(account:t)
      ~destination
      ~amount () =
    Client_proto_context.transfer (new Client_rpcs.rpc !rpc_config)
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
      ?(fee = Tez.fifty_cents)
      ~(src:t)
      ~manager_pkh
      ~balance
      () =
    let delegatable, delegate = match delegate with
      | None -> false, None
      | Some delegate -> true, Some delegate in
    Client_proto_context.originate_account
      ~source:src.contract
      ~src_pk:src.pk
      ~src_sk:src.sk
      ~manager_pkh
      ~balance
      ~delegatable
      ?delegate
      ~fee
      block
      (new Client_rpcs.rpc !rpc_config)
      ()

  let set_delegate
      ?(block = `Prevalidation)
      ?(fee = Tez.fifty_cents)
      ~contract
      ~manager_sk
      ~src_pk
      delegate_opt =
    Client_proto_context.set_delegate
      (new Client_rpcs.rpc !rpc_config)
      block
      ~fee
      contract
      ~src_pk
      ~manager_sk
      delegate_opt

  let balance ?(block = `Prevalidation) (account : t) =
    Client_proto_rpcs.Context.Contract.balance (new Client_rpcs.rpc !rpc_config)
      block account.contract

  (* TODO: gather contract related functions in a Contract module? *)
  let delegate ?(block = `Prevalidation) (contract : Contract.t) =
    Client_proto_rpcs.Context.Contract.delegate (new Client_rpcs.rpc !rpc_config)
      block contract

end

module Protocol = struct

  open Account

  let voting_period_kind ?(block = `Prevalidation) () =
    Client_proto_rpcs.Context.voting_period_kind (new Client_rpcs.rpc !rpc_config) block

  let proposals ?(block = `Prevalidation) ~src:({ pk; sk } : Account.t) proposals =
    Client_node_rpcs.Blocks.info (new Client_rpcs.rpc !rpc_config) block >>=? fun block_info ->
    Client_proto_rpcs.Context.next_level (new Client_rpcs.rpc !rpc_config) block >>=? fun next_level ->
    Client_proto_rpcs.Helpers.Forge.Delegate.proposals (new Client_rpcs.rpc !rpc_config) block
      ~branch:block_info.hash
      ~source:pk
      ~period:next_level.voting_period
      ~proposals
      () >>=? fun bytes ->
    let signed_bytes = Ed25519.Signature.append sk bytes in
    return (Tezos_base.Operation.of_bytes_exn signed_bytes)

  let ballot ?(block = `Prevalidation) ~src:({ pk; sk } : Account.t) ~proposal ballot =
    let rpc = new Client_rpcs.rpc !rpc_config in
    Client_node_rpcs.Blocks.info rpc block >>=? fun block_info ->
    Client_proto_rpcs.Context.next_level rpc block >>=? fun next_level ->
    Client_proto_rpcs.Helpers.Forge.Delegate.ballot rpc block
      ~branch:block_info.hash
      ~source:pk
      ~period:next_level.voting_period
      ~proposal
      ~ballot
      () >>=? fun bytes ->
    let signed_bytes = Ed25519.Signature.append sk bytes in
    return (Tezos_base.Operation.of_bytes_exn signed_bytes)

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
    let eq tz1 tz2 = Int64.equal (Tez.to_mutez tz1) (Tez.to_mutez tz2) in
    let prn = Tez.to_string in
    Assert.equal ?msg ~prn ~eq tz1 tz2

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
    | Environment.Ecoproto_error errors ->
        List.exists f errors
    | _ -> false

  let hash op = Tezos_base.Operation.hash op

  let failed_to_preapply ~msg ?op f =
    Assert.contain_error ~msg ~f:begin function
      | Client_baking_forge.Failed_to_preapply (op', err) ->
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
    Assert.contain_error ~msg ~f:begin ecoproto_error (function
        | Raw_context.Storage_error _ -> true
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
        | Contract_storage.Inconsistent_hash _ -> true
        | _ -> false)
    end

  let inconsistent_public_key ~msg =
    Assert.contain_error ~msg ~f:begin ecoproto_error (function
        | Contract_storage.Inconsistent_public_key _ -> true
        | _ -> false)
    end

  let missing_public_key ~msg =
    Assert.contain_error ~msg ~f:begin ecoproto_error (function
        | Contract_storage.Missing_public_key _ -> true
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
        | Baking.Wrong_delegate _ -> true
        | _ -> false)
    end

  let check_protocol ?msg ~block h =
    Client_node_rpcs.Blocks.protocol (new Client_rpcs.rpc !rpc_config) block >>=? fun block_proto ->
    return @@ Assert.equal
      ?msg:(Assert.format_msg msg)
      ~prn:Protocol_hash.to_b58check
      ~eq:Protocol_hash.equal
      block_proto h

  let check_voting_period_kind ?msg ~block kind =
    Client_proto_rpcs.Context.voting_period_kind (new Client_rpcs.rpc !rpc_config) block
    >>=? fun current_kind ->
    return @@ Assert.equal
      ?msg:(Assert.format_msg msg)
      current_kind kind

end

module Baking = struct

  let bake block (contract: Account.t) operations =
    let seed_nonce =
      match Nonce.of_bytes @@
        Sodium.Random.Bigbytes.generate Constants.nonce_length with
      | Error _ -> assert false
      | Ok nonce -> nonce in
    let seed_nonce_hash = Nonce.hash seed_nonce in
    Client_baking_forge.forge_block
      (new Client_rpcs.rpc !rpc_config)
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
    Client_proto_rpcs.Header.priority (new Client_rpcs.rpc !rpc_config) block >>=? fun prio ->
    Baking.endorsement_reward ~block_priority:prio >|=
    Environment.wrap_error >>|?
    Tez.to_mutez

end

module Endorse = struct

  let forge_endorsement
      block
      src_sk
      source
      slot =
    let block = Client_rpcs.last_baked_block block in
    let rpc = new Client_rpcs.rpc !rpc_config in
    Client_node_rpcs.Blocks.info rpc block >>=? fun { hash ; _ } ->
    Client_proto_rpcs.Helpers.Forge.Delegate.endorsement rpc
      block
      ~branch:hash
      ~source
      ~block:hash
      ~slot:slot
      () >>=? fun bytes ->
    let signed_bytes = Ed25519.Signature.append src_sk bytes in
    return (Tezos_base.Operation.of_bytes_exn signed_bytes)

  let signing_slots
      ?(max_priority = 1024)
      block
      delegate
      level =
    Client_proto_rpcs.Helpers.Rights.endorsement_rights_for_delegate
      (new Client_rpcs.rpc !rpc_config) ~max_priority ~first_level:level ~last_level:level
      block delegate () >>=? fun possibilities ->
    let slots =
      List.map (fun (_,slot) -> slot)
      @@ List.filter (fun (l, _) -> l = level) possibilities in
    return slots

  let endorse
      ?slot
      (contract : Account.t)
      block =
    Client_proto_rpcs.Context.next_level (new Client_rpcs.rpc !rpc_config) block >>=? fun { level } ->
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
        (new Client_rpcs.rpc !rpc_config) block account.pkh
        ~max_priority:16
        ~first_level:level
        ~last_level:level () >>|? fun slots ->
      List.iter (fun (_,slot) -> result.(slot) <- account) slots
    in
    let { Account.b1 ; b2 ; b3 ; b4 ; b5 } = Account.bootstrap_accounts in
    let result = Array.make 16 b1 in
    Client_proto_rpcs.Context.level (new Client_rpcs.rpc !rpc_config) block >>=? fun level ->
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
    let rpc = new Client_rpcs.rpc !rpc_config in
    Client_proto_rpcs.Context.level rpc block >>=? fun level ->
    let delegate = contract.pkh in
    let level = level.level in
    Client_proto_rpcs.Helpers.Rights.endorsement_rights_for_delegate
      rpc
      ~max_priority
      ~first_level:level
      ~last_level:level
      block delegate ()

end

let display_level block =
  Client_proto_rpcs.Context.level (new Client_rpcs.rpc !rpc_config) block >>=? fun lvl ->
  Format.eprintf "Level: %a@." Level.pp_full lvl ;
  return ()
