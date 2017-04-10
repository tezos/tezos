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

let rpc_config : Client_rpcs.config = {
  host = "localhost" ;
  port = 18732 ;
  tls = false ;
  logger = Client_rpcs.null_logger ;
}

let dictator_sk =
  Environment.Ed25519.Secret_key.of_b58check_exn
    "edskRhxswacLW6jF6ULavDdzwqnKJVS4UcDTNiCyiH6H8ZNnn2pmNviL7\
     pRNz9kRxxaWQFzEQEcZExGHKbwmuaAcoMegj5T99z"

let activate_alpha () =
  let fitness = Fitness_repr.from_int64 0L in
  Client_genesis.Client_proto_main.mine
    rpc_config (`Head 0)
    (Activate Client_proto_main.protocol)
    fitness dictator_sk

let init () =
  Random.self_init () ;
  Unix.chdir (Filename.dirname (Filename.dirname Sys.executable_name)) ;
  let pid =
    Node_helpers.fork_node
      ~port:rpc_config.port
      ~sandbox:(Filename.dirname Sys.executable_name // "sandbox.json")
      () in
  activate_alpha () >>=? fun hash ->
  return (pid, hash)

module Account = struct

  type t = {
    alias : string ;
    sk : secret_key ;
    pk : public_key ;
    pkh : public_key_hash ;
    contract : Contract.t ;
  }

  let encoding =
    let open Environment.Ed25519 in
    let open Data_encoding in
    conv
      (fun { alias ; sk ; pk ; pkh ; contract } ->
         (alias, sk, pk, pkh, contract)
      )
      (fun (alias, sk, pk, pkh, contract) ->
         { alias ; sk ; pk ; pkh ; contract })
      (obj5
         (req "alias" string)
         (req "sk" Secret_key.encoding)
         (req "pk" Public_key.encoding)
         (req "pkh" Public_key_hash.encoding)
         (req "contract" Contract.encoding))

  let pp_account ppf account =
    let json = Data_encoding.Json.construct encoding account in
    Format.fprintf ppf "%s" (Data_encoding_ezjsonm.to_string json)

  let create ?keys alias =
    let sk, pk = match keys with
      | Some keys -> keys
      | None -> Sodium.Sign.random_keypair () in
    let pkh = Environment.Ed25519.Public_key.hash pk in
    let contract = Contract.default_contract pkh in
    { alias ; contract ; pkh ; pk ; sk }

  type destination = {
    alias : string ;
    contract : Contract.t ;
    pk : public_key ;
    pkh : public_key_hash ;
  }

  let destination_encoding =
    let open Environment.Ed25519 in
    let open Data_encoding in
    conv
      (fun { alias ; pk ; pkh ; contract } ->
         (alias, pk, pkh, contract))
      (fun (alias, pk, pkh, contract) ->
         { alias ; pk ; pkh ; contract })
      (obj4
         (req "alias" string)
         (req "pk" Public_key.encoding)
         (req "pkh" Public_key_hash.encoding)
         (req "contract" Contract.encoding))

  let pp_destination ppf destination =
    let json = Data_encoding.Json.construct destination_encoding destination in
    Format.fprintf ppf "%s" (Data_encoding_ezjsonm.to_string json)

  let create_destination ~alias ~contract ~pk =
    let pkh = Environment.Ed25519.Public_key.hash pk in
    { alias ; contract ; pk ; pkh }

  type bootstrap_accounts = { b1 : t ; b2 : t ; b3 : t ; b4 : t ;  b5 : t  ; }

  let bootstrap_accounts =
    let open Environment.Ed25519 in
    let bootstrap1_pk =
      Public_key.of_b58check_exn
        "edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav" in
    let bootstrap2_pk =
      Public_key.of_b58check_exn
        "edpktzNbDAUjUk697W7gYg2CRuBQjyPxbEg8dLccYYwKSKvkPvjtV9" in
    let bootstrap3_pk =
      Public_key.of_b58check_exn
        "edpkuTXkJDGcFd5nh6VvMz8phXxU3Bi7h6hqgywNFi1vZTfQNnS1RV" in
    let bootstrap4_pk =
      Public_key.of_b58check_exn
        "edpkuFrRoDSEbJYgxRtLx2ps82UdaYc1WwfS9sE11yhauZt5DgCHbU" in
    let bootstrap5_pk =
      Public_key.of_b58check_exn
        "edpkv8EUUH68jmo3f7Um5PezmfGrRF24gnfLpH3sVNwJnV5bVCxL2n" in
    let bootstrap1_sk =
      Secret_key.of_b58check_exn
        "edskRuR1azSfboG86YPTyxrQgosh5zChf5bVDmptqLTb5EuXAm9\
         rsnDYfTKhq7rDQujdn5WWzwUMeV3agaZ6J2vPQT58jJAJPi" in
    let bootstrap2_sk =
      Secret_key.of_b58check_exn
        "edskRkJz4Rw2rM5NtabEWMbbg2bF4b1nfFajaqEuEk4SgU7eeDby\
         m9gVQtBTbYo32WUg2zb5sNBkD1whRN7zX43V9bftBbtaKc" in
    let bootstrap3_sk =
      Secret_key.of_b58check_exn
        "edskS3qsqsNgdjUqeMsVcEwBn8dkZ5iDRz6aF21KhcCtRiAkWByp\
         USbicccR4Vgqm9UdW2Vabuos6seezqgbXTrmcbLUG4rdAC" in
    let bootstrap4_sk =
      Secret_key.of_b58check_exn
        "edskRg9qcPqaVQa6jXWNMU5p71tseSuR7NzozgqZ9URsVDi81wTyP\
         JdFSBdeakobyHUi4Xgu61jgKRQvkhXrPmEdEUfiqfiJFL" in
    let bootstrap5_sk =
      Secret_key.of_b58check_exn
        "edskS7rLN2Df3nbS1EYvwJbWo4umD7yPM1SUeX7gp1WhCVpMFXjcC\
         yM58xs6xsnTsVqHQmJQ2RxoAjJGedWfvFmjQy6etA3dgZ" in
    let cpt = ref 0 in
    match List.map begin fun (pk, sk) ->
        incr cpt ;
        let alias = Printf.sprintf "bootstrap%d" !cpt in
        let pkh = Environment.Ed25519.Public_key.hash pk in
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

module Assert = struct

  include Assert

  let equal_pkh ?msg pkh1 pkh2 =
    let msg = Assert.format_msg msg in
    let eq pkh1 pkh2 =
      match pkh1, pkh2 with
      | None, None -> true
      | Some pkh1, Some pkh2 ->
          Environment.Ed25519.Public_key_hash.equal pkh1 pkh2
      | _ -> false in
    let prn = function
      | None -> "none"
      | Some pkh -> Environment.Ed25519.Public_key_hash.to_hex pkh in
    Assert.equal ?msg ~prn ~eq pkh1 pkh2

  let equal_tez ?msg tz1 tz2 =
    let msg = Assert.format_msg msg in
    let eq tz1 tz2 = Int64.equal (Tez.to_cents tz1) (Tez.to_cents tz2) in
    let prn = Tez.to_string in
    Assert.equal ?msg ~prn ~eq tz1 tz2

  let balance_equal ~msg account expected_balance =
    Account.balance account >>=? fun actual_balance ->
    match Tez.of_cents expected_balance with
    | None ->
        failwith "invalid tez constant"
    | Some expected_balance ->
        return (equal_tez ~msg actual_balance expected_balance)

  let delegate_equal ~msg contract expected_delegate =
    Account.delegate contract >>|? fun actual_delegate ->
    equal_pkh ~msg actual_delegate expected_delegate

  let ecoproto_error f = function
    | Register_client_embedded_proto_alpha.Ecoproto_error errors ->
        List.exists f errors
    | _ -> false

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

  let invalid_endorsement_slot ~msg =
    Assert.contain_error ~msg ~f:begin ecoproto_error (function
        | Mining.Invalid_endorsement_slot _ -> true
        | _ -> false)
    end

end

module Mining = struct

  let get_first_priority
      ?(max_priority=1024)
      level
      (contract : Account.t)
      block =
    Client_proto_rpcs.Helpers.Rights.mining_rights_for_delegate
      rpc_config
      ~max_priority
      ~first_level:level
      ~last_level:level
      block contract.Account.pkh () >>=? fun possibilities ->
    try
      let _, prio, _ =
        List.find (fun (l,_,_) -> l = level) possibilities in
      return prio
    with Not_found ->
      failwith "No slot found at level %a" Raw_level.pp level

  let rec mine_stamp
      block
      delegate_sk
      shell
      priority
      seed_nonce_hash =
    Client_proto_rpcs.Constants.stamp_threshold
      rpc_config block >>=? fun stamp_threshold ->
    let rec loop () =
      let proof_of_work_nonce =
        Sodium.Random.Bigbytes.generate Constants.proof_of_work_nonce_size in
      let unsigned_header =
        Block.forge_header
          shell { priority ; seed_nonce_hash ; proof_of_work_nonce } in
      let signed_header =
        Environment.Ed25519.Signature.append delegate_sk unsigned_header in
      let block_hash = Block_hash.hash_bytes [signed_header] in
      if Mining.check_hash block_hash stamp_threshold then
        proof_of_work_nonce
      else
        loop () in
    return (loop ())

  let inject_block
      block
      ?force
      ~priority
      ~timestamp
      ~fitness
      ~seed_nonce
      ~src_sk
      operation_list =
    let block = match block with `Prevalidation -> `Head 0 | block -> block in
    Client_node_rpcs.Blocks.info rpc_config block >>=? fun bi ->
    let seed_nonce_hash = Nonce.hash seed_nonce in
    Client_proto_rpcs.Context.next_level rpc_config block >>=? fun level ->
    let operations_hash =
      Operation_list_list_hash.compute
        [Operation_list_hash.compute operation_list] in
    let shell =
      { Store.Block_header.net_id = bi.net_id ; predecessor = bi.hash ;
        timestamp ; fitness ; operations_hash ;
        level = Raw_level.to_int32 level.level } in
    mine_stamp
      block src_sk shell priority seed_nonce_hash >>=? fun proof_of_work_nonce ->
    Client_proto_rpcs.Helpers.Forge.block rpc_config
      block
      ~net:bi.net_id
      ~predecessor:bi.hash
      ~timestamp
      ~fitness
      ~operations_hash
      ~level:level.level
      ~priority
      ~seed_nonce_hash
      ~proof_of_work_nonce
      () >>=? fun unsigned_header ->
    let signed_header = Environment.Ed25519.Signature.append src_sk unsigned_header in
    Client_node_rpcs.inject_block rpc_config
      ?force signed_header [operation_list] >>=? fun block_hash ->
    return block_hash

  let mine
      ?(force = false)
      ?(operations = [])
      ~fitness_gap
      contract
      block =
  Client_mining_blocks.info rpc_config block >>=? fun bi ->
  let seed_nonce =
    match Nonce.of_bytes @@
      Sodium.Random.Bigbytes.generate Constants.nonce_length with
    | Error _ -> assert false
    | Ok nonce -> nonce in
  let timestamp = Time.add (Time.now ()) 1L in
  Client_proto_rpcs.Context.level rpc_config block >>=? fun level ->
  let level = Raw_level.succ level.level in
  get_first_priority level contract block >>=? fun priority ->
  (Lwt.return (Fitness_repr.to_int64 bi.fitness) >|=
   Register_client_embedded_proto_alpha.wrap_error) >>=? fun fitness ->
  let fitness =
    Fitness_repr.from_int64 @@
    Int64.add fitness (Int64.of_int fitness_gap) in
  inject_block
    ~force
    ~priority
    ~timestamp
    ~fitness
    ~seed_nonce
    ~src_sk:contract.sk
    block
    operations

  let endorsement_reward contract block =
    Client_mining_blocks.info rpc_config block >>=? fun bi ->
    get_first_priority bi.level.level contract block >>=? fun prio ->
    Mining.endorsement_reward ~block_priority:prio >|=
    Register_client_embedded_proto_alpha.wrap_error >>|?
    Tez.to_cents

end

module Endorse = struct

  let inject_endorsement
      block
      _level
      ?async
      ?force
      src_sk
      source
      slot =
    Client_blocks.get_block_hash rpc_config block >>=? fun block_hash ->
    Client_node_rpcs.Blocks.net rpc_config block >>=? fun net ->
    Client_proto_rpcs.Helpers.Forge.Delegate.endorsement rpc_config
      block
      ~net
      ~source
      ~block:block_hash
      ~slot:slot
      () >>=? fun bytes ->
    let signed_bytes = Environment.Ed25519.Signature.append src_sk bytes in
    Client_node_rpcs.inject_operation
      rpc_config ?force ?async signed_bytes >>=? fun oph ->
    return oph

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
      ?(force = false)
      ?slot
      (contract : Account.t)
      block =
    Client_proto_rpcs.Context.level rpc_config block >>=? fun level ->
    let level = Raw_level.succ @@ level.level in
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
    inject_endorsement
      block level contract.sk contract.pk slot ~force >>=? fun oph ->
    return oph

  (* FIXME @vb: I don't understand this function, copied from @cago. *)
  let endorsers_list block { Account.b1 ; b2 ; b3 ; b4 ; b5 } =
    let get_endorser_list result (account : Account.t) level block =
      Client_proto_rpcs.Helpers.Rights.endorsement_rights_for_delegate
        rpc_config block account.pkh
        ~max_priority:16
        ~first_level:level
        ~last_level:level () >>|? fun slots ->
      List.iter (fun (_,slot) -> result.(slot) <- account) slots
    in
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
