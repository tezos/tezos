(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Proto_alpha
open Alpha_context

let (//) = Filename.concat

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
let no_write_context ?(block = `Head 0) config : #Client_context.full = object
  inherit RPC_client.http_ctxt config Media_type.all_media_types
  inherit Client_context.simple_printer (fun _ _ -> Lwt.return_unit)
  method load : type a. string -> default:a -> a Data_encoding.encoding -> a Error_monad.tzresult Lwt.t =
    fun _ ~default _ -> return default
  method write : type a. string ->
    a ->
    a Data_encoding.encoding -> unit Error_monad.tzresult Lwt.t =
    fun _ _ _ -> return ()
  method block = block
  method prompt : type a. (a, string) Client_context.lwt_format -> a =
    Format.kasprintf (fun _ -> Lwt.return "")
  method prompt_password : type a. (a, string) Client_context.lwt_format -> a =
    Format.kasprintf (fun _ -> Lwt.return "")
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
    [ "tz1fkmDXEQdua3u71JBVh4eGUGqK4t1G8xhZ", "btz1T77Ly5U1bWNBR5KzDSgNFST5Bh5F1eB6g", "1868898542104130027" ],
    [ "tz1doNkK6RKaRswsuKJV4erT6HauYSm9fuHi", "btz1QxTPgszARgWioEor3eMehxW3osfhw3KoJ", "517697389496079974" ],
    [ "tz1g6zFsci4YR8p1MJrkyc6wAKizR6mqJmyQ", "btz1NUYc1tV5VBksMNuQG4AuZF9Xudh1sDJni", "962290491831710023" ],
    [ "tz1MpbcwGFWVBBWoxwm6iQH5Hzh9mCXbnETJ", "btz1Wwifd8vbQqnbuzSbQLvJEjQ9FUoxVJm68", "1233665184704419921" ],
    [ "tz1U4t2PmX5cZVUui4BNaiRVokLa6AxB5G9Z", "btz1RrFkp9GmnypoNGRSyURkBQNUs4PPYG8SR", "131959324067470008" ],
    [ "tz1RUxPjviua4XJM78XjSKGUCAS9R3y8Bdof", "btz1hozabzP9HdRakJddzyea7DgPHzJ5PB37N", "112378240876120002" ],
    [ "tz1fJPeueQKJrTj2SFV2PmyCinLYJKXCEMf4", "btz1j1k4nrZB4r8RpTYiy8zbi3Fappopkb8ZF", "1060667014046690017" ],
    [ "tz1iDPZLxcGf5CqCNpTuuMdtu3zKpJ6HvvFR", "btz1TaSfoSNhFoqwqbPC9iC19rN24KJtB7skD", "71300478465380003" ],
    [ "tz1i2kbtVu65dP739qLGRpJNujRM8pdpkH3p", "btz1USqQRuvASPXwseXkGTWeWv4dn3VwMVPEk", "283380756728119992" ],
    [ "tz1LKGCg6ESJLDviHkT8Jc7tUwjw4h3d9MaF", "btz1YwCKMbBLRL1qkBjAHGCwjbWDqiTAEFpbw", "1357762577679880028" ]
  ],
  "time_between_blocks" : [ 1, 0 ],
  "blocks_per_cycle" : 4,
  "blocks_per_roll_snapshot" : 2,
  "preserved_cycles" : 1,
  "first_free_baking_slot" : 4,
  "proof_of_work_threshold": -1
}
|json} in
  match json_result with
  | Error err -> raise (Failure err)
  | Ok json ->
      Data_encoding.Binary.to_bytes Data_encoding.json json

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
  "time_between_blocks" : [ 1, 0 ],
  "blocks_per_cycle" : 4,
  "blocks_per_roll_snapshot" : 2,
  "preserved_cycles" : 1,
  "first_free_baking_slot" : 4,
  "blocks_per_voting_period": 2,
  "proof_of_work_threshold": -1
}
|json} in
  match json_result with
  | Error err -> raise (Failure err)
  | Ok json ->
      Data_encoding.Binary.to_bytes Data_encoding.json json

let activate_alpha ?(vote = false) () =
  let fitness = Fitness_repr.from_int64 0L in
  let dictator_sk = Client_keys.Secret_key_locator.create
      ~scheme:"unencrypted"
      ~location:"edsk31vznjHSSpGExDMHYASz45VZqXN4DPxvsa4hAyY8dHM28cZzp6" in
  let protocol_parameters =
    if vote then vote_protocol_parameters else protocol_parameters in
  Tezos_client_genesis.Client_proto_main.bake
    (no_write_context ~block:(`Head 0) !rpc_config) (`Head 0)
    (Activate  { protocol = Proto_alpha.hash ;
                 fitness ;
                 protocol_parameters ;
               })
    dictator_sk

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

let level block =
  Alpha_services.Context.level !rpc_ctxt block

let rpc_raw_context block path depth =
  Block_services.raw_context !rpc_ctxt block path depth

module Account = struct

  type t = {
    alias : string ;
    sk : Ed25519.Secret_key.t ;
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
    Format.fprintf ppf "%s" (Data_encoding.Json.to_string json)

  let create ?keys alias =
    let sk, pk = match keys with
      | Some keys -> keys
      | None -> let _, pk, sk = Ed25519.generate_key () in sk, pk in
    let pkh = Ed25519.Public_key.hash pk in
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
         (req "pk" Ed25519.Public_key.encoding)
         (req "pkh" Ed25519.Public_key_hash.encoding)
         (req "contract" Contract.encoding))

  let pp_destination ppf destination =
    let json = Data_encoding.Json.construct destination_encoding destination in
    Format.fprintf ppf "%s" (Data_encoding.Json.to_string json)

  let create_destination ~alias ~contract ~pk =
    let pkh = Ed25519.Public_key.hash pk in
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
        let sk = Ed25519.Secret_key.of_b58check_exn sk in
        let alias = Printf.sprintf "bootstrap%d" !cpt in
        let pk = Ed25519.Secret_key.to_public_key sk in
        let pkh = Ed25519.Public_key.hash pk in
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
      ~amount () =
    let src_sk = Client_keys.Secret_key_locator.create
        ~scheme:"unencrypted"
        ~location:(Ed25519.Secret_key.to_b58check account.sk) in
    Client_proto_context.transfer
      (new wrap_full (no_write_context !rpc_config ~block))
      block
      ~source:account.contract
      ~src_pk:account.pk
      ~src_sk
      ~destination
      ~amount
      ~fee ()

  let originate
      ?(block = `Head 0)
      ?delegate
      ?(fee = Tez.fifty_cents)
      ~(src:t)
      ~manager_pkh
      ~balance
      () =
    let delegatable, delegate = match delegate with
      | None -> false, None
      | Some delegate -> true, Some delegate in
    let src_sk = Client_keys.Secret_key_locator.create
        ~scheme:"unencrypted"
        ~location:(Ed25519.Secret_key.to_b58check src.sk) in
    Client_proto_context.originate_account
      ~source:src.contract
      ~src_pk:src.pk
      ~src_sk
      ~manager_pkh
      ~balance
      ~delegatable
      ?delegate
      ~fee
      block
      (new wrap_full (no_write_context !rpc_config))
      ()

  let set_delegate
      ?(block = `Head 0)
      ?(fee = Tez.fifty_cents)
      ~contract
      ~manager_sk
      ~src_pk
      delegate_opt =
    Client_proto_context.set_delegate
      (new wrap_full (no_write_context ~block !rpc_config))
      block
      ~fee
      contract
      ~src_pk
      ~manager_sk
      delegate_opt

  let balance ?(block = `Head 0) (account : t) =
    Alpha_services.Contract.balance !rpc_ctxt
      block account.contract

  (* TODO: gather contract related functions in a Contract module? *)
  let delegate ?(block = `Head 0) (contract : Contract.t) =
    Alpha_services.Contract.delegate_opt !rpc_ctxt block contract

end

module Protocol = struct

  open Account

  let voting_period_kind ?(block = `Head 0) () =
    Alpha_services.Context.voting_period_kind !rpc_ctxt block

  let proposals ?(block = `Head 0) ~src:({ pkh; sk } : Account.t) proposals =
    Block_services.info !rpc_ctxt block >>=? fun block_info ->
    Alpha_services.Context.next_level !rpc_ctxt block >>=? fun next_level ->
    Alpha_services.Forge.Amendment.proposals !rpc_ctxt block
      ~branch:block_info.hash
      ~source:pkh
      ~period:next_level.voting_period
      ~proposals
      () >>=? fun bytes ->
    let signed_bytes = Ed25519.append sk bytes in
    return (Tezos_base.Operation.of_bytes_exn signed_bytes)

  let ballot ?(block = `Head 0) ~src:({ pkh; sk } : Account.t) ~proposal ballot =
    Block_services.info !rpc_ctxt block >>=? fun block_info ->
    Alpha_services.Context.next_level !rpc_ctxt block >>=? fun next_level ->
    Alpha_services.Forge.Amendment.ballot !rpc_ctxt block
      ~branch:block_info.hash
      ~source:pkh
      ~period:next_level.voting_period
      ~proposal
      ~ballot
      () >>=? fun bytes ->
    let signed_bytes = Ed25519.append sk bytes in
    return (Tezos_base.Operation.of_bytes_exn signed_bytes)

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
          Ed25519.Public_key_hash.equal pkh1 pkh2
      | _ -> false in
    let prn = function
      | None -> "none"
      | Some pkh -> Ed25519.Public_key_hash.to_b58check pkh in
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
            | Some op ->
                let h = hash op and h' = hash op' in
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
        | Contract_storage.Missing_public_key _ -> true
        | _ -> false)
    end

  let non_delegatable ~msg =
    contain_error ~msg ~f:begin ecoproto_error (function
        | Delegate_storage.Non_delegatable_contract _ -> true
        | _ -> false)
    end

  let check_protocol ?msg ~block h =
    Block_services.protocol !rpc_ctxt block >>=? fun block_proto ->
    return @@ equal
      ?msg
      ~prn:Protocol_hash.to_b58check
      ~eq:Protocol_hash.equal
      block_proto h

  let check_voting_period_kind ?msg ~block kind =
    Alpha_services.Context.voting_period_kind !rpc_ctxt block
    >>=? fun current_kind ->
    return @@ equal
      ?msg
      current_kind kind

  let is_none ?(msg="") x =
    if x <> None then fail "None" "Some _" msg
  let is_some ?(msg="") x =
    if x = None then fail "Some _" "None" msg

end

module Baking = struct

  let bake block (contract: Account.t) operations =
    let ctxt = (new wrap_full (no_write_context ~block !rpc_config)) in
    Alpha_services.Context.next_level ctxt block >>=? fun level ->
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
    let src_sk = Client_keys.Secret_key_locator.create
        ~scheme:"unencrypted"
        ~location:(Ed25519.Secret_key.to_b58check contract.sk) in
    Client_baking_forge.forge_block
      ctxt
      block
      ~operations
      ~force:true
      ~best_effort:false
      ~sort:false
      ~priority:(`Auto (contract.pkh, Some 1024, false))
      ?seed_nonce_hash
      ~src_sk
      ()

end

module Endorse = struct

  let forge_endorsement
      block
      src_sk
      slot =
    Block_services.info !rpc_ctxt block >>=? fun { hash ; _ } ->
    Alpha_services.Context.level !rpc_ctxt (`Hash (hash, 0)) >>=? fun level ->
    Alpha_services.Forge.Consensus.endorsement !rpc_ctxt
      block
      ~branch:hash
      ~block:hash
      ~level:level.level
      ~slots:[slot]
      () >>=? fun bytes ->
    let signed_bytes = Ed25519.append src_sk bytes in
    return (Tezos_base.Operation.of_bytes_exn signed_bytes)

  let signing_slots
      ?(max_priority = 1024)
      block
      delegate
      level =
    Alpha_services.Delegate.Endorser.rights_for_delegate
      !rpc_ctxt ~max_priority ~first_level:level ~last_level:level
      block delegate >>=? fun possibilities ->
    let slots =
      List.map (fun (_,slot) -> slot)
      @@ List.filter (fun (l, _) -> l = level) possibilities in
    return slots

  let endorse
      ?slot
      (contract : Account.t)
      block =
    Alpha_services.Context.level !rpc_ctxt block >>=? fun { level } ->
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
    forge_endorsement block contract.sk slot

  (* FIXME @vb: I don't understand this function, copied from @cago. *)
  let endorsers_list block =
    let get_endorser_list result (account : Account.t) level block =
      Alpha_services.Delegate.Endorser.rights_for_delegate
        !rpc_ctxt block account.pkh
        ~max_priority:16
        ~first_level:level
        ~last_level:level >>|? fun slots ->
      List.iter (fun (_,slot) -> result.(slot) <- account) slots
    in
    let { Account.b1 ; b2 ; b3 ; b4 ; b5 } = Account.bootstrap_accounts in
    let result = Array.make 16 b1 in
    Alpha_services.Context.level !rpc_ctxt block >>=? fun level ->
    let level = level.level in
    get_endorser_list result b1 level block >>=? fun () ->
    get_endorser_list result b2 level block >>=? fun () ->
    get_endorser_list result b3 level block >>=? fun () ->
    get_endorser_list result b4 level block >>=? fun () ->
    get_endorser_list result b5 level block >>=? fun () ->
    return result

  let endorsement_rights
      ?(max_priority = 1024)
      (contract : Account.t) block =
    Alpha_services.Context.level !rpc_ctxt block >>=? fun level ->
    let delegate = contract.pkh in
    let level = level.level in
    Alpha_services.Delegate.Endorser.rights_for_delegate
      !rpc_ctxt
      ~max_priority
      ~first_level:level
      ~last_level:level
      block delegate

end

let display_level block =
  Alpha_services.Context.level !rpc_ctxt block >>=? fun lvl ->
  Format.eprintf "Level: %a@." Level.pp_full lvl ;
  return ()

let endorsement_security_deposit block =
  Constants_services.endorsement_security_deposit !rpc_ctxt block
