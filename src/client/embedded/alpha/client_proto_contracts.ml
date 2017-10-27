(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

module Ed25519 = Environment.Ed25519

module ContractEntity = struct
  type t = Contract.t
  let encoding = Contract.encoding
  let of_source _ s =
    match Contract.of_b58check s with
    | Error _ as err ->
        Lwt.return (Environment.wrap_error err)
        |> trace (failure "bad contract notation")
    | Ok s -> return s
  let to_source _ s = return (Contract.to_b58check s)
  let name = "contract"
end

module RawContractAlias = Client_aliases.Alias (ContractEntity)

module ContractAlias = struct

  let find cctxt s =
    RawContractAlias.find_opt cctxt s >>=? function
    | Some v -> return (s, v)
    | None ->
        Client_keys.Public_key_hash.find_opt cctxt s >>=? function
        | Some v ->
            return (s, Contract.default_contract v)
        | None ->
            failwith "no contract or key named %s" s

  let find_key cctxt name =
    Client_keys.Public_key_hash.find cctxt name >>=? fun v ->
    return (name, Contract.default_contract v)

  let rev_find cctxt c =
    match Contract.is_default c with
    | Some hash -> begin
        Client_keys.Public_key_hash.rev_find cctxt hash >>=? function
        | Some name -> return (Some ("key:" ^ name))
        | None -> return None
      end
    | None -> RawContractAlias.rev_find cctxt c

  let get_contract cctxt s =
    match Utils.split ~limit:1 ':' s with
    | [ "key" ; key ]->
        find_key cctxt key
    | _ -> find cctxt s

  let alias_param ?(name = "name") ?(desc = "existing contract alias") next =
    let desc =
      desc ^ "\n"
      ^ "can be an contract alias or a key alias (autodetected in this order)\n\
         use 'key:name' to force the later" in
    Cli_entries.param ~name ~desc
      (fun cctxt p -> get_contract cctxt p)
      next

  let destination_param ?(name = "dst") ?(desc = "destination contract") next =
    let desc =
      desc ^ "\n"
      ^ "can be an alias, a key alias, or a literal (autodetected in this order)\n\
         use 'text:literal', 'alias:name', 'key:name' to force" in
    Cli_entries.param ~name ~desc
      (fun cctxt s ->
         begin
           match Utils.split ~limit:1 ':' s with
           | [ "alias" ; alias ]->
               find cctxt alias
           | [ "key" ; text ] ->
               Client_keys.Public_key_hash.find cctxt text >>=? fun v ->
               return (s, Contract.default_contract v)
           | _ ->
               find cctxt s >>= function
               | Ok v -> return v
               | Error k_errs ->
                   ContractEntity.of_source cctxt s >>= function
                   | Ok v -> return (s, v)
                   | Error c_errs -> Lwt.return (Error (k_errs @ c_errs))
         end)
      next

   let name cctxt contract =
     rev_find cctxt contract >>=? function
     | None -> return (Contract.to_b58check contract)
     | Some name -> return name

end

module Contract_tags = Client_tags.Tags (struct
    let name = "contract"
  end)

let list_contracts cctxt =
  (* List contracts *)
  RawContractAlias.load cctxt >>=? fun raw_contracts ->
  Lwt_list.map_s
    (fun (n, v) -> Lwt.return ("", n, v))
    raw_contracts >>= fun contracts ->
  Client_keys.Public_key_hash.load cctxt >>=? fun keys ->
  (* List accounts (default contracts of identities) *)
  map_s (fun (n, v) ->
      RawContractAlias.mem cctxt n >>=? fun mem ->
      let p = if mem then "key:" else "" in
      let v' = Contract.default_contract v in
      return (p, n, v'))
    keys >>=? fun accounts ->
  return (contracts @ accounts)

let get_manager cctxt block source =
  match Contract.is_default source with
  | Some hash -> return hash
  | None -> Client_proto_rpcs.Context.Contract.manager cctxt block source

let get_delegate cctxt block source =
  let open Client_keys in
  match Contract.is_default source with
  | Some hash -> return hash
  | None ->
      Client_proto_rpcs.Context.Contract.delegate cctxt
        block source >>=? function
      | Some delegate ->
          return delegate
      | None ->
          Client_proto_rpcs.Context.Contract.manager cctxt block source

let may_check_key sourcePubKey sourcePubKeyHash =
  match sourcePubKey with
  | Some sourcePubKey ->
      fail_unless
        (Ed25519.Public_key_hash.equal
           (Ed25519.Public_key.hash sourcePubKey) sourcePubKeyHash)
        (failure "Invalid public key in `client_proto_endorsement`")
  | None ->
      return ()

let check_public_key cctxt block ?src_pk src_pk_hash =
  Client_proto_rpcs.Context.Key.get cctxt block src_pk_hash >>= function
  | Error errors ->
      begin
        match src_pk with
        | None ->
            let exn = Client_proto_rpcs.string_of_errors errors in
            failwith "Unknown public key\n%s" exn
        | Some key ->
            may_check_key src_pk src_pk_hash >>=? fun () ->
            return (Some key)
      end
  | Ok _ -> return None

let group =
  { Cli_entries.name = "contracts" ;
    title = "Commands for managing the record of known contracts" }

let commands  () =
  let open Cli_entries in
  let open Client_commands in
  [

    command ~group ~desc: "add a contract to the wallet"
      no_options
      (prefixes [ "remember" ; "contract" ]
       @@ RawContractAlias.fresh_alias_param
       @@ RawContractAlias.source_param
       @@ stop)
      (fun () name hash cctxt ->
         RawContractAlias.add cctxt name hash) ;

    command ~group ~desc: "remove a contract from the wallet"
      no_options
      (prefixes [ "forget" ; "contract" ]
       @@ RawContractAlias.alias_param
       @@ stop)
      (fun () (name, _) cctxt -> RawContractAlias.del cctxt name) ;

    command ~group ~desc: "lists all known contracts"
      no_options
      (fixed [ "list" ; "known" ; "contracts" ])
      (fun () cctxt ->
         list_contracts cctxt >>=? fun contracts ->
         iter_s
           (fun (prefix, alias, contract) ->
              cctxt.message "%s%s: %s" prefix alias
                (Contract.to_b58check contract) >>= fun () ->
              return ())
           contracts) ;

    command ~group ~desc: "forget all known contracts"
      no_options
      (fixed [ "forget" ; "all" ; "contracts" ])
      (fun () cctxt ->
         fail_unless
           cctxt.config.force
           (failure "this can only used with option -force true") >>=? fun () ->
         RawContractAlias.save cctxt []) ;

    command ~group ~desc: "display a contract from the wallet"
      no_options
      (prefixes [ "show" ; "known" ; "contract" ]
       @@ RawContractAlias.alias_param
       @@ stop)
      (fun () (_, contract) cctxt ->
         cctxt.message "%a\n%!" Contract.pp contract >>= fun () ->
         return ()) ;

    command ~group ~desc: "tag a contract in the wallet"
      no_options
      (prefixes [ "tag" ; "contract" ]
       @@ RawContractAlias.alias_param
       @@ prefixes [ "with" ]
       @@ Contract_tags.tag_param
       @@ stop)
      (fun () (alias, _contract) new_tags cctxt ->
         Contract_tags.find_opt cctxt alias >>=? fun tags ->
         let new_tags =
           match tags with
           | None -> new_tags
           | Some tags -> Utils.merge_list2 tags new_tags in
         Contract_tags.update cctxt alias new_tags) ;

    command ~group ~desc: "remove tag(s) from a contract in the wallet"
      no_options
      (prefixes [ "untag" ; "contract" ]
       @@ RawContractAlias.alias_param
       @@ prefixes [ "with" ]
       @@ Contract_tags.tag_param
       @@ stop)
      (fun () (alias, _contract) new_tags cctxt ->
         Contract_tags.find_opt cctxt alias >>=? fun tags ->
         let new_tags =
           match tags with
           | None -> []
           | Some tags ->
               Utils.merge_filter_list2
                 ~f:(fun x1 x2 -> match x1, x2 with
                     | None, None -> assert false
                     | None, Some _ -> None
                     | Some t1, Some t2 when t1 = t2 -> None
                     | Some t1, _ -> Some t1) tags new_tags in
         Contract_tags.update cctxt alias new_tags) ;

  ]
