(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

module Ed25519 = Environment.Ed25519

module RawContractAlias = Client_aliases.Alias (struct
    type t = Contract.t
    let encoding = Contract.encoding
    let of_source s =
      match Contract.of_b48check s with
      | Error _ -> Lwt.fail (Failure "bad contract notation")
      | Ok s -> Lwt.return s
    let to_source s =
      Lwt.return (Contract.to_b48check s)
    let name = "contract"
  end)

module ContractAlias = struct
  let find s =
    RawContractAlias.find_opt s >>= function
    | Some v -> Lwt.return (s, v)
    | None ->
        Client_keys.Public_key_hash.find_opt s >>= function
        | Some v ->
            Lwt.return (s, Contract.default_contract v)
        | None ->
            Cli_entries.error
              "no contract alias nor key alias names %s" s
  let find_key  name =
    Client_keys.Public_key_hash.find name >>= fun v ->
    Lwt.return (name, Contract.default_contract v)

  let rev_find c =
    match Contract.is_default c with
    | Some hash -> begin
        Client_keys.Public_key_hash.rev_find hash >>= function
        | Some name -> Lwt.return (Some ("key:" ^ name))
        | None -> Lwt.return_none
      end
    | None -> RawContractAlias.rev_find c

  let get_contract s =
    match Utils.split ~limit:1 ':' s with
    | [ "key" ; key ]->
        find_key key
    | _ -> find s

  let alias_param ?(name = "name") ?(desc = "existing contract alias") next =
    let desc =
      desc ^ "\n"
      ^ "can be an contract alias or a key alias (autodetected in this order)\n\
         use 'key:name' to force the later" in
    Cli_entries.param ~name ~desc get_contract next

  let destination_param ?(name = "dst") ?(desc = "destination contract") next =
    let desc =
      desc ^ "\n"
      ^ "can be an alias, a key alias, or a literal (autodetected in this order)\n\
         use 'text:literal', 'alias:name', 'key:name' to force" in
    Cli_entries.param ~name ~desc
      (fun s ->
         match Utils.split ~limit:1 ':' s with
         | [ "alias" ; alias ]->
             find alias
         | [ "key" ; text ] ->
             Client_keys.Public_key_hash.find text >>= fun v ->
             Lwt.return (s, Contract.default_contract v)
         | _ ->
             Lwt.catch
               (fun () -> find s)
               (fun _ ->
                  match Contract.of_b48check s with
                  | Error _ -> Lwt.fail (Failure "bad contract notation")
                  | Ok v -> Lwt.return (s, v)))
      next

   let name contract =
     rev_find contract >|= function
     | None -> Contract.to_b48check contract
     | Some name -> name

end

let get_manager block source =
  match Contract.is_default source with
  | Some hash -> return hash
  | None -> Client_proto_rpcs.Context.Contract.manager block source

let get_delegate block source =
  let open Client_keys in
  match Contract.is_default source with
  | Some hash -> return hash
  | None ->
      Client_proto_rpcs.Context.Contract.delegate block source >>=? function
      | Some delegate -> return delegate
      | None -> Client_proto_rpcs.Context.Contract.manager block source

let may_check_key sourcePubKey sourcePubKeyHash =
  match sourcePubKey with
  | Some sourcePubKey ->
      if not (Ed25519.Public_key_hash.equal (Ed25519.hash sourcePubKey) sourcePubKeyHash)
      then
        failwith "Invalid public key in `client_proto_endorsement`"
      else
        return ()
  | None -> return ()

let check_public_key block ?src_pk src_pk_hash =
  Client_proto_rpcs.Context.Key.get block src_pk_hash >>= function
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

let commands  () =
  let open Cli_entries in
  register_group "contracts"
    "Commands for managing the record of known contracts" ;
  [
    command
      ~group: "contracts"
      ~desc: "add a contract to the wallet"
      (prefixes [ "remember" ; "contract" ]
       @@ RawContractAlias.fresh_alias_param
       @@ RawContractAlias.source_param
       @@ stop)
      (fun name hash () -> RawContractAlias.add name hash) ;
    command
      ~group: "contracts"
      ~desc: "remove a contract from the wallet"
      (prefixes [ "forget" ; "contract" ]
       @@ RawContractAlias.alias_param
       @@ stop)
      (fun (name, _) () -> RawContractAlias.del name) ;
    command
      ~group: "contracts"
      ~desc: "lists all known contracts"
      (fixed [ "list" ; "known" ; "contracts" ])
      (fun () ->
         RawContractAlias.load () >>= fun list ->
         Lwt_list.iter_s (fun (n, v) ->
             let v = Contract.to_b48check v in
             message "%s: %s" n v)
           list >>= fun () ->
         Client_keys.Public_key_hash.load () >>= fun list ->
         Lwt_list.iter_s (fun (n, v) ->
             RawContractAlias.mem n >>= fun mem ->
             let p = if mem then "key:" else "" in
             let v = Contract.to_b48check (Contract.default_contract v) in
             message "%s%s: %s" p n v)
           list >>= fun () ->
         Lwt.return ()) ;
    command
      ~group: "contracts"
      ~desc: "forget all known contracts"
      (fixed [ "forget" ; "all" ; "contracts" ])
      (fun () ->
         if not Client_config.force#get then
           error "this can only used with option -force true"
         else
           RawContractAlias.save []) ;
    command
      ~group: "contracts"
      ~desc: "display a contract from the wallet"
      (prefixes [ "show" ; "known" ; "contract" ]
       @@ RawContractAlias.alias_param
       @@ stop)
      (fun (_, contract) () ->
         Cli_entries.message "%a\n%!" Contract.pp contract) ;
  ]
