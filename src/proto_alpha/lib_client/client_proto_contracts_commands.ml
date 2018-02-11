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
open Client_proto_contracts

let group =
  { Cli_entries.name = "contracts" ;
    title = "Commands for managing the record of known contracts" }

let commands  () =
  let open Cli_entries in
  [

    command ~group ~desc: "Add a contract to the wallet."
      (args1 (RawContractAlias.force_switch ()))
      (prefixes [ "remember" ; "contract" ]
       @@ RawContractAlias.fresh_alias_param
       @@ RawContractAlias.source_param
       @@ stop)
      (fun force name hash cctxt ->
         RawContractAlias.of_fresh cctxt force name >>=? fun name ->
         RawContractAlias.add ~force cctxt name hash) ;

    command ~group ~desc: "Remove a contract from the wallet."
      no_options
      (prefixes [ "forget" ; "contract" ]
       @@ RawContractAlias.alias_param
       @@ stop)
      (fun () (name, _) cctxt ->
         RawContractAlias.del cctxt name) ;

    command ~group ~desc: "Lists all known contracts in the wallet."
      no_options
      (fixed [ "list" ; "known" ; "contracts" ])
      (fun () (cctxt : Proto_alpha.full_context) ->
         list_contracts cctxt >>=? fun contracts ->
         iter_s
           (fun (prefix, alias, contract) ->
              cctxt#message "%s%s: %s" prefix alias
                (Contract.to_b58check contract) >>= return)
           contracts) ;

    command ~group ~desc: "Forget the entire wallet of known contracts."
      (args1 (RawContractAlias.force_switch ()))
      (fixed [ "forget" ; "all" ; "contracts" ])
      (fun force cctxt ->
         fail_unless
           force
           (failure "this can only used with option -force") >>=? fun () ->
         RawContractAlias.set cctxt []) ;

    command ~group ~desc: "Display a contract from the wallet."
      no_options
      (prefixes [ "show" ; "known" ; "contract" ]
       @@ RawContractAlias.alias_param
       @@ stop)
      (fun () (_, contract) (cctxt : Proto_alpha.full_context) ->
         cctxt#message "%a\n%!" Contract.pp contract >>= fun () ->
         return ()) ;

    command ~group ~desc: "Tag a contract in the wallet."
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
           | Some tags -> List.merge2 tags new_tags in
         Contract_tags.update cctxt alias new_tags) ;

    command ~group ~desc: "Remove tag(s) from a contract in the wallet."
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
               List.merge_filter2
                 ~f:(fun x1 x2 -> match x1, x2 with
                     | None, None -> assert false
                     | None, Some _ -> None
                     | Some t1, Some t2 when t1 = t2 -> None
                     | Some t1, _ -> Some t1) tags new_tags in
         Contract_tags.update cctxt alias new_tags) ;

  ]
