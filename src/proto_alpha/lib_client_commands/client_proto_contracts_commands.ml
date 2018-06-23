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
  { Clic.name = "contracts" ;
    title = "Commands for managing the record of known contracts" }

let commands  () =
  let open Clic in
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
      (fun () (cctxt : Proto_alpha.full) ->
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
      (fun () (_, contract) (cctxt : Proto_alpha.full) ->
         cctxt#message "%a\n%!" Contract.pp contract >>= fun () ->
         return ()) ;

  ]
