(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Proto_alpha
open Alpha_context
open Helpers_assert

let endorsement_rights ~tc () =
  let level = Level.current tc in
  Alpha_services.Delegate.endorsement_rights tc level None >>=?? fun (_, endorsers) ->
  return @@ List.mapi (fun x i -> x, i) endorsers


let baking_rights ~tc () =
  Alpha_services.Delegate.baking_rights tc () None >>=?? fun (_, bakers) ->
  return @@ List.mapi (fun x (i,_) -> x, i) bakers

