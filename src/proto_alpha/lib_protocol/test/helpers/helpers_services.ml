(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Proto_alpha.Alpha_context
open Helpers_assert

let endorsement_rights ~tc () =
  let level = Level.current tc in
  Proto_alpha.Services_registration.endorsement_rights tc level None >>=?? fun (_, endorsers) ->
  return @@ List.mapi (fun x i -> x, i) endorsers


let baking_rights ~tc () =
  let level = Level.succ tc @@ Level.current tc in
  Proto_alpha.Services_registration.baking_rights tc level None >>=?? fun (_, bakers) ->
  return @@ List.mapi (fun x i -> x, i) bakers

