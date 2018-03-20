(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Alpha_context

val operations:
  'a #RPC_context.simple -> 'a -> (Operation_hash.t * Operation.t) list list shell_tzresult Lwt.t
val header:
  'a #RPC_context.simple -> 'a -> Block_header.t shell_tzresult Lwt.t
val priority:
  'a #RPC_context.simple -> 'a -> int shell_tzresult Lwt.t
val seed_nonce_hash:
  'a #RPC_context.simple -> 'a -> Nonce_hash.t shell_tzresult Lwt.t

module Context : sig

  val level:
    'a #RPC_context.simple -> 'a -> Level.t shell_tzresult Lwt.t
  (** [level cctxt blk] returns the (protocol view of the) level of
      [blk]. *)

  val next_level:
    'a #RPC_context.simple -> 'a -> Level.t shell_tzresult Lwt.t
  (** [next_level cctxt blk] returns the (protocol view of the) level
      of the successor of [blk]. *)

  val voting_period_kind:
    'a #RPC_context.simple -> 'a -> Voting_period.kind shell_tzresult Lwt.t
    (** [voting_period_kind cctxt blk] returns the voting period kind
        of [blk]. *)

end

module Nonce : sig

  val hash:
    'a #RPC_context.simple ->
    'a -> Nonce_hash.t shell_tzresult Lwt.t

  type info =
    | Revealed of Nonce.t
    | Missing of Nonce_hash.t
    | Forgotten

  val get:
    'a #RPC_context.simple ->
    'a -> Raw_level.t -> info shell_tzresult Lwt.t

end

module Contract = Contract_services
module Constants = Constants_services
module Delegate = Delegate_services
module Helpers = Helpers_services
module Forge = Helpers_services.Forge
module Parse = Helpers_services.Parse
