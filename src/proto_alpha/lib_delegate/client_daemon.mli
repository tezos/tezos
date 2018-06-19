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

module Endorser : sig
  val run:
    #Proto_alpha.full ->
    delay: int ->
    ?min_date: Time.t ->
    public_key_hash list -> unit tzresult Lwt.t
end

module Baker : sig
  val run:
    #Proto_alpha.full ->
    ?max_priority: int ->
    ?min_date: Time.t ->
    context_path: string ->
    public_key_hash list -> unit tzresult Lwt.t
end

module Accuser : sig
  val run:
    #Proto_alpha.full -> unit tzresult Lwt.t
end
