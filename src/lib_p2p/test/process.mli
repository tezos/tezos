(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Error_monad

exception Exited of int

module Channel : sig
  type ('a, 'b) t
  val push: ('a, 'b) t -> 'a -> unit tzresult Lwt.t
  val pop: ('a, 'b) t -> 'b tzresult Lwt.t
end

type ('a, 'b) t = {
  termination: unit tzresult Lwt.t ;
  channel: ('b, 'a) Channel.t ;
}

val detach:
  ?prefix:string ->
  (('a, 'b) Channel.t -> unit tzresult Lwt.t) ->
  ('a, 'b) t Lwt.t

val wait_all: ('a, 'b) t list -> unit tzresult Lwt.t
