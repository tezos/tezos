(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

module Make(Error : sig
    type error
    type error += Exn of exn
    type 'a tzresult = ('a, error list) result
    val pp_print_error: Format.formatter -> error list -> unit
    val error_exn: exn -> ('a, error list) result
    val join: unit tzresult Lwt.t list -> unit tzresult Lwt.t
    val failwith:
      ('a, Format.formatter, unit, 'b tzresult Lwt.t) format4 ->
      'a
  end) : sig

  open Error

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

end
