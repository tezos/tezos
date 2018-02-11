(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Error_monad

include module type of (struct include Resto_directory.Make(RPC_encoding) end)

(** Registring handler in service tree. *)
val register:
  'prefix directory ->
  ([< Resto.meth ], 'prefix, 'p, 'q, 'i, 'o) RPC_service.t ->
  ('p -> 'q -> 'i -> 'o tzresult Lwt.t) ->
  'prefix directory

val opt_register:
  'prefix directory ->
  ([< Resto.meth ], 'prefix, 'p, 'q, 'i, 'o) RPC_service.t ->
  ('p -> 'q -> 'i -> 'o option tzresult Lwt.t) ->
  'prefix directory

val gen_register:
  'prefix directory ->
  ('meth, 'prefix, 'params, 'query, 'input, 'output) RPC_service.t ->
  ('params -> 'query -> 'input -> [< 'output RPC_answer.t ] Lwt.t) ->
  'prefix directory

val lwt_register:
  'prefix directory ->
  ([< Resto.meth ], 'prefix, 'p, 'q, 'i, 'o) RPC_service.t ->
  ('p -> 'q -> 'i -> 'o Lwt.t) ->
  'prefix directory

(** Registring handler in service tree. Curryfied variant.  *)

val register0:
  unit directory ->
  ('m, unit, unit, 'q, 'i, 'o) RPC_service.t ->
  ('q -> 'i -> 'o tzresult Lwt.t) ->
  unit directory

val register1:
  'prefix directory ->
  ('m, 'prefix, unit * 'a, 'q , 'i, 'o) RPC_service.t ->
  ('a -> 'q -> 'i -> 'o tzresult Lwt.t) ->
  'prefix directory

val register2:
  'prefix directory ->
  ('m, 'prefix, (unit * 'a) * 'b, 'q , 'i, 'o) RPC_service.t ->
  ('a -> 'b -> 'q -> 'i -> 'o tzresult Lwt.t) ->
  'prefix directory

val register3:
  'prefix directory ->
  ('m, 'prefix, ((unit * 'a) * 'b) * 'c, 'q , 'i, 'o) RPC_service.t ->
  ('a -> 'b -> 'c -> 'q -> 'i -> 'o tzresult Lwt.t) ->
  'prefix directory

val register4:
  'prefix directory ->
  ('m, 'prefix, (((unit * 'a) * 'b) * 'c) * 'd, 'q , 'i, 'o) RPC_service.t ->
  ('a -> 'b -> 'c -> 'd -> 'q -> 'i -> 'o tzresult Lwt.t) ->
  'prefix directory

val register5:
  'prefix directory ->
  ('m, 'prefix, ((((unit * 'a) * 'b) * 'c) * 'd) * 'e, 'q , 'i, 'o) RPC_service.t ->
  ('a -> 'b -> 'c -> 'd -> 'e -> 'q -> 'i -> 'o tzresult Lwt.t) ->
  'prefix directory


val opt_register0:
  unit directory ->
  ('m, unit, unit, 'q, 'i, 'o) RPC_service.t ->
  ('q -> 'i -> 'o option tzresult Lwt.t) ->
  unit directory

val opt_register1:
  'prefix directory ->
  ('m, 'prefix, unit * 'a, 'q , 'i, 'o) RPC_service.t ->
  ('a -> 'q -> 'i -> 'o option tzresult Lwt.t) ->
  'prefix directory

val opt_register2:
  'prefix directory ->
  ('m, 'prefix, (unit * 'a) * 'b, 'q , 'i, 'o) RPC_service.t ->
  ('a -> 'b -> 'q -> 'i -> 'o option tzresult Lwt.t) ->
  'prefix directory

val opt_register3:
  'prefix directory ->
  ('m, 'prefix, ((unit * 'a) * 'b) * 'c, 'q , 'i, 'o) RPC_service.t ->
  ('a -> 'b -> 'c -> 'q -> 'i -> 'o option tzresult Lwt.t) ->
  'prefix directory

val opt_register4:
  'prefix directory ->
  ('m, 'prefix, (((unit * 'a) * 'b) * 'c) * 'd, 'q , 'i, 'o) RPC_service.t ->
  ('a -> 'b -> 'c -> 'd -> 'q -> 'i -> 'o option tzresult Lwt.t) ->
  'prefix directory

val opt_register5:
  'prefix directory ->
  ('m, 'prefix, ((((unit * 'a) * 'b) * 'c) * 'd) * 'e, 'q , 'i, 'o) RPC_service.t ->
  ('a -> 'b -> 'c -> 'd -> 'e -> 'q -> 'i -> 'o option tzresult Lwt.t) ->
  'prefix directory


val gen_register0:
  unit directory ->
  ('m, unit, unit, 'q, 'i, 'o) RPC_service.t ->
  ('q -> 'i -> [< 'o RPC_answer.t ] Lwt.t) ->
  unit directory

val gen_register1:
  'prefix directory ->
  ('m, 'prefix, unit * 'a, 'q , 'i, 'o) RPC_service.t ->
  ('a -> 'q -> 'i -> [< 'o RPC_answer.t ] Lwt.t) ->
  'prefix directory

val gen_register2:
  'prefix directory ->
  ('m, 'prefix, (unit * 'a) * 'b, 'q , 'i, 'o) RPC_service.t ->
  ('a -> 'b -> 'q -> 'i -> [< 'o RPC_answer.t ] Lwt.t) ->
  'prefix directory

val gen_register3:
  'prefix directory ->
  ('m, 'prefix, ((unit * 'a) * 'b) * 'c, 'q , 'i, 'o) RPC_service.t ->
  ('a -> 'b -> 'c -> 'q -> 'i -> [< 'o RPC_answer.t ] Lwt.t) ->
  'prefix directory

val gen_register4:
  'prefix directory ->
  ('m, 'prefix, (((unit * 'a) * 'b) * 'c) * 'd, 'q , 'i, 'o) RPC_service.t ->
  ('a -> 'b -> 'c -> 'd -> 'q -> 'i -> [< 'o RPC_answer.t ] Lwt.t) ->
  'prefix directory

val gen_register5:
  'prefix directory ->
  ('m, 'prefix, ((((unit * 'a) * 'b) * 'c) * 'd) * 'e, 'q , 'i, 'o) RPC_service.t ->
  ('a -> 'b -> 'c -> 'd -> 'e -> 'q -> 'i -> [< 'o RPC_answer.t ] Lwt.t) ->
  'prefix directory


val lwt_register0:
  unit directory ->
  ('m, unit, unit, 'q, 'i, 'o) RPC_service.t ->
  ('q -> 'i -> 'o Lwt.t) ->
  unit directory

val lwt_register1:
  'prefix directory ->
  ('m, 'prefix, unit * 'a, 'q , 'i, 'o) RPC_service.t ->
  ('a -> 'q -> 'i -> 'o Lwt.t) ->
  'prefix directory

val lwt_register2:
  'prefix directory ->
  ('m, 'prefix, (unit * 'a) * 'b, 'q , 'i, 'o) RPC_service.t ->
  ('a -> 'b -> 'q -> 'i -> 'o Lwt.t) ->
  'prefix directory

val lwt_register3:
  'prefix directory ->
  ('m, 'prefix, ((unit * 'a) * 'b) * 'c, 'q , 'i, 'o) RPC_service.t ->
  ('a -> 'b -> 'c -> 'q -> 'i -> 'o Lwt.t) ->
  'prefix directory

val lwt_register4:
  'prefix directory ->
  ('m, 'prefix, (((unit * 'a) * 'b) * 'c) * 'd, 'q , 'i, 'o) RPC_service.t ->
  ('a -> 'b -> 'c -> 'd -> 'q -> 'i -> 'o Lwt.t) ->
  'prefix directory

val lwt_register5:
  'prefix directory ->
  ('m, 'prefix, ((((unit * 'a) * 'b) * 'c) * 'd) * 'e, 'q , 'i, 'o) RPC_service.t ->
  ('a -> 'b -> 'c -> 'd -> 'e -> 'q -> 'i -> 'o Lwt.t) ->
  'prefix directory


