(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(** Dispatch tree *)
type 'prefix t
type 'prefix directory = 'prefix t

(** Empty list of dispatch trees *)
val empty: 'prefix directory

val map: ('a -> 'b Lwt.t) -> 'b directory -> 'a directory

val prefix: ('pr, 'p) RPC_path.path -> 'p directory -> 'pr directory
val merge: 'a directory -> 'a directory -> 'a directory

(** Possible error while registring services. *)
type step =
  | Static of string
  | Dynamic of RPC_arg.descr
  | DynamicTail of RPC_arg.descr

type conflict =
  | CService of RPC_service.meth | CDir | CBuilder | CTail
  | CTypes of RPC_arg.descr *
              RPC_arg.descr
  | CType of RPC_arg.descr * string list
exception Conflict of step list * conflict

(** Registring handler in service tree. *)
val register:
  'prefix directory ->
  ('meth, 'prefix, 'params, 'query, 'input, 'output) RPC_service.t ->
  ('params -> 'query -> 'input -> 'output tzresult Lwt.t) ->
  'prefix directory

val opt_register:
  'prefix directory ->
  ('meth, 'prefix, 'params, 'query, 'input, 'output) RPC_service.t ->
  ('params -> 'query -> 'input -> 'output option tzresult Lwt.t) ->
  'prefix directory

val gen_register:
  'prefix directory ->
  ('meth, 'prefix, 'params, 'query, 'input, 'output) RPC_service.t ->
  ('params -> 'query -> 'input -> [< 'output RPC_answer.t ] Lwt.t) ->
  'prefix directory

val lwt_register:
  'prefix directory ->
  ('meth, 'prefix, 'params, 'query, 'input, 'output) RPC_service.t ->
  ('params -> 'query -> 'input -> 'output Lwt.t) ->
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

(** Registring dynamic subtree. *)
val register_dynamic_directory:
  ?descr:string ->
  'prefix directory ->
  ('prefix, 'a) RPC_path.t -> ('a -> 'a directory Lwt.t) ->
  'prefix directory
