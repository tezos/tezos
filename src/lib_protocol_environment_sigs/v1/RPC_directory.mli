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

val map: ('a -> 'b) -> 'b directory -> 'a directory

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
  ('params -> 'query -> 'input -> [< 'output RPC_answer.t ] Lwt.t) ->
  'prefix directory

(** Registring handler in service tree. Curryfied variant.  *)
val register0:
  unit directory ->
  ('m, unit, unit, 'q, 'i, 'o) RPC_service.t ->
  ('q -> 'i -> [< 'o RPC_answer.t ] Lwt.t) ->
  unit directory

val register1:
  'prefix directory ->
  ('m, 'prefix, unit * 'a, 'q , 'i, 'o) RPC_service.t ->
  ('a -> 'q -> 'i -> [< 'o RPC_answer.t ] Lwt.t) ->
  'prefix directory

val register2:
  'prefix directory ->
  ('m, 'prefix, (unit * 'a) * 'b, 'q , 'i, 'o) RPC_service.t ->
  ('a -> 'b -> 'q -> 'i -> [< 'o RPC_answer.t ] Lwt.t) ->
  'prefix directory

val register3:
  'prefix directory ->
  ('m, 'prefix, ((unit * 'a) * 'b) * 'c, 'q , 'i, 'o) RPC_service.t ->
  ('a -> 'b -> 'c -> 'q -> 'i -> [< 'o RPC_answer.t ] Lwt.t) ->
  'prefix directory

val register4:
  'prefix directory ->
  ('m, 'prefix, (((unit * 'a) * 'b) * 'c) * 'd, 'q , 'i, 'o) RPC_service.t ->
  ('a -> 'b -> 'c -> 'd -> 'q -> 'i -> [< 'o RPC_answer.t ] Lwt.t) ->
  'prefix directory

val register5:
  'prefix directory ->
  ('m, 'prefix, ((((unit * 'a) * 'b) * 'c) * 'd) * 'e, 'q , 'i, 'o) RPC_service.t ->
  ('a -> 'b -> 'c -> 'd -> 'e -> 'q -> 'i -> [< 'o RPC_answer.t ] Lwt.t) ->
  'prefix directory
