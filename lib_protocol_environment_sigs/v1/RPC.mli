(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(** View over the RPC service, restricted to types. A protocol
    implementation can define a set of remote procedures which are
    registered when the protocol is activated via its [rpcs]
    function. However, it cannot register new or update existing
    procedures afterwards, neither can it see other procedures. *)

(** HTTP methods. *)
type meth = [
  | `GET
  | `POST
  | `DELETE
  | `PUT
  | `PATCH
]

(** Typed path argument. *)
module Arg : sig

  type 'a t
  type 'a arg = 'a t
  val make:
    ?descr:string ->
    name:string ->
    destruct:(string -> ('a, string) result) ->
    construct:('a -> string) ->
    unit -> 'a arg

  type descr = {
    name: string ;
    descr: string option ;
  }
  val descr: 'a arg -> descr

  val int: int arg
  val int32: int32 arg
  val int64: int64 arg
  val float: float arg

end

(** Parametrized path to services. *)
module Path : sig

  type ('prefix, 'params) t
  type ('prefix, 'params) path = ('prefix, 'params) t
  type 'prefix context = ('prefix, 'prefix) path

  val root: unit context
  val open_root: 'a context

  val add_suffix:
    ('prefix, 'params) path -> string -> ('prefix, 'params) path
  val (/):
    ('prefix, 'params) path -> string -> ('prefix, 'params) path

  val add_arg:
    ('prefix, 'params) path -> 'a Arg.t -> ('prefix, 'params * 'a) path
  val (/:):
    ('prefix, 'params) path -> 'a Arg.t -> ('prefix, 'params * 'a) path

  val add_final_args:
    ('prefix, 'params) path -> 'a Arg.t -> ('prefix, 'params * 'a list) path
  val (/:*):
    ('prefix, 'params) path -> 'a Arg.t -> ('prefix, 'params * 'a list) path

  val prefix:
    ('prefix, 'a) path -> ('a, 'params) path -> ('prefix, 'params) path

  val map:
    ('a -> 'b) -> ('b -> 'a) -> ('prefix, 'a) path -> ('prefix, 'b) path

end

module Query : sig

  type 'a t
  type 'a query = 'a t

  val empty: unit query

  type ('a, 'b) field
  val field:
    ?descr: string ->
    string -> 'a Arg.t -> 'a -> ('b -> 'a) -> ('b, 'a) field

  type ('a, 'b, 'c) open_query
  val query: 'b -> ('a, 'b, 'b) open_query
  val (|+):
    ('a, 'b, 'c -> 'd) open_query ->
    ('a, 'c) field -> ('a, 'b, 'd) open_query
  val seal: ('a, 'b, 'a) open_query -> 'a t

  type untyped = (string * string) list
  exception Invalid of string
  val parse: 'a query -> untyped -> 'a

end

(** Services. *)
module Service : sig

  type (+'meth, 'prefix, 'params, 'query, 'input, 'output, 'error) t
    constraint 'meth = [< meth ]
  type (+'meth, 'prefix, 'params, 'query, 'input, 'output, 'error) service =
    ('meth, 'prefix, 'params, 'query, 'input, 'output, 'error) t

  val query:
    ('meth, 'prefix, 'params, 'query, 'input, 'output, 'error) service ->
    'query Query.t

  type _ input =
    | No_input : unit input
    | Input : 'input Data_encoding.t -> 'input input

  val input_encoding:
    ('meth, 'prefix, 'params, 'query, 'input, 'output, 'error) service ->
    'input input

  val output_encoding:
    ('meth, 'prefix, 'params, 'query, 'input, 'output, 'error) service ->
    'output Data_encoding.t

  val error_encoding:
    ('meth, 'prefix, 'params, 'query, 'input, 'output, 'error) service ->
    'error Data_encoding.t

  val prefix:
    ('prefix, 'inner_prefix) Path.t ->
    ('meth, 'inner_prefix, 'params, 'query,
     'input, 'output, 'error) service ->
    ('meth, 'prefix, 'params,
     'query, 'input, 'output, 'error) service

  val map:
    ('a -> 'b) ->
    ('b -> 'a) ->
    ('meth, 'pr, 'a, 'q, 'i, 'o, 'e) service ->
    ('meth, 'pr, 'b, 'q, 'i, 'o, 'e) service

  val get_service:
    ?description: string ->
    query: 'query Query.t ->
    output: 'output Data_encoding.t ->
    error: 'error Data_encoding.t ->
    ('prefix, 'params) Path.t ->
    ([ `GET ], 'prefix, 'params, 'query, unit, 'output, 'error) service

  val post_service:
    ?description: string ->
    query:'query Query.t ->
    input: 'input Data_encoding.t ->
    output: 'output Data_encoding.t ->
    error: 'error Data_encoding.t ->
    ('prefix, 'params) Path.t ->
    ([ `POST ], 'prefix, 'params, 'query, 'input, 'output, 'error) service

  val delete_service:
    ?description: string ->
    query:'query Query.t ->
    output: 'output Data_encoding.t ->
    error: 'error Data_encoding.t ->
    ('prefix, 'params) Path.t ->
    ([ `DELETE ], 'prefix, 'params, 'query, unit, 'output, 'error) service

  val patch_service:
    ?description: string ->
    query:'query Query.t ->
    input: 'input Data_encoding.t ->
    output: 'output Data_encoding.t ->
    error: 'error Data_encoding.t ->
    ('prefix, 'params) Path.t ->
    ([ `PATCH ], 'prefix, 'params, 'query, 'input, 'output, 'error) service

  val put_service:
    ?description: string ->
    query:'query Query.t ->
    input: 'input Data_encoding.t ->
    output: 'output Data_encoding.t ->
    error: 'error Data_encoding.t ->
    ('prefix, 'params) Path.t ->
    ([ `PUT ], 'prefix, 'params, 'query, 'input, 'output, 'error) service

end

module Answer : sig

  (** Return type for service handler *)
  type ('o, 'e) t =
    [ `Ok of 'o (* 200 *)
    | `OkStream of 'o stream (* 200 *)
    | `Created of string option (* 201 *)
    | `No_content (* 204 *)
    | `Unauthorized of 'e option (* 401 *)
    | `Forbidden of 'e option (* 403 *)
    | `Not_found of 'e option (* 404 *)
    | `Conflict of 'e option (* 409 *)
    | `Error of 'e option (* 500 *)
    ]

  and 'a stream = {
    next: unit -> 'a option Lwt.t ;
    shutdown: unit -> unit ;
  }

  val return: 'o -> ('o, 'e) t Lwt.t
  val return_stream: 'o stream -> ('o, 'e) t Lwt.t

end

module Directory : sig

  (** Dispatch tree *)
  type 'prefix t
  type 'prefix directory = 'prefix t

  (** Empty list of dispatch trees *)
  val empty: 'prefix directory

  val map: ('a -> 'b) -> 'b directory -> 'a directory

  val prefix: ('pr, 'p) Path.path -> 'p directory -> 'pr directory
  val merge: 'a directory -> 'a directory -> 'a directory

  (** Possible error while registring services. *)
  type step =
    | Static of string
    | Dynamic of Arg.descr
    | DynamicTail of Arg.descr

  type conflict =
    | CService of meth | CDir | CBuilder | CTail
    | CTypes of Arg.descr *
                Arg.descr
    | CType of Arg.descr * string list
  exception Conflict of step list * conflict

  (** Registring handler in service tree. *)
  val register:
    'prefix directory ->
    ('meth, 'prefix, 'params, 'query, 'input, 'output, 'error) Service.t ->
    ('params -> 'query -> 'input -> [< ('output, 'error) Answer.t ] Lwt.t) ->
    'prefix directory

  (** Registring handler in service tree. Curryfied variant.  *)
  val register0:
    unit directory ->
    ('m, unit, unit, 'q, 'i, 'o, 'e) Service.t ->
    ('q -> 'i -> [< ('o, 'e) Answer.t ] Lwt.t) ->
    unit directory

  val register1:
    'prefix directory ->
    ('m, 'prefix, unit * 'a, 'q , 'i, 'o, 'e) Service.t ->
    ('a -> 'q -> 'i -> [< ('o, 'e) Answer.t ] Lwt.t) ->
    'prefix directory

  val register2:
    'prefix directory ->
    ('m, 'prefix, (unit * 'a) * 'b, 'q , 'i, 'o, 'e) Service.t ->
    ('a -> 'b -> 'q -> 'i -> [< ('o, 'e) Answer.t ] Lwt.t) ->
    'prefix directory

  val register3:
    'prefix directory ->
    ('m, 'prefix, ((unit * 'a) * 'b) * 'c, 'q , 'i, 'o, 'e) Service.t ->
    ('a -> 'b -> 'c -> 'q -> 'i -> [< ('o, 'e) Answer.t ] Lwt.t) ->
    'prefix directory

  val register4:
    'prefix directory ->
    ('m, 'prefix, (((unit * 'a) * 'b) * 'c) * 'd, 'q , 'i, 'o, 'e) Service.t ->
    ('a -> 'b -> 'c -> 'd -> 'q -> 'i -> [< ('o, 'e) Answer.t ] Lwt.t) ->
    'prefix directory

  val register5:
    'prefix directory ->
    ('m, 'prefix, ((((unit * 'a) * 'b) * 'c) * 'd) * 'e, 'q , 'i, 'o, 'e) Service.t ->
    ('a -> 'b -> 'c -> 'd -> 'e -> 'q -> 'i -> [< ('o, 'e) Answer.t ] Lwt.t) ->
    'prefix directory

end

(** Compatibility layer, to be removed ASAP. *)

type 'a directory = 'a Directory.t

val service:
  ?description: string ->
  input: 'input Data_encoding.t ->
  output: 'output Data_encoding.t ->
  ('prefix, 'params) Path.t ->
  ([ `POST], 'prefix, 'params, unit, 'input, 'output, unit) Service.t

val register:
  'prefix directory ->
  ([ `POST ], 'prefix,
   'params, unit, 'input,
   'output, unit) Service.t ->
  ('params -> 'input -> [< ('output, unit) Answer.t ] Lwt.t) ->
  'prefix directory

val register0:
  unit directory ->
  ([ `POST ], unit,
   unit, unit, 'i,
   'o, unit) Service.t ->
  ('i -> [< ('o, unit) Answer.t ] Lwt.t) ->
  unit directory

val register1:
  'prefix directory ->
  ([ `POST ], 'prefix,
   unit * 'a, unit, 'i,
   'o, unit) Service.t ->
  ('a -> 'i -> [< ('o, unit) Answer.t ] Lwt.t) ->
  'prefix directory

val register2:
  'prefix directory ->
  ([ `POST ], 'prefix,
   (unit * 'a) * 'b, unit, 'i,
   'o, unit) Service.t ->
  ('a -> 'b -> 'i -> [< ('o, unit) Answer.t ] Lwt.t) ->
  'prefix directory
