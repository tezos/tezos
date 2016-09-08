(** View over the RPC service, restricted to types. A protocol
    implementation can define a set of remote procedures which are
    registered when the protocol is activated via its [rpcs]
    function. However, it cannot register new or update existing
    procedures afterwards, neither can it see other procedures. *)

(** Typed path argument. *)
module Arg : sig

  type 'a arg
  val make:
    ?descr:string ->
    name:string ->
    destruct:(string -> ('a, string) result) ->
    construct:('a -> string) ->
    'a arg

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

  type ('prefix, 'params) path
  type 'prefix context = ('prefix, 'prefix) path

  val root: 'a context

  val add_suffix:
    ('prefix, 'params) path -> string -> ('prefix, 'params) path
  val (/):
    ('prefix, 'params) path -> string -> ('prefix, 'params) path

  val add_arg:
    ('prefix, 'params) path -> 'a Arg.arg -> ('prefix, 'params * 'a) path
  val (/:):
    ('prefix, 'params) path -> 'a Arg.arg -> ('prefix, 'params * 'a) path

  val prefix:
    ('prefix, 'a) path -> ('a, 'params) path -> ('prefix, 'params) path

  val map:
    ('a -> 'b) -> ('b -> 'a) -> ('prefix, 'a) path -> ('prefix, 'b) path

end

(** Services. *)
type ('prefix, 'params, 'input, 'output) service

val service:
  ?description: string ->
  input: 'input Data_encoding.t ->
  output: 'output Data_encoding.t ->
  ('prefix, 'params) Path.path ->
  ('prefix, 'params, 'input, 'output) service

module Answer : sig

  (** Return type for service handler *)
  type 'a answer =
    { code : int ;
      body : 'a output ;
    }

  and 'a output =
    | Empty
    | Single of 'a
    | Stream of 'a stream

  and 'a stream = {
    next: unit -> 'a option Lwt.t ;
    shutdown: unit -> unit ;
  }

  val ok: 'a -> 'a answer
  val return: 'a -> 'a answer Lwt.t

end

(** Dispatch tree *)
type 'prefix directory

val empty: 'prefix directory

(** Registring handler in service tree. *)
val register:
  'prefix directory ->
  ('prefix, 'params, 'input, 'output) service ->
  ('params -> 'input -> 'output Answer.answer Lwt.t) ->
  'prefix directory

(** Registring handler in service tree. Curryfied variant.  *)
val register0:
  unit directory ->
  (unit, unit, 'i, 'o) service ->
  ('i -> 'o Answer.answer Lwt.t) ->
  unit directory

val register1:
  'prefix directory ->
  ('prefix, unit * 'a, 'i, 'o) service ->
  ('a -> 'i -> 'o Answer.answer Lwt.t) ->
  'prefix directory

val register2:
  'prefix directory ->
  ('prefix, (unit * 'a) * 'b, 'i, 'o) service ->
  ('a -> 'b -> 'i -> 'o Answer.answer Lwt.t) ->
  'prefix directory

val register3:
  'prefix directory ->
  ('prefix, ((unit * 'a) * 'b) * 'c, 'i, 'o) service ->
  ('a -> 'b -> 'c -> 'i -> 'o Answer.answer Lwt.t) ->
  'prefix directory

val register4:
  'prefix directory ->
  ('prefix, (((unit * 'a) * 'b) * 'c) * 'd, 'i, 'o) service ->
  ('a -> 'b -> 'c -> 'd -> 'i -> 'o Answer.answer Lwt.t) ->
  'prefix directory

val register5:
  'prefix directory ->
  ('prefix, ((((unit * 'a) * 'b) * 'c) * 'd) * 'e, 'i, 'o) service ->
  ('a -> 'b -> 'c -> 'd -> 'e -> 'i -> 'o Answer.answer Lwt.t) ->
  'prefix directory
