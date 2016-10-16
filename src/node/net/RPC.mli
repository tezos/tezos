(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
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

(** Typed path argument. *)
module Arg : sig

  type 'a arg
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

val prefix:
  ('prefix, 'inner_prefix) Path.path ->
  ('inner_prefix, 'params, 'input, 'output) service ->
  ('prefix, 'params, 'input, 'output) service

val forge_request:
  (unit, 'params, 'input, 'output) service ->
  'params -> 'input -> string list * Data_encoding.json

val read_answer:
  (unit, 'params, 'input, 'output) service ->
  Data_encoding.json -> ('output, string) result

(** Service directory description *)
module Description : sig

  type service_descr = {
    description: string option ;
    input: Json_schema.schema ;
    output: Json_schema.schema ;
  }

  type directory_descr =
    | Static of static_directory_descr
    | Dynamic of string option

  and static_directory_descr = {
    service: service_descr option ;
    subdirs: static_subdirectories_descr option ;
  }

  and static_subdirectories_descr =
    | Suffixes of directory_descr Map.Make(String).t
    | Arg of Arg.descr * directory_descr

  val service:
    ?description:string ->
    ('prefix, 'params) Path.path ->
    ('prefix, 'params, bool option, directory_descr) service

  val pp_print_directory_descr:
    Format.formatter -> directory_descr -> unit

end

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
  val return_stream: 'a stream -> 'a answer Lwt.t

end

(** Dispatch tree *)
type 'prefix directory

(** Empty tree *)
val empty: 'prefix directory

val map: ('a -> 'b) -> 'b directory -> 'a directory

val prefix: ('pr, 'p) Path.path -> 'p directory -> 'pr directory
val merge: 'a directory -> 'a directory -> 'a directory

(** Possible error while registring services. *)
type step =
  | Static of string
  | Dynamic of Arg.descr
type conflict =
  | CService | CDir | CBuilder | CCustom
  | CTypes of Arg.descr *
              Arg.descr
  | CType of Arg.descr * string list
exception Conflict of step list * conflict

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

(** Registring dynamic subtree. *)
val register_dynamic_directory:
  ?descr:string ->
  'prefix directory ->
  ('prefix, 'a) Path.path -> ('a -> 'a directory Lwt.t) ->
  'prefix directory

(** Registring dynamic subtree. (Curryfied variant) *)
val register_dynamic_directory1:
  ?descr:string ->
  'prefix directory ->
  ('prefix, unit * 'a) Path.path ->
  ('a -> (unit * 'a) directory Lwt.t) ->
  'prefix directory

val register_dynamic_directory2:
  ?descr:string ->
  'prefix directory ->
  ('prefix, (unit * 'a) * 'b) Path.path ->
  ('a -> 'b -> ((unit * 'a) * 'b) directory Lwt.t) ->
  'prefix directory

val register_dynamic_directory3:
  ?descr:string ->
  'prefix directory ->
  ('prefix, ((unit * 'a) * 'b) * 'c) Path.path ->
  ('a -> 'b -> 'c -> (((unit * 'a) * 'b) * 'c) directory Lwt.t) ->
  'prefix directory

(** Registring custom directory lookup. *)
type custom_lookup =
  | CustomService of Description.service_descr *
                     ( Data_encoding.json option ->
                       Data_encoding.json Answer.answer Lwt.t )
  | CustomDirectory of Description.directory_descr

val register_custom_lookup:
  ?descr:string ->
  'prefix directory ->
  ('prefix, 'params) Path.path ->
  ('params -> string list -> custom_lookup Lwt.t) ->
  'prefix directory

val register_custom_lookup1:
  ?descr:string ->
  'prefix directory ->
  ('prefix, unit * 'a) Path.path ->
  ('a -> string list -> custom_lookup Lwt.t) ->
  'prefix directory

val register_custom_lookup2:
  ?descr:string ->
  'prefix directory ->
  ('prefix, (unit * 'a) * 'b) Path.path ->
  ('a -> 'b -> string list -> custom_lookup Lwt.t) ->
  'prefix directory

val register_custom_lookup3:
  ?descr:string ->
  'prefix directory ->
  ('prefix, ((unit * 'a) * 'b) * 'c) Path.path ->
  ('a -> 'b -> 'c -> string list -> custom_lookup Lwt.t) ->
  'prefix directory


(** Registring a description service. *)
val register_describe_directory_service:
  'prefix directory ->
  ('prefix, 'prefix, bool option, Description.directory_descr) service ->
  'prefix directory

(** A handle on the server worker. *)
type server

(** Promise a running RPC serve ; takes the address and port. To call
    an RPX at /p/a/t/h/ in the provided service, one must call the URI
    /call/p/a/t/h/. Calling /list/p/a/t/h/ will list the services
    prefixed by /p/a/t/h/, if any. Calling /schema/p/a/t/h/ will
    describe the input and output of the service, if it is
    callable. Calling /pipe will read a sequence of services to call in
    sequence from the request body, see {!pipe_encoding}. *)
val launch : string -> int -> unit directory -> server Lwt.t

(** Kill an RPC server. *)
val shutdown : server -> unit Lwt.t

(** Retrieve the root service of the server *)
val root_service : server -> unit directory

(** Change the root service of the server *)
val set_root_service : server -> unit directory -> unit

module Error : sig
  val service: (unit, unit, unit, Json_schema.schema) service
  val encoding: error list Data_encoding.t
  val wrap:
    'a Data_encoding.t -> 'a tzresult Data_encoding.encoding

end
