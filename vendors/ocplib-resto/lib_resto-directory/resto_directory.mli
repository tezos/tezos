(**************************************************************************)
(*  ocplib-resto                                                          *)
(*  Copyright (C) 2016, OCamlPro.                                         *)
(*                                                                        *)
(*    All rights reserved.  This file is distributed under the terms      *)
(*    of the GNU Lesser General Public License version 2.1, with the      *)
(*    special exception on linking described in the file LICENSE.         *)
(*                                                                        *)
(**************************************************************************)

open Resto

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

module Make (Encoding : ENCODING) : sig

  module Service : (module type of (struct include Resto.MakeService(Encoding) end))

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

  type ('query, 'input, 'output, 'error) types = {
    query : 'query Resto.Query.t ;
    input : 'input Service.input ;
    output : 'output Encoding.t ;
    error : 'error Encoding.t ;
  }

  type registered_service =
    | Service :
        { types : ('q, 'i, 'o, 'e) types ;
          handler : ('q -> 'i -> ('o, 'e) Answer.t Lwt.t) ;
        } -> registered_service

  (** Dispatch tree *)
  type 'prefix t
  type 'prefix directory = 'prefix t

  type lookup_error =
    [ `Not_found (* 404 *)
    | `Method_not_allowed of meth list (* 405 *)
    | `Cannot_parse_path of string list * Arg.descr * string (* 400 *)
    ]

  (** Resolve a service. *)
  val lookup:
    'prefix directory -> 'prefix ->
    meth -> string list -> (registered_service, [> lookup_error ]) result Lwt.t

  val allowed_methods:
    'prefix directory -> 'prefix -> string list ->
    (meth list, [> lookup_error ]) result Lwt.t

  val transparent_lookup:
    'prefix directory ->
    ('meth, 'prefix, 'params, 'query, 'input, 'output, 'error) Service.t ->
    'params -> 'query -> 'input -> [> ('output, 'error) Answer.t ] Lwt.t

  (** Empty tree *)
  val empty: 'prefix directory

  val map: ('a -> 'b Lwt.t) -> 'b directory -> 'a directory

  val prefix: ('pr, 'p) Path.path -> 'p directory -> 'pr directory
  val merge: 'a directory -> 'a directory -> 'a directory

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
    ('m, 'prefix, ((((unit * 'a) * 'b) * 'c) * 'd) * 'f, 'q , 'i, 'o, 'e) Service.t ->
    ('a -> 'b -> 'c -> 'd -> 'f -> 'q -> 'i -> [< ('o, 'e) Answer.t ] Lwt.t) ->
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

  (** Registring a description service. *)
  val register_describe_directory_service:
    'prefix directory ->
    ('prefix, 'prefix, 'error) Service.description_service ->
    'prefix directory

  val describe_directory:
    recurse:bool ->
    ?arg:'a ->
    'a directory -> Encoding.schema Resto.Description.directory Lwt.t

  (**/**)

  module Curry: sig

    type (_,_,_,_,_,_) conv =
      | Z : (unit, 'g, 'g, unit, 'f, 'f) conv
      | S : ('t, 'g, 'b * 's, 'rt, 'f, 'r) conv ->
        ('t * 'b, 'g, 's, 'a * 'rt, 'a -> 'f, 'r) conv
    val curry : ('a, 'b, unit, 'b, 'c, 'd) conv -> 'c -> 'a -> 'd

  end

  (**/**)

end
