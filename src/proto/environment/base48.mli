
type data = ..

val decode: ?alphabet:string -> string -> data
val encode: ?alphabet:string -> data -> string

type kind

val register:
  prefix:string ->
  read:(data -> string option) ->
  build:(string -> data) ->
  kind

val register_resolver:
  kind -> (string -> string list Lwt.t) -> unit

module Prefix : sig
  val protocol_prefix: string
end
