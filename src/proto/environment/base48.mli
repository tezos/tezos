
type data = ..

val decode: ?alphabet:string -> string -> data
val encode: ?alphabet:string -> data -> string

val register:
  prefix:string ->
  read:(data -> string option) ->
  build:(string -> data) ->
  unit

module Prefix : sig
  val protocol_prefix: string
end
