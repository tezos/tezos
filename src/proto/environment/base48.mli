
module Prefix : sig
  val protocol_prefix: string
end

type 'a encoding = 'a Base48.encoding

val simple_decode: ?alphabet:string -> 'a encoding -> string -> 'a option
val simple_encode: ?alphabet:string -> 'a encoding -> 'a -> string

type data = Base48.data = ..

val register_encoding:
  prefix: string ->
  to_raw: ('a -> string) ->
  of_raw: (string -> 'a option) ->
  wrap: ('a -> data) ->
  'a encoding

val decode: ?alphabet:string -> string -> data option

val register_resolver:
  'a encoding -> (Context.t -> string -> 'a list Lwt.t) -> unit

val complete:
  ?alphabet:string -> Context.t -> string -> string list Lwt.t
