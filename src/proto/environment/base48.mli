
module Prefix : sig
  val protocol_prefix: string
end

type 'a encoding

val simple_decode: ?alphabet:string -> 'a encoding -> string -> 'a option
val simple_encode: ?alphabet:string -> 'a encoding -> 'a -> string

type data = ..

val register_encoding:
  prefix: string ->
  to_raw: ('a -> string) ->
  of_raw: (string -> 'a option) ->
  wrap: ('a -> data) ->
  'a encoding

val decode: ?alphabet:string -> string -> data option
