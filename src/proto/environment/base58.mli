
type 'a encoding

val simple_decode: 'a encoding -> string -> 'a option
val simple_encode: 'a encoding -> 'a -> string

type data = ..

val register_encoding:
  prefix: string ->
  length: int ->
  to_raw: ('a -> string) ->
  of_raw: (string -> 'a option) ->
  wrap: ('a -> data) ->
  'a encoding

val check_encoded_prefix: 'a encoding -> string -> int -> unit

val decode: string -> data option
