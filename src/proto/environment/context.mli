(** View over the context store, restricted to types, access and
    functional manipulation of an existing context. *)

open Hash

include Persist.STORE

val get_fitness: t -> Fitness.fitness Lwt.t
val set_fitness: t -> Fitness.fitness -> t Lwt.t

val get_timestamp: t -> Time.t Lwt.t
val set_commit_message: t -> string -> t Lwt.t

val register_resolver:
  'a Base58.encoding -> (t -> string -> 'a list Lwt.t) -> unit

val complete: t -> string -> string list Lwt.t
