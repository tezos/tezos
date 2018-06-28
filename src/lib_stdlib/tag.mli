(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(** Tags and tag sets.  Tags are basically similar to a plain extensible
    variant type, but wrapped with metadata that enables them to be printed
    generically and combined into tag sets where each tag is either not
    present or associated with a specific value.

    They are primarily intended for use with the `Logging` module but it
    would probably be reasonable to use them for other purposes. *)

(** Type of tag definitions.  Analogous to a constructor of an extensible
    variant type, but first-class. *)
type _ def

(** Define a new tag with a name, printer, and optional documentation string.
    This is generative, not applicative, so tag definitions created with
    identical names and printers at different times or places will be
    different tags!  You probably do not want to define a tag in a local
    scope unless you have something really tricky in mind.  Basically all
    the caveats you would have if you wrote [type t +=] apply. *)
val def : ?doc:string -> string -> (Format.formatter -> 'a -> unit) -> 'a def

val name : 'a def -> string
val doc : 'a def -> string
val printer : 'a def -> (Format.formatter -> 'a -> unit)

(** Print the name of a tag definition. *)
val pp_def : Format.formatter -> 'a def -> unit

(** A binding consisting of a tag and value.  If a `def` is a constructor
    of an extensible variant type, a `t` is a value of that type. *)
type t = V : 'a def * 'a -> t
val pp : Format.formatter -> t -> unit

module Key : sig
  type t = V : 'a def -> t
end

(** Tag sets.  If `t` is an extensible variant type, `set` is a set of `t`s
    no two of which have the same constructor.  Most ordinary set and map
    operations familiar from the Ocaml standard library are provided.
    `equal` and `compare` are purposely not provided as there is no
    meaningful ordering on tags and their arguments may not even have a
    meaningful notion of equality. *)
type set

val empty : set
val is_empty : set -> bool
val mem : 'a def -> set -> bool
val add : 'a def -> 'a -> set -> set
val update : 'a def -> ('a option -> 'a option) -> (set -> set)
val singleton : 'a def -> 'a -> set
val remove : 'a def -> set -> set
val rem : 'a def -> set -> set
type merger = { merger : 'a. 'a def -> 'a option -> 'a option -> 'a option }
val merge : merger -> set -> set -> set
type unioner = { unioner : 'a. 'a def -> 'a -> 'a -> 'a }
val union : unioner -> set -> set -> set
val iter : (t -> unit) -> set -> unit
val fold : (t -> 'b -> 'b) -> (set -> 'b -> 'b)
val for_all : (t -> bool) -> (set -> bool)
val exists : (t -> bool) -> (set -> bool)
val filter : (t -> bool) -> set -> set
val partition : (t -> bool) -> set -> (set * set)
val cardinal : set -> int
val min_binding : set -> t
val min_binding_opt : set -> t option
val max_binding : set -> t
val max_binding_opt : set -> t option
val choose : set -> t
val choose_opt : set -> t option
val split : 'a def -> set -> set * 'a option * set
val find : 'a def -> set -> 'a option
val get : 'a def -> set -> 'a
val find_first : (Key.t -> bool) -> set -> t
val find_first_opt : (Key.t -> bool) -> set -> t option
val find_last : (Key.t -> bool) -> set -> t
val find_last_opt : (Key.t -> bool) -> set -> t option
val map : (t -> t) -> set -> set
val mapi : (t -> t) -> set -> set
val pp_set : Format.formatter -> set -> unit

module DSL : sig
  type (_,_,_,_) arg
  val a : 'v def -> 'v -> (('b -> 'v -> 'c) -> 'v -> 'd, 'b, 'c, 'd) arg
  val s : 'v def -> 'v -> ('v -> 'd, 'b, 'c, 'd) arg
  val t : 'v def -> 'v -> ('d, 'b, 'c, 'd) arg
  val (-%) : (?tags:set -> 'a) -> ('a,Format.formatter,unit,'d) arg -> (?tags:set -> 'd)
end
