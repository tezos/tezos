(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

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
val find_opt : 'a def -> set -> 'a option
val find : 'a def -> set -> 'a option
val get : 'a def -> set -> 'a
val find_first : (Key.t -> bool) -> set -> t
val find_first_opt : (Key.t -> bool) -> set -> t option
val find_last : (Key.t -> bool) -> set -> t
val find_last_opt : (Key.t -> bool) -> set -> t option
val map : (t -> t) -> set -> set
val mapi : (t -> t) -> set -> set
val pp_set : Format.formatter -> set -> unit

(** DSL for logging messages.  Opening this locally makes it easy to supply a number
    of semantic tags for a log event while using their values in the human-readable
    text.  For example:

    {[
      lwt_log_info Tag.DSL.(fun f ->
          f "request for operations %a:%d from peer %a timed out."
          -% t event "request_operations_timeout"
          -% a Block_hash.Logging.tag bh
          -% s operations_index_tag n
          -% a P2p_peer.Id.Logging.tag pipeline.peer_id)
    ]} *)
module DSL : sig
  type (_,_,_,_) arg

  (** Use a semantic tag with a `%a` format, supplying the pretty printer from the tag. *)
  val a : 'v def -> 'v -> (('b -> 'v -> 'c) -> 'v -> 'd, 'b, 'c, 'd) arg

  (** Use a semantic tag with ordinary formats such as `%s`, `%d`, and `%f`. *)
  val s : 'v def -> 'v -> ('v -> 'd, 'b, 'c, 'd) arg

  (** Supply a semantic tag without formatting it. *)
  val t : 'v def -> 'v -> ('d, 'b, 'c, 'd) arg

  (** Perform the actual application of a tag to a format. *)
  val (-%) : (?tags:set -> 'a) -> ('a,Format.formatter,unit,'d) arg -> (?tags:set -> 'd)
end
