(** This module wraps the type ['kind t] around the notion of
    tezos-excutable: a path to a binary with tezos-specific properties. *)

(** Helper functions to construct exec-style command lines for
    ["tezos-*"] applications. *)
module Make_cli : sig
  val flag : string -> string list
  val opt : string -> string -> string list
  val optf : string -> ('a, unit, string, string list) format4 -> 'a
end

(** Manipulate the ["TEZOS_EVENTS_CONFIG"] environment variable. *)
module Unix_files_sink : sig
  type t = private {matches: string list option; level_at_least: string}

  val all_notices : t
  val all_info : t
end

(** The type [kind] is used to distinguish ['a t] executables. *)
type kind = [`Node | `Baker | `Endorser | `Accuser | `Client | `Admin]

(** The wrapper of the tezos-executable. *)
type 'kind t = private
  { kind: 'kind
  ; binary: string option
  ; unix_files_sink: Unix_files_sink.t option
  ; environment: (string * string) list }

val node :
     ?binary:string
  -> ?unix_files_sink:Unix_files_sink.t
  -> ?environment:(string * string) list
  -> unit
  -> [> `Node] t
(** Create a ["tezos-node"] executable. *)

val kind_string : [< kind] -> string
(** Convert a [kind] to a [string]. *)

val default_binary : [< kind] t -> string
(** Get the path/name of the default binary for a given kind, e.g.,
    ["tezos-admin-client"]. *)

val call : [< kind] t -> path:string -> string list -> unit Genspio.EDSL.t
(** Build a [Genspio.EDSL.t] script to run a tezos command, the
    [~path] argument is used as a toplevel path for the unix-files
    event-sink (event-logging-framework) and for other local logging
    files. *)

val cli_term : ([< kind] as 'a) -> string -> 'a t Cmdliner.Term.t
(** Build a [Cmdliner] term which creates tezos-executables, the
    second argument is a prefix of option names (e.g. ["tezos"] for the
    option ["--tezos-accuser-alpha-binary"]). *)
