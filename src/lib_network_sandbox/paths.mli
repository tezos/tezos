(** Configure and manipulate a “root-path.”   *)

type t

val make : string -> t
val pp : Format.formatter -> t -> unit

val root : < paths: t ; .. > -> string
(** Query the configured root-path. *)

val cli_term :
  ?option_name:string -> default_root:string -> unit -> t Cmdliner.Term.t
