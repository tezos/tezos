(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Error_monad

(* Tezos: a small Command Line Parsing library *)
(* Only used in the client. *)

(** The type for positional parameters and flags *)
type ('p, 'ctx) parameter
val parameter : ?autocomplete:('ctx -> string list tzresult Lwt.t) ->
  ('ctx -> string -> 'p tzresult Lwt.t) ->
  ('p, 'ctx) parameter

(** {2 Flags and Options } *)

(** {3 Options and Switches } *)

(** Type for option or switch *)
type ('a, 'ctx) arg

(** [arg ~doc ~parameter converter] creates an argument to a command.
    The [~parameter] argument should begin with a [-].
    If the argument is not provided, [None] is returned *)
val arg :
  doc:string ->
  parameter:string ->
  placeholder:string ->
  ('p, 'ctx) parameter ->
  ('p option, 'ctx) arg

(** Create an argument that will contain the [~default] value if it is not provided.
    see arg *)
val default_arg :
  doc:string ->
  parameter:string ->
  placeholder:string ->
  default:string ->
  ('p, 'ctx) parameter ->
  ('p, 'ctx) arg

(** Create a boolean switch.
    The value will be set to [true] if the switch is provided and [false] if it is not. *)
val switch : doc:string -> parameter:string ->
  (bool, 'ctx) arg

(** {3 Optional Argument Combinators} *)
(** To specify default arguments ([options]) for a command,
    You need to use the following functions,
    which allow you to specify how many arguments you have.
    If you are not including any arguments, use [no_args]. *)

(** The type of a series of labeled arguments to a command *)
type ('a, 'ctx) options

(** Include no optional parameters *)
val no_options : (unit, 'ctx) options

(** Include 1 optional parameter *)
val args1 :
  ('a, 'ctx) arg ->
  ('a, 'ctx) options

(** Include 2 optional parameters *)
val args2 :
  ('a, 'ctx) arg ->
  ('b, 'ctx) arg ->
  ('a * 'b, 'ctx) options

(** Include 3 optional parameters *)
val args3 :
  ('a, 'ctx) arg ->
  ('b, 'ctx) arg ->
  ('c, 'ctx) arg ->
  ('a * 'b * 'c, 'ctx) options

(** Include 4 optional parameters *)
val args4 :
  ('a, 'ctx) arg ->
  ('b, 'ctx) arg ->
  ('c, 'ctx) arg ->
  ('d, 'ctx) arg ->
  ('a * 'b * 'c * 'd, 'ctx) options

(** Include 5 optional parameters *)
val args5 :
  ('a, 'ctx) arg ->
  ('b, 'ctx) arg ->
  ('c, 'ctx) arg ->
  ('d, 'ctx) arg ->
  ('e, 'ctx) arg ->
  ('a * 'b * 'c * 'd * 'e, 'ctx) options

(** Include 6 optional parameters *)
val args6 :
  ('a, 'ctx) arg ->
  ('b, 'ctx) arg ->
  ('c, 'ctx) arg ->
  ('d, 'ctx) arg ->
  ('e, 'ctx) arg ->
  ('f, 'ctx) arg ->
  ('a * 'b * 'c * 'd * 'e * 'f, 'ctx) options

(** Include 7 optional parameters *)
val args7 :
  ('a, 'ctx) arg ->
  ('b, 'ctx) arg ->
  ('c, 'ctx) arg ->
  ('d, 'ctx) arg ->
  ('e, 'ctx) arg -> ('f, 'ctx) arg -> ('g, 'ctx) arg ->
  ('a * 'b * 'c * 'd * 'e * 'f * 'g, 'ctx) options

(** Include 8 optional parameters *)
val args8 : ('a, 'ctx) arg -> ('b, 'ctx) arg -> ('c, 'ctx) arg -> ('d, 'ctx) arg ->
  ('e, 'ctx) arg -> ('f, 'ctx) arg -> ('g, 'ctx) arg -> ('h, 'ctx) arg ->
  ('a * 'b * 'c * 'd * 'e * 'f * 'g * 'h, 'ctx) options

(** Include 9 optional parameters *)
val args9 : ('a, 'ctx) arg -> ('b, 'ctx) arg -> ('c, 'ctx) arg -> ('d, 'ctx) arg ->
  ('e, 'ctx) arg -> ('f, 'ctx) arg -> ('g, 'ctx) arg -> ('h, 'ctx) arg ->
  ('i, 'ctx) arg ->
  ('a * 'b * 'c * 'd * 'e * 'f * 'g * 'h * 'i, 'ctx) options

(** Include 10 optional parameters *)
val args10 : ('a, 'ctx) arg -> ('b, 'ctx) arg -> ('c, 'ctx) arg -> ('d, 'ctx) arg ->
  ('e, 'ctx) arg -> ('f, 'ctx) arg -> ('g, 'ctx) arg -> ('h, 'ctx) arg ->
  ('i, 'ctx) arg -> ('j, 'ctx) arg ->
  ('a * 'b * 'c * 'd * 'e * 'f * 'g * 'h * 'i * 'j, 'ctx) options

(** {2 Parameter based command lines } *)

(** Type of parameters for a command *)
type ('a, 'ctx, 'ret) params

(** A piece of data inside a command line *)
val param:
  name: string ->
  desc: string ->
  ('a, 'ctx) parameter ->
  ('b, 'ctx, 'ret) params ->
  ('a -> 'b, 'ctx, 'ret) params

(** A word in a command line.
    Should be descriptive. *)
val prefix:
  string ->
  ('a, 'ctx, 'ret) params ->
  ('a, 'ctx, 'ret) params

(** Multiple words given in sequence for a command line *)
val prefixes:
  string list ->
  ('a, 'ctx, 'ret) params ->
  ('a, 'ctx, 'ret) params

(** A fixed series of words that trigger a command. *)
val fixed:
  string list ->
  ('ctx -> 'ret tzresult Lwt.t, 'ctx, 'ret) params

(** End the description of the command line *)
val stop:
  ('ctx -> 'ret tzresult Lwt.t, 'ctx, 'ret) params

(** Take a sequence of parameters instead of only a single one.
    Must be the last thing in the command line. *)
val seq_of_param:
  (('ctx -> 'ret tzresult Lwt.t, 'ctx, 'ret) params ->
   ('a -> 'ctx -> 'ret tzresult Lwt.t, 'ctx, 'ret) params) ->
  ('a list -> 'ctx -> 'ret tzresult Lwt.t, 'ctx, 'ret) params

(** Parameter that expects a string *)
val string:
  name: string ->
  desc: string ->
  ('a, 'ctx, 'ret) params ->
  (string -> 'a, 'ctx, 'ret) params

(** {2 Commands }  *)

(** Command, including a parameter specification, optional arguments, and handlers  *)
type ('ctx, 'ret) command

(** Type of a group of commands.
    Groups have their documentation printed together
    and should include a descriptive title. *)
type group =
  { name : string ;
    title : string }

(** A complete command, with documentation, a specification of its options, parameters, and handler function *)
val command:
  ?group: group ->
  desc: string ->
  ('b, 'ctx) options ->
  ('a, 'ctx, 'ret) params ->
  ('b -> 'a) ->
  ('ctx, 'ret) command

(** {2 Parsing and error reporting} *)

(** Print readable descriptions for CLI parsing errors.
    This function must be used for help printing to work. *)
val handle_cli_errors:
  stdout: Format.formatter ->
  stderr: Format.formatter ->
  global_options:(_, _) options ->
  'a tzresult -> int tzresult Lwt.t

(** Find and call the applicable command on the series of arguments.
    @raises [Failure] if the command list would be ambiguous. *)
val dispatch:
  ?global_options:('a, 'ctx) options ->
  ('ctx, 'ret) command list ->
  'ctx ->
  string list ->
  'ret tzresult Lwt.t

(** Parse the sequence of optional arguments that proceed a command *)
val parse_initial_options :
  ('a, 'ctx) options ->
  'ctx ->
  string list ->
  ('a * string list) tzresult Lwt.t
