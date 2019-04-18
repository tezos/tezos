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

open Error_monad

(** Command Line Interpretation Combinators.

    Supports command lines of the following form:

    [executable [global options] command [command options]]

    Global options must be passed before the command, and may define
    the set of supported commands.

    Commands are series of fixed keywords and positional arguments, in
    order to support command lines close to a natural language. *)

(** {2 Argument parsers.} *)

(** The type for argument parsers, used for both positional and
    optional arguments.

    The first type parameter is the OCaml type of the argument once
    parsed from its string notation. The second parameter is a context
    that is passed througout the parsing of the command line. Some
    parameters (for instance a simple [int]) can remain polymorphic,
    while others need a context to be parsed. Of course, a command line
    can only contain parameters that bear the same context type. *)
type ('a, 'ctx) parameter

(** Build an argument parser, combining a parsing function and an
    autocompletion function. The autocompletion must simply return the
    list of all valid values for the parameter. *)
val parameter :
  ?autocomplete:('ctx -> string list tzresult Lwt.t) ->
  ('ctx -> string -> 'a tzresult Lwt.t) ->
  ('a, 'ctx) parameter

(** Build an argument parser by composing two other parsers. The
    resulting parser will try the first parser and if it fails will
    try the second. The auto-complete contents of the two will be
    concatenated. *)
val compose_parameters : ('a, 'ctx) parameter -> ('a, 'ctx) parameter -> ('a, 'ctx) parameter

(** Map a pure function over the result of a parameter parser. *)
val map_parameter : f:('a -> 'b) -> ('a, 'ctx) parameter -> ('b, 'ctx) parameter

(** {2 Flags and Options } *)

(** The type for optional arguments (and switches).

    Extends a parser with a parameter name and a placeholder to
    display in help screens.

    Also adds a documentation for the switch, that must be of the form
    ["lowercase short description\nOptional longer description."]. *)
type ('a, 'ctx) arg

val constant: 'a -> ('a, 'ctx) arg

(** [arg ~doc ~long ?short converter] creates an argument to a command.
    The [~long] argument is the long format, without the double dashes.
    The [?short] argument is the optional one letter shortcut.
    If the argument is not provided, [None] is returned. *)
val arg :
  doc:string ->
  ?short:char ->
  long:string ->
  placeholder:string ->
  ('a, 'ctx) parameter ->
  ('a option, 'ctx) arg

(** Create an argument that will contain the [~default] value if it is not provided. *)
val default_arg :
  doc:string ->
  ?short:char ->
  long:string ->
  placeholder:string ->
  default:string ->
  ('a, 'ctx) parameter ->
  ('a, 'ctx) arg

(** Create a boolean switch.
    The value will be set to [true] if the switch is provided and [false] if it is not. *)
val switch :
  doc:string ->
  ?short:char ->
  long:string ->
  unit ->
  (bool, 'ctx) arg

(** {2 Groups of Optional Arguments} *)

(** Defines a group of options, either the global options or the
    command options. *)

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

(** Include 11 optional parameters *)
val args11 : ('a, 'ctx) arg -> ('b, 'ctx) arg -> ('c, 'ctx) arg -> ('d, 'ctx) arg ->
  ('e, 'ctx) arg -> ('f, 'ctx) arg -> ('g, 'ctx) arg -> ('h, 'ctx) arg ->
  ('i, 'ctx) arg -> ('j, 'ctx) arg -> ('k, 'ctx) arg ->
  ('a * 'b * 'c * 'd * 'e * 'f * 'g * 'h * 'i * 'j * 'k, 'ctx) options

(** Include 12 optional parameters *)
val args12 : ('a, 'ctx) arg -> ('b, 'ctx) arg -> ('c, 'ctx) arg -> ('d, 'ctx) arg ->
  ('e, 'ctx) arg -> ('f, 'ctx) arg -> ('g, 'ctx) arg -> ('h, 'ctx) arg ->
  ('i, 'ctx) arg -> ('j, 'ctx) arg -> ('k, 'ctx) arg -> ('l, 'ctx) arg ->
  ('a * 'b * 'c * 'd * 'e * 'f * 'g * 'h * 'i * 'j * 'k * 'l, 'ctx) options

(** Include 13 optional parameters *)
val args13 : ('a, 'ctx) arg -> ('b, 'ctx) arg -> ('c, 'ctx) arg -> ('d, 'ctx) arg ->
  ('e, 'ctx) arg -> ('f, 'ctx) arg -> ('g, 'ctx) arg -> ('h, 'ctx) arg ->
  ('i, 'ctx) arg -> ('j, 'ctx) arg -> ('k, 'ctx) arg -> ('l, 'ctx) arg -> ('m, 'ctx) arg ->
  ('a * 'b * 'c * 'd * 'e * 'f * 'g * 'h * 'i * 'j * 'k * 'l * 'm, 'ctx) options

(** Include 14 optional parameters *)
val args14 : ('a, 'ctx) arg -> ('b, 'ctx) arg -> ('c, 'ctx) arg -> ('d, 'ctx) arg ->
  ('e, 'ctx) arg -> ('f, 'ctx) arg -> ('g, 'ctx) arg -> ('h, 'ctx) arg ->
  ('i, 'ctx) arg -> ('j, 'ctx) arg -> ('k, 'ctx) arg -> ('l, 'ctx) arg ->
  ('m, 'ctx) arg -> ('n, 'ctx) arg ->
  ('a * 'b * 'c * 'd * 'e * 'f * 'g * 'h * 'i * 'j * 'k * 'l * 'm * 'n, 'ctx) options

(** Include 15 optional parameters *)
val args15 : ('a, 'ctx) arg -> ('b, 'ctx) arg -> ('c, 'ctx) arg -> ('d, 'ctx) arg ->
  ('e, 'ctx) arg -> ('f, 'ctx) arg -> ('g, 'ctx) arg -> ('h, 'ctx) arg ->
  ('i, 'ctx) arg -> ('j, 'ctx) arg -> ('k, 'ctx) arg -> ('l, 'ctx) arg ->
  ('m, 'ctx) arg -> ('n, 'ctx) arg -> ('o, 'ctx) arg ->
  ('a * 'b * 'c * 'd * 'e * 'f * 'g * 'h * 'i * 'j * 'k * 'l * 'm * 'n * 'o, 'ctx) options

(** Include 16 optional parameters *)
val args16 : ('a, 'ctx) arg -> ('b, 'ctx) arg -> ('c, 'ctx) arg -> ('d, 'ctx) arg ->
  ('e, 'ctx) arg -> ('f, 'ctx) arg -> ('g, 'ctx) arg -> ('h, 'ctx) arg ->
  ('i, 'ctx) arg -> ('j, 'ctx) arg -> ('k, 'ctx) arg -> ('l, 'ctx) arg ->
  ('m, 'ctx) arg -> ('n, 'ctx) arg -> ('o, 'ctx) arg -> ('p, 'ctx) arg ->
  ('a * 'b * 'c * 'd * 'e * 'f * 'g * 'h * 'i * 'j * 'k * 'l * 'm * 'n * 'o * 'p, 'ctx) options

(** Include 17 optional parameters *)
val args17 : ('a, 'ctx) arg -> ('b, 'ctx) arg -> ('c, 'ctx) arg -> ('d, 'ctx) arg ->
  ('e, 'ctx) arg -> ('f, 'ctx) arg -> ('g, 'ctx) arg -> ('h, 'ctx) arg ->
  ('i, 'ctx) arg -> ('j, 'ctx) arg -> ('k, 'ctx) arg -> ('l, 'ctx) arg ->
  ('m, 'ctx) arg -> ('n, 'ctx) arg -> ('o, 'ctx) arg -> ('p, 'ctx) arg ->
  ('q, 'ctx) arg ->
  ('a * 'b * 'c * 'd * 'e * 'f * 'g * 'h * 'i * 'j * 'k * 'l * 'm * 'n * 'o * 'p * 'q, 'ctx) options

(** {2 Parameter based command lines} *)

(** Type of parameters for a command *)
type ('a, 'ctx) params

(** A piece of data inside a command line *)
val param:
  name: string ->
  desc: string ->
  ('a, 'ctx) parameter ->
  ('b, 'ctx) params ->
  ('a -> 'b, 'ctx) params

(** A word in a command line.
    Should be descriptive. *)
val prefix:
  string ->
  ('a, 'ctx) params ->
  ('a, 'ctx) params

(** Multiple words given in sequence for a command line *)
val prefixes:
  string list ->
  ('a, 'ctx) params ->
  ('a, 'ctx) params

(** A fixed series of words that trigger a command. *)
val fixed:
  string list ->
  ('ctx -> unit tzresult Lwt.t, 'ctx) params

(** End the description of the command line *)
val stop:
  ('ctx -> unit tzresult Lwt.t, 'ctx) params

(** Take a sequence of parameters instead of only a single one.
    Must be the last thing in the command line. *)
val seq_of_param:
  (('ctx -> unit tzresult Lwt.t, 'ctx) params ->
   ('a -> 'ctx -> unit tzresult Lwt.t, 'ctx) params) ->
  ('a list -> 'ctx -> unit tzresult Lwt.t, 'ctx) params

(** Parameter that expects a string *)
val string:
  name: string ->
  desc: string ->
  ('a, 'ctx) params ->
  (string -> 'a, 'ctx) params

(** {2 Commands }  *)

(** Command, including a parameter specification, optional arguments, and handlers  *)
type 'ctx command

(** Type of a group of commands.
    Groups have their documentation printed together
    and should include a descriptive title. *)
type group =
  { name : string ;
    title : string }

(** A complete command, with documentation, a specification of its
    options, parameters, and handler function. *)
val command:
  ?group: group ->
  desc: string ->
  ('b, 'ctx) options ->
  ('a, 'ctx) params ->
  ('b -> 'a) ->
  'ctx command

(** Combinator to use a command in an adaptated context. *)
val map_command: ('a -> 'b) -> 'b command -> 'a command

(** {2 Output formatting} *)

(** Used to restore the formatter state after [setup_formatter]. *)
type formatter_state

(** Supported output formats.
    Currently: black and white, colors using ANSI escapes, and HTML.*)
type format = Plain | Ansi | Html

(** Verbosity level, from terse to verbose. *)
type verbosity = Terse | Short | Details | Full

(** Updates the formatter's functions to interprete some semantic tags
    used in manual production. Returns the previous state of the
    formatter to restore it afterwards if needed.

    Toplevel structure tags:

      * [<document>]: a toplevel group
      * [<title>]: a section title (just below a [<document])
      * [<list>]: a list section (just below a [<document])

    Structure tags used internally for generating the manual:

    * [<command>]: wraps the full documentation bloc for a command
    * [<commandline>]: wraps the command line in a [<command>]
    * [<commanddoc>]: wraps everything but the command line in a [<command>]

    Cosmetic tags for hilighting text:

    * [<opt>]: optional arguments * [<arg>]: positional arguments
    * [<kwd>]: positional keywords * [<hilight>]: search results

    Verbosity levels, in order, and how they are used in the manual:

    * [<terse>]: titles, commands lines
    * [<short>]: lists of arguments
    * [<details>]: single line descriptions
    * [<full>]: with long descriptions

    Wrapping a piece of text with a debug level means that the
    contents are only printed if the verbosity is equal to or
    above that level. Use prefix [=] for an exact match, or [-]
    for the inverse interpretation. *)
val setup_formatter :
  Format.formatter ->
  format ->
  verbosity ->
  formatter_state

(** Restore the formatter state after [setup_formatter]. *)
val restore_formatter : Format.formatter -> formatter_state -> unit

(** {2 Parsing and error reporting} *)

(** Help error (not really an error), thrown by {!dispatch} and {!parse_initial_options}. *)
type error += Help : _ command option -> error

(** Find and call the applicable command on the series of arguments.
    @raise [Failure] if the command list would be ambiguous. *)
val dispatch: 'ctx command list -> 'ctx -> string list -> unit tzresult Lwt.t

(** Parse the global options, and return their value, with the rest of
    the command to be parsed. *)
val parse_global_options : ('a, 'ctx) options -> 'ctx -> string list -> ('a * string list) tzresult Lwt.t

(** Pretty printfs the error messages to the given formatter.
    [executable_name] and [global_options] are for help screens.
    [default] is used to print non-cli errors. *)
val pp_cli_errors :
  Format.formatter ->
  executable_name: string ->
  global_options: (_, _) options ->
  default: (Format.formatter -> error -> unit) ->
  error list ->
  unit

(** Acts as {!dispatch}, but stops if the given command up to
    [prev_arg] is a valid prefix command, returning the list of valid
    next words, filtered with [cur_arg]. *)
val autocompletion :
  script:string -> cur_arg:string -> prev_arg:string -> args:string list ->
  global_options:('a, 'ctx) options -> 'ctx command list -> 'ctx ->
  string list Error_monad.tzresult Lwt.t

(** Displays a help page for the given commands. *)
val usage :
  Format.formatter ->
  executable_name:string ->
  global_options:(_, _) options ->
  _ command list ->
  unit

(** {2 Manual} *)

(** Add manual commands to a list of commands.
    For this to work, the command list must be complete.
    Commands added later will not appear in the manual. *)
val add_manual :
  executable_name: string ->
  global_options: ('a, 'ctx) options ->
  format ->
  Format.formatter ->
  'ctx command list ->
  'ctx command list
