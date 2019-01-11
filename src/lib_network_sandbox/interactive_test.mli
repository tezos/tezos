(** Tools to manage interactivity in test scenarios. *)

open Internal_pervasives

(** Implementations of common {!Console.Prompt.item}. *)
module Commands : sig
  val cmdline_fail :
       ( 'a
       , Format.formatter
       , unit
       , ('b, [> `Command_line of string]) Asynchronous_result.t )
       format4
    -> 'a

  val no_args :
    'a list -> (unit, [> `Command_line of string]) Asynchronous_result.t

  val flag : string -> Sexplib0.Sexp.t list -> bool

  val unit_loop_no_args :
       Easy_format.t
    -> string list
    -> (   unit
        -> ( unit
           , [`Command_line of string | `Lwt_exn of exn] )
           Asynchronous_result.t)
    -> Console.Prompt.item

  val du_sh_root :
       < application_name: string
       ; console: Console.t
       ; paths: Paths.t
       ; runner: Running_processes.State.t
       ; .. >
    -> Console.Prompt.item

  val processes :
       < application_name: string
       ; console: Console.t
       ; runner: Running_processes.State.t
       ; .. >
    -> Console.Prompt.item

  val curl :
       ?jq:string
    -> < application_name: string
       ; console: Console.t
       ; paths: Paths.t
       ; runner: Running_processes.State.t
       ; .. >
    -> port:int
    -> path:string
    -> ( [> `Error | `Success of string list]
       , [> `Lwt_exn of exn] )
       Asynchronous_result.t

  val curl_unit_display :
       ?jq:string
    -> < application_name: string
       ; console: Console.t
       ; paths: Paths.t
       ; runner: Running_processes.State.t
       ; .. >
    -> string list
    -> default_port:int
    -> path:string
    -> doc:string
    -> Console.Prompt.item

  val curl_metadata :
       < application_name: string
       ; console: Console.t
       ; paths: Paths.t
       ; runner: Running_processes.State.t
       ; .. >
    -> default_port:int
    -> Console.Prompt.item

  val curl_level :
       < application_name: string
       ; console: Console.t
       ; paths: Paths.t
       ; runner: Running_processes.State.t
       ; .. >
    -> default_port:int
    -> Console.Prompt.item

  val curl_baking_rights :
       < application_name: string
       ; console: Console.t
       ; paths: Paths.t
       ; runner: Running_processes.State.t
       ; .. >
    -> default_port:int
    -> Console.Prompt.item

  val all_levels :
       < application_name: string
       ; console: Console.t
       ; paths: Paths.t
       ; runner: Running_processes.State.t
       ; .. >
    -> nodes:Tezos_node.t list
    -> Console.Prompt.item

  val show_process :
       < application_name: string
       ; console: Console.t
       ; paths: Paths.t
       ; runner: Running_processes.State.t
       ; .. >
    -> Console.Prompt.item

  val kill_all :
    < runner: Running_processes.State.t ; .. > -> Console.Prompt.item

  val secret_keys :
       < application_name: string ; console: Console.t ; .. >
    -> protocol:Tezos_protocol.t
    -> Console.Prompt.item

  val arbitrary_command_on_clients :
       ?make_admin:(Tezos_client.t -> Tezos_admin_client.t)
    -> ?command_names:string list
    -> < application_name: string
       ; console: Console.t
       ; paths: Paths.t
       ; runner: Running_processes.State.t
       ; .. >
    -> clients:Tezos_client.t list
    -> Console.Prompt.item

  val all_defaults :
       < application_name: string
       ; console: Console.t
       ; paths: Paths.t
       ; runner: Running_processes.State.t
       ; .. >
    -> nodes:Tezos_node.t list
    -> Console.Prompt.item list
end

(** Configurable (through {!Cmdliner.Term.t}) interactivity of
    test-scenarios. *)
module Interactivity : sig
  type t = [`Full | `None | `On_error | `At_end]

  val pause_on_error : < test_interactivity: t ; .. > -> bool
  val pause_on_success : < test_interactivity: t ; .. > -> bool
  val is_interactive : < test_interactivity: t ; .. > -> bool
  val cli_term : ?default:t -> unit -> t Cmdliner.Term.t
end

(** A {!Pauser.t} is tool to include optional prompting pauses in
    test-scenarios. *)
module Pauser : sig
  type t = private
    { mutable extra_commands: Console.Prompt.item list
    ; default_end: [`Sleep of float] }

  val make : ?default_end:[`Sleep of float] -> Console.Prompt.item list -> t

  val add_commands : < pauser: t ; .. > -> Console.Prompt.item list -> unit
  (** Add commands to the current pauser. *)

  val generic :
       < application_name: string
       ; console: Console.t
       ; pauser: t
       ; test_interactivity: Interactivity.t
       ; .. >
    -> ?force:bool
    -> Easy_format.t list
    -> (unit, [> `Lwt_exn of exn]) Asynchronous_result.t
  (** Pause the test according to [state#interactivity] (overridden
      with [~force:true]), the pause displays the list of
      {!Easy_format.t}s and prompts the user for commands (see
      {!add_commands}). *)

  val run_test :
       < application_name: string
       ; console: Console.t
       ; paths: Paths.t
       ; pauser: t
       ; runner: Running_processes.State.t
       ; test_interactivity: Interactivity.t
       ; .. >
    -> (unit -> (unit, ([> `Lwt_exn of exn] as 'errors)) Asynchronous_result.t)
    -> pp_error:(Format.formatter -> 'errors -> unit)
    -> unit
    -> (unit, 'errors) Asynchronous_result.t
  (** Run a test-scenario and deal with potential errors according
      to [state#test_interactivity]. *)
end
