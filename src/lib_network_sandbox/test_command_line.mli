(** Functions for building test-scenarios commands. *)

open Internal_pervasives

(** Make {!Cmdliner} commands from {!Asynchronous_result} functions. *)
module Run_command : sig
  val make :
       pp_error:(Format.formatter -> ([> ] as 'a) -> unit)
    -> ( < application_name: string ; console: Console.t ; .. >
       * (unit -> (unit, 'a) Asynchronous_result.t) )
       Cmdliner.Term.t
    -> 'b
    -> unit Cmdliner.Term.t * 'b
end

val cli_state :
     ?default_interactivity:Interactive_test.Interactivity.t
  -> ?disable_interactivity:bool
  -> name:string
  -> unit
  -> < application_name: string
     ; console: Console.t
     ; operations_log: Log_recorder.Operations.t
     ; paths: Paths.t
     ; pauser: Interactive_test.Pauser.t
     ; runner: Running_processes.State.t
     ; test_interactivity: Interactive_test.Interactivity.t >
     Cmdliner.Term.t
(** Create a full [state] value for test-scenarios. *)
