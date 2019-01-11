(** Manage a Kiln process next to a network-sandbox. *)

open Internal_pervasives

type t

val make : run:[`Docker of string] -> port:int -> postgres_port:int -> t
(** Configure a Kiln process-to-be, running on port [~port] and
    managing a PostgreSQL database on port [~postgres_port]. *)

val default_docker_image : string
val default : t

val start :
     ?network_id:string
  -> < application_name: string
     ; console: Console.t
     ; paths: Paths.t
     ; pauser: Interactive_test.Pauser.t
     ; runner: Running_processes.State.t
     ; test_interactivity: Interactive_test.Interactivity.t
     ; .. >
  -> t
  -> node_uris:string list
  -> bakers:(string * string) list
  -> ( Running_processes.State.process_state
       * Running_processes.State.process_state
     , [> `Lwt_exn of exn | `Waiting_for of string * [`Time_out]] )
     Asynchronous_result.t
(** Start the Kiln and Postgres processes. [~network_id] is usually
    the chain-id of the sandbox, [~node_uris] is the list or URIs given to
    the ["--nodes"] option, if [~bakers] is not [[]] the test will force
    [state#pauser] to pause for the user to add the baker addresses to
    Kiln. *)

val cli_term : unit -> t option Cmdliner.Term.t
(** Build a {!Cmdliner.Term.t} which provides options like
    ["--with-kiln"] or ["--kiln-docker-image"]. *)
