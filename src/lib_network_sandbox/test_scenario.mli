(** Build and manage Network Sandboxes. *)

open Internal_pervasives

module Inconsistency_error : sig
  type t = [`Empty_protocol_list | `Too_many_protocols of Tezos_protocol.t list]

  val should_be_one_protocol :
       'a list
    -> ( 'a
       , [> `Empty_protocol_list | `Too_many_protocols of 'a list] Error.t )
       result
       Lwt.t

  val pp :
       Format.formatter
    -> [< `Empty_protocol_list | `Too_many_protocols of 'a Base.List.t]
    -> unit
end

(** Build {i static} tezos network topologies. *)
module Topology : sig
  type node = Tezos_node.t

  type _ t = private
    | Mesh : {size: int} -> node list t
    | Bottleneck :
        { name: string
        ; left: 'a network
        ; right: 'b network }
        -> ('a * node * 'b) t
    | Net_in_the_middle :
        { middle: 'm network
        ; left: 'a network
        ; right: 'b network }
        -> ('a * 'm * 'b) t

  and 'a network = {topology: 'a t; name: string}

  val mesh : string -> int -> node list network
  val sub : string -> 'a t -> 'a network

  val bottleneck :
    string -> 'a network -> 'b network -> ('a * node * 'b) network

  val node_count : 'a t -> int
  val node_ids : 'a t -> 'a -> string list

  val net_in_the_middle :
    string -> 'a network -> 'b network -> 'c network -> ('b * 'a * 'c) network

  val build :
       ?protocol:Tezos_protocol.t
    -> ?base_port:int
    -> exec:[`Node] Tezos_executable.t
    -> 'a network
    -> 'a
end

(** Start networks from (and manipulate) {!Topology.t} values. *)
module Network : sig
  type t = private {nodes: Tezos_node.t list}

  val make : Tezos_node.t list -> t

  val netstat_listening_ports :
       < paths: Paths.t ; runner: Running_processes.State.t ; .. >
    -> ( (int * [> `Tcp of int * string list]) list
       , [> `Lwt_exn of exn | Process_result.Error.t] )
       Asynchronous_result.t
  (** Call ["netstat"] to find TCP ports already in use. *)

  val start_up :
       ?check_ports:bool
    -> < paths: Paths.t ; runner: Running_processes.State.t ; .. >
    -> client_exec:[`Client] Tezos_executable.t
    -> t
    -> ( unit
       , [> `Empty_protocol_list
         | `Lwt_exn of exn
         | `Sys_error of string
         | Process_result.Error.t
         | `Too_many_protocols of Tezos_protocol.t list ] )
       Asynchronous_result.t
end

val network_with_protocol :
     ?base_port:int
  -> ?size:int
  -> ?protocol:Tezos_protocol.t
  -> < paths: Paths.t ; runner: Running_processes.State.t ; .. >
  -> node_exec:[`Node] Tezos_executable.t
  -> client_exec:[`Client] Tezos_executable.t
  -> ( Tezos_node.t list * Tezos_protocol.t
     , [> `Empty_protocol_list
       | `Lwt_exn of exn
       | `Sys_error of string
       | Process_result.Error.t
       | `Too_many_protocols of Tezos_protocol.t list ] )
     Asynchronous_result.t
(** [network_with_protocol] is a wrapper simply starting-up a
    {!Topology.mesh}. *)

(** Run queries on running networks. *)
module Queries : sig
  val all_levels :
       < application_name: string
       ; console: Console.t
       ; paths: Paths.t
       ; runner: Running_processes.State.t
       ; .. >
    -> nodes:Tezos_node.t list
    -> ( (string * [> `Failed | `Level of int | `Null | `Unknown of string])
         list
       , [> `Lwt_exn of exn] )
       Asynchronous_result.t
  (** Get the current chain level for all the nodes, returns {i
      node-ID × level } values. *)

  val wait_for_all_levels_to_be :
       < application_name: string
       ; console: Console.t
       ; paths: Paths.t
       ; runner: Running_processes.State.t
       ; .. >
    -> attempts:int
    -> seconds:float
    -> Tezos_node.t list
    -> [< `At_least of int | `Equal_to of int]
    -> ( unit
       , [> `Lwt_exn of exn | `Waiting_for of string * [`Time_out]] )
       Asynchronous_result.t
  (** Try-sleep-loop waiting for all given nodes to reach a given level. *)
end
