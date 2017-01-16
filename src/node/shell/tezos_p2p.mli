
open P2p

type net

(** A faked p2p layer, which do not initiate any connection
    nor open any listening socket *)
val faked_network : net

(** Main network initialisation function *)
val bootstrap : config:config -> limits:limits -> net Lwt.t

(** A maintenance operation : try and reach the ideal number of peers *)
val maintain : net -> unit Lwt.t

(** Voluntarily drop some connections and replace them by new buddies *)
val roll : net -> unit Lwt.t

(** Close all connections properly *)
val shutdown : net -> unit Lwt.t

(** A connection to a peer *)
type connection

(** Access the domain of active connections *)
val connections : net -> connection list

(** Return the active connection with identity [gid] *)
val find_connection : net -> Gid.t -> connection option

(** Access the info of an active connection. *)
val connection_info : net -> connection -> Connection_info.t

(** Accessors for meta information about a global identifier *)

type metadata = unit

val get_metadata : net -> Gid.t -> metadata option
val set_metadata : net -> Gid.t -> metadata -> unit

type net_id = Store.net_id

type msg =

  | Discover_blocks of net_id * Block_hash.t list (* Block locator *)
  | Block_inventory of net_id * Block_hash.t list

  | Get_blocks of Block_hash.t list
  | Block of MBytes.t

  | Current_operations of net_id
  | Operation_inventory of net_id * Operation_hash.t list

  | Get_operations of Operation_hash.t list
  | Operation of MBytes.t

  | Get_protocols of Protocol_hash.t list
  | Protocol of MBytes.t

(** Wait for a payload from any connection in the network *)
val recv : net -> (connection * msg) Lwt.t

(** [send net conn msg] is a thread that returns when [msg] has been
    successfully enqueued in the send queue. *)
val send : net -> connection -> msg -> unit Lwt.t

(** [try_send net conn msg] is [true] if [msg] has been added to the
    send queue for [peer], [false] otherwise *)
val try_send : net -> connection -> msg -> bool

(** Send a payload to all peers *)
val broadcast : net -> msg -> unit

(**/**)
module Raw : sig
  type 'a t =
    | Bootstrap
    | Advertise of Point.t list
    | Message of 'a
    | Disconnect
  type message = msg t
  val encoding: message Data_encoding.t
  val supported_versions: Version.t list
end
