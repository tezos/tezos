
open P2p

type net

(** A faked p2p layer, which do not initiate any connection
    nor open any listening socket *)
val faked_network : net

(** Main network initialisation function *)
val bootstrap : config:config -> limits:limits -> net Lwt.t

(** A maintenance operation : try and reach the ideal number of peers *)
val maintain : net -> unit Lwt.t

(** Voluntarily drop some peers and replace them by new buddies *)
val roll : net -> unit Lwt.t

(** Close all connections properly *)
val shutdown : net -> unit Lwt.t

(** A connection to a peer *)
type peer

(** Access the domain of active peers *)
val peers : net -> peer list

(** Return the active peer with identity [gid] *)
val find_peer : net -> gid -> peer option

type peer_info = {
  gid : gid ;
  addr : addr ;
  port : port ;
  version : version ;
}

(** Access the info of an active peer, if available *)
val peer_info : net -> peer -> peer_info

(** Accessors for meta information about a global identifier *)

type metadata = unit

val get_metadata : net -> gid -> metadata option
val set_metadata : net -> gid -> metadata -> unit

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

(** Wait for a payload from any peer in the network *)
val recv : net -> (peer * msg) Lwt.t

(** Send a payload to a peer and wait for it to be in the tube *)
val send : net -> peer -> msg -> unit Lwt.t

(** Send a payload to a peer without waiting for the result. Return
    [true] if the msg can be enqueued in the peer's output queue
    or [false] otherwise. *)
val try_send : net -> peer -> msg -> bool

(** Send a payload to all peers *)
val broadcast : net -> msg -> unit

(** Shutdown the connection to all peers at this address and stop the
    communications with this machine for [duration] seconds *)
val blacklist : net -> gid -> unit

(** Keep a connection to this pair as often as possible *)
val whitelist : net -> gid -> unit
