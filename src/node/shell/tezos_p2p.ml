
open P2p

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

module Message = struct

  type t = msg

  let encoding =
    let open Data_encoding in
    let case ?max_length ~tag encoding unwrap wrap =
      P2p.Encoding { tag; encoding; wrap; unwrap; max_length } in
    [
      case ~tag:0x10 (tup2 Block_hash.encoding (list Block_hash.encoding))
        (function
          | Discover_blocks (Net genesis_bh, bhs) -> Some (genesis_bh, bhs)
          | _ -> None)
        (fun (genesis_bh, bhs) -> Discover_blocks (Net genesis_bh, bhs));
      case ~tag:0x11 (tup2 Block_hash.encoding (list Block_hash.encoding))
        (function
          | Block_inventory (Net genesis_bh, bhs) -> Some (genesis_bh, bhs)
          | _ -> None)
        (fun (genesis_bh, bhs) -> Block_inventory (Net genesis_bh, bhs));

      case ~tag:0x12 (list Block_hash.encoding)
        (function
          | Get_blocks bhs -> Some bhs
          | _ -> None)
        (fun bhs -> Get_blocks bhs);
      case ~tag:0x13 Data_encoding.bytes
        (function Block b -> Some b  | _ -> None)
        (fun b -> Block b);

      case ~tag:0x20 Block_hash.encoding
        (function Current_operations (Net genesis_bh) -> Some genesis_bh | _ -> None)
        (fun genesis_bh -> Current_operations (Net genesis_bh));
      case ~tag:0x21 (tup2 Block_hash.encoding (list Operation_hash.encoding))
        (function Operation_inventory ((Net genesis_bh), ops) -> Some (genesis_bh, ops) | _ -> None)
        (fun (genesis_bh, ops) -> Operation_inventory (Net genesis_bh, ops));

      case ~tag:0x22 (list Operation_hash.encoding)
        (function
          | Get_operations ops -> Some ops
          | _ -> None)
        (fun ops -> Get_operations ops);
      case ~tag:0x23 Data_encoding.bytes
        (function Operation o -> Some o  | _ -> None)
        (fun o -> Operation o);

      case ~tag:0x32 (list Protocol_hash.encoding)
        (function
          | Get_protocols protos -> Some protos
          | _ -> None)
        (fun protos -> Get_protocols protos);
      case ~tag:0x33 Data_encoding.bytes
        (function Protocol proto -> Some proto  | _ -> None)
        (fun proto -> Protocol proto);
    ]

  let supported_versions =
    let open P2p.Version in
    [ { name = "TEZOS" ;
        major = 0 ;
        minor = 0 ;
      }
    ]

end

type metadata = unit

module Metadata = struct
  type t = metadata
  let initial = ()
  let encoding = Data_encoding.empty
  let score () = 0.
end


let meta_cfg : _ P2p.meta_config = {
  P2p.encoding = Metadata.encoding ;
  initial = Metadata.initial ;
  score = Metadata.score ;
}

and msg_cfg : _ P2p.message_config = {
  encoding = Message.encoding ;
  versions = Message.supported_versions ;
}

type net = (Message.t, Metadata.t) P2p.net
type pool = (Message.t, Metadata.t) P2p_connection_pool.t

let create ~config ~limits =
  P2p.create ~config ~limits meta_cfg msg_cfg

let broadcast = P2p.broadcast
let try_send = P2p.try_send
let recv = P2p.recv_any
let send = P2p.send
let set_metadata = P2p.set_metadata
let get_metadata = P2p.get_metadata
let connection_info = P2p.connection_info
let find_connection = P2p.find_connection
let connections = P2p.connections
type connection = (Message.t, Metadata.t) P2p.connection
let shutdown = P2p.shutdown
let roll = P2p.roll
let maintain = P2p.maintain
let faked_network = P2p.faked_network

module Raw = struct
  type 'a t = 'a P2p.Raw.t =
    | Bootstrap
    | Advertise of Point.t list
    | Message of 'a
    | Disconnect
  type message = Message.t t
  let encoding = P2p.Raw.encoding msg_cfg.encoding
  let supported_versions = msg_cfg.versions
end

module RPC = struct
  let stat net = P2p.RPC.stat net

  module Event = P2p.RPC.Event

  let watch = P2p.RPC.watch

  let connect = P2p.RPC.connect

  module Connection = struct
    let info = P2p.RPC.Connection.info
    let kick = P2p.RPC.Connection.kick
    let list = P2p.RPC.Connection.list
    let count = P2p.RPC.Connection.count
  end

  module Point = struct
    type info = P2p.RPC.Point.info
    module Event = P2p_connection_pool_types.Point_info.Event

    let info = P2p.RPC.Point.info
    let events = P2p.RPC.Point.events
    let infos = P2p.RPC.Point.infos
    let watch = P2p.RPC.Point.watch
  end

  module Peer_id = struct
    type info = P2p.RPC.Peer_id.info
    module Event = P2p_connection_pool_types.Peer_info.Event

    let info = P2p.RPC.Peer_id.info
    let events = P2p.RPC.Peer_id.events
    let infos = P2p.RPC.Peer_id.infos
    let watch = P2p.RPC.Peer_id.watch
  end
end
