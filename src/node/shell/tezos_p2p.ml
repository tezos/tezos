
module Param = struct

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

  let msg_encodings =
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

  type meta = unit
  let init_meta = ()
  let score_enc = Data_encoding.empty
  let score () = 0.

  let supported_versions =
    let open P2p in
    [ { name = "TEZOS" ;
        major = 0 ;
        minor = 0 ;
      }
    ]

end

include Param
include P2p.Make(Param)
