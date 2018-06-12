(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

include Crypto_box.Public_key_hash

let rpc_arg =
  RPC_arg.like
    rpc_arg
    ~descr:"A cryptographic node identity (Base58Check-encoded)"
    "peer_id"

let pp_source ppf = function
  | None -> ()
  | Some peer -> Format.fprintf ppf " from peer %a" pp peer

module Logging = struct
  open Tezos_stdlib.Logging
  include Make_semantic(struct let name = "node.distributed_db.p2p_reader" end)
  let mk_tag pp = Tag.def ~doc:"P2P peer ID" "p2p_peer_id" pp
  let tag = mk_tag pp_short
  let tag_opt = mk_tag (fun ppf -> function
      | None -> ()
      | Some peer -> pp_short ppf peer)
  let tag_source = Tag.def ~doc:"Peer which provided information" "p2p_peer_id_source" pp_source
end
