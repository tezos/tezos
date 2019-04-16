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
  include Internal_event.Legacy_logging.Make_semantic
      (struct let name = "node.distributed_db.p2p_peer_id" end)
  let mk_tag pp = Tag.def ~doc:"P2P peer ID" "p2p_peer_id" pp
  let tag = mk_tag pp_short
  let tag_opt = mk_tag (fun ppf -> function
      | None -> ()
      | Some peer -> pp_short ppf peer)
  let tag_source = Tag.def ~doc:"Peer which provided information" "p2p_peer_id_source" pp_source
end
