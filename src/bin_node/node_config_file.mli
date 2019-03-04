(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2019 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

[@@@ocaml.warning "-30"]

type t = {
  data_dir : string ;
  p2p : p2p ;
  rpc : rpc ;
  log : Lwt_log_sink_unix.cfg ;
  internal_events : Internal_event_unix.Configuration.t ;
  shell : shell ;
}

and p2p = {
  expected_pow : float ;
  bootstrap_peers : string list ;
  listen_addr : string option ;
  discovery_addr : string option ;
  private_mode : bool ;
  limits : P2p.limits ;
  disable_mempool : bool ;
  disable_testchain : bool ;
}

and rpc = {
  listen_addr : string option ;
  cors_origins : string list ;
  cors_headers : string list ;
  tls : tls option ;
}

and tls = {
  cert : string ;
  key : string ;
}

and shell = {
  block_validator_limits : Node.block_validator_limits ;
  prevalidator_limits : Node.prevalidator_limits ;
  peer_validator_limits : Node.peer_validator_limits ;
  chain_validator_limits : Node.chain_validator_limits ;
}

val default_data_dir: string
val default_p2p_port: int
val default_rpc_port: int
val default_p2p: p2p
val default_config: t

val update:
  ?data_dir:string ->
  ?min_connections:int ->
  ?expected_connections:int ->
  ?max_connections:int ->
  ?max_download_speed:int ->
  ?max_upload_speed:int ->
  ?binary_chunks_size:int->
  ?peer_table_size:int ->
  ?expected_pow:float ->
  ?bootstrap_peers:string list ->
  ?listen_addr:string ->
  ?discovery_addr:string ->
  ?rpc_listen_addr:string ->
  ?private_mode:bool ->
  ?disable_mempool:bool ->
  ?disable_testchain:bool ->
  ?cors_origins:string list ->
  ?cors_headers:string list ->
  ?rpc_tls:tls ->
  ?log_output:Lwt_log_sink_unix.Output.t ->
  ?bootstrap_threshold:int ->
  t -> t tzresult Lwt.t

val to_string: t -> string
val read: string -> t tzresult Lwt.t
val write: string -> t -> unit tzresult Lwt.t

val resolve_listening_addrs: string -> (P2p_addr.t * int) list Lwt.t
val resolve_discovery_addrs: string -> (Ipaddr.V4.t * int) list Lwt.t
val resolve_rpc_listening_addrs: string -> (P2p_addr.t * int) list Lwt.t
val resolve_bootstrap_addrs: string list -> (P2p_addr.t * int) list Lwt.t

val encoding: t Data_encoding.t

val check: t -> unit Lwt.t
