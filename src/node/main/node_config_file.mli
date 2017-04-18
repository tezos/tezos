(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open P2p_types

type t = {
  data_dir : string ;
  net : net ;
  rpc : rpc ;
  log : log ;
}

and net = {
  expected_pow : float ;
  bootstrap_peers : string list ;
  listen_addr : string option ;
  closed : bool ;
  limits : P2p.limits ;
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

and log = {
  output : Logging.Output.t ;
  default_level : Logging.level ;
  rules : string option ;
  template : Logging.template ;
}

val default_data_dir: string
val default_net_port: int
val default_rpc_port: int
val default_net: net
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
  ?rpc_listen_addr:string ->
  ?closed:bool ->
  ?cors_origins:string list ->
  ?cors_headers:string list ->
  ?rpc_tls:tls ->
  ?log_output:Logging.Output.t ->
  t -> t

val to_string: t -> string
val read: string -> t tzresult Lwt.t
val write: string -> t -> unit tzresult Lwt.t

val resolve_listening_addrs: string -> (P2p_types.addr * int) list Lwt.t
val resolve_rpc_listening_addrs: string -> (P2p_types.addr * int) list Lwt.t
val resolve_bootstrap_addrs: string list -> (P2p_types.addr * int) list Lwt.t

val check: t -> unit Lwt.t
