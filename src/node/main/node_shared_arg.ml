(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Cmdliner
open P2p_types

let (//) = Filename.concat

type t = {
  data_dir: string option ;
  config_file: string ;
  min_connections: int option ;
  expected_connections: int option ;
  max_connections: int option ;
  max_download_speed: int option ;
  max_upload_speed: int option ;
  binary_chunks_size: int option ;
  peer_table_size: int option ;
  expected_pow: float option ;
  peers: string list ;
  no_bootstrap_peers: bool ;
  listen_addr: string option ;
  rpc_listen_addr: string option ;
  closed: bool ;
  cors_origins: string list ;
  cors_headers: string list ;
  rpc_tls: Node_config_file.tls option ;
  log_output: Logging.Output.t option ;
}

let wrap
    data_dir config_file
    connections max_download_speed max_upload_speed binary_chunks_size
    peer_table_size
    listen_addr peers no_bootstrap_peers closed expected_pow
    rpc_listen_addr rpc_tls
    cors_origins cors_headers log_output =

  let actual_data_dir =
    Utils.unopt ~default:Node_config_file.default_data_dir data_dir in

  let config_file =
    Utils.unopt ~default:(actual_data_dir // "config.json") config_file in

  let rpc_tls =
    Utils.map_option
      (fun (cert, key) -> { Node_config_file.cert ; key })
      rpc_tls in

  (* when `--expected-connections` is used,
     override all the bounds defined in the configuration file. *)
  let min_connections, expected_connections, max_connections =
    match connections with
    | None -> None, None, None
    | Some x -> Some (x/2), Some x, Some (3*x/2) in

  { data_dir ;
    config_file ;
    min_connections ;
    expected_connections ;
    max_connections ;
    max_download_speed ;
    max_upload_speed ;
    binary_chunks_size ;
    expected_pow ;
    peers ;
    no_bootstrap_peers ;
    listen_addr ;
    rpc_listen_addr ;
    closed ;
    cors_origins ;
    cors_headers ;
    rpc_tls ;
    log_output ;
    peer_table_size ;
  }

module Manpage = struct

  let misc_section = "MISC OPTIONS"
  let network_section = "NETWORK OPTIONS"
  let rpc_section = "RPC OPTIONS"

  let args = [
    `S network_section ;
    `S rpc_section ;
    `S misc_section ;
  ]

  let bugs = [
    `S "BUGS";
    `P "Check bug reports at https://github.com/tezos/tezos/issues.";
  ]

end

module Term = struct

  let log_output_converter =
    (fun s -> match Logging.Output.of_string s with
       | Some res -> `Ok res
       | None -> `Error s),
    Logging.Output.pp

  (* misc args *)

  let docs = Manpage.misc_section

  let log_output =
    let doc =
      "Log output. Either $(i,stdout), $(i,stderr), \
       $(i,syslog:<facility>) or a file path." in
    Arg.(value & opt (some log_output_converter) None &
         info ~docs ~docv:"OUTPUT" ~doc ["log-output"])

  let data_dir =
    let doc =
      "The directory where the Tezos node will store all its data." in
    Arg.(value & opt (some string) None &
         info ~docs ~doc ~docv:"DIR" ["data-dir"])

  let config_file =
    let doc = "The main configuration file." in
    Arg.(value & opt (some string) None &
         info ~docs ~doc ~docv:"FILE" ["config-file"])

  (* net args *)

  let docs = Manpage.network_section

  let connections =
    let doc =
      "The number of running connections that we aim for." in
    Arg.(value & opt (some int) None &
         info ~docs ~doc ~docv:"NUM" ["connections"])

  let max_download_speed =
    let doc =
      "The maximum number of bytes read per second." in
    Arg.(value & opt (some int) None &
         info ~docs ~doc ~docv:"NUM" ["max-download-speed"])

  let max_upload_speed =
    let doc =
      "The maximum number of bytes sent per second." in
    Arg.(value & opt (some int) None &
         info ~docs ~doc ~docv:"NUM" ["max-upload-speed"])

  let binary_chunks_size =
    let doc =
      Format.sprintf
      "Size limit (in kB) of binary blocks that are sent to other peers."
    in
    Arg.(value & opt (some int) None &
         info ~docs ~doc ~docv:"NUM" ["binary-chunks-size"])

  let peer_table_size =
    let doc = "Maximum size of internal peer tables, \
               used to store metadata/logs about a peer or about a \
               to-be-authenticated host:port couple." in
    Arg.(value & opt (some int) None &
         info ~docs ~doc ~docv:"NUM" ["peer-table-size"])

  let listen_addr =
    let doc =
      "The TCP address and port at which this instance can be reached." in
    Arg.(value & opt (some string) None &
         info ~docs ~doc ~docv:"ADDR:PORT" ["net-addr"])

  let no_bootstrap_peers =
    let doc =
      "Ignore the peers foud in the config file (or the hardcoded \
       bootstrap peers in the absence of config file)." in
    Arg.(value & flag &
         info ~docs ~doc ["no-bootstrap-peers"])

  let peers =
    let doc =
      "A peer to bootstrap the network from. \
       Can be used several times to add several peers." in
    Arg.(value & opt_all string [] &
         info ~docs ~doc ~docv:"ADDR:PORT" ["peer"])

  let expected_pow =
    let doc =
      "Expected level of proof-of-work for peers identity." in
    Arg.(value & opt (some float) None &
         info ~docs ~doc ~docv:"FLOAT" ["expected-pow"])

  let closed =
    let doc =
      "Only accept connections from the configured bootstrap peers." in
    Arg.(value & flag & info ~docs ~doc ["closed"])

  (* rpc args *)
  let docs = Manpage.rpc_section

  let rpc_listen_addr =
    let doc =
      "The TCP socket address at which this RPC server \
       instance can be reached." in
    Arg.(value & opt (some string) None &
         info ~docs ~doc ~docv:"ADDR:PORT" ["rpc-addr"])

  let rpc_tls =
    let doc =
      "Enable TLS for this RPC server \
       with the provided certificate and key." in
    Arg.(value & opt (some (pair string string)) None &
         info ~docs ~doc ~docv:"crt,key" ["rpc-tls"])

  let cors_origins =
    let doc =
      "CORS origin allowed by the RPC server \
       via Access-Control-Allow-Origin; may be used multiple times" in
    Arg.(value & opt_all string [] &
         info ~docs ~doc ~docv:"ORIGIN" ["cors-origin"])

  let cors_headers =
    let doc =
      "Header reported by Access-Control-Allow-Headers \
       reported during CORS preflighting; may be used multiple times" in
    Arg.(value & opt_all string [] &
         info ~docs ~doc ~docv:"HEADER" ["cors-header"])

  let args =
    let open Term in
    const wrap $ data_dir $ config_file
    $ connections
    $ max_download_speed $ max_upload_speed $ binary_chunks_size
    $ peer_table_size
    $ listen_addr $ peers $ no_bootstrap_peers $ closed $ expected_pow
    $ rpc_listen_addr $ rpc_tls
    $ cors_origins $ cors_headers
    $ log_output

end

let read_and_patch_config_file args =
  begin
    if Sys.file_exists args.config_file then
      Node_config_file.read args.config_file
    else
      return Node_config_file.default_config
  end >>=? fun cfg ->
  let { data_dir ;
        min_connections ; expected_connections ; max_connections ;
        max_download_speed ; max_upload_speed ; binary_chunks_size ;
        peer_table_size ;
        expected_pow ;
        peers ; no_bootstrap_peers ;
        listen_addr ; closed ;
        rpc_listen_addr ; rpc_tls ;
        cors_origins ; cors_headers ;
        log_output } = args in
  let bootstrap_peers =
    if no_bootstrap_peers then
      peers
    else
      cfg.net.bootstrap_peers @ peers in
  return @@
  Node_config_file.update
    ?data_dir ?min_connections ?expected_connections ?max_connections
    ?max_download_speed ?max_upload_speed ?binary_chunks_size
    ?peer_table_size ?expected_pow
    ~bootstrap_peers ?listen_addr ?rpc_listen_addr
    ~closed ~cors_origins ~cors_headers ?rpc_tls ?log_output cfg
