(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(* Tezos Command line interface - Configuration and Arguments Parsing *)

type error += Invalid_block_argument of string
type error += Invalid_protocol_argument of string
type error += Invalid_port_arg of string
let () =
  register_error_kind
    `Branch
    ~id: "badBlockArgument"
    ~title: "Bad Block Argument"
    ~description: "Block argument could not be parsed"
    ~pp:
      (fun ppf s ->
         Format.fprintf ppf "Value provided for -block flag (%s) could not be parsed" s)
    Data_encoding.(obj1 (req "value" string))
    (function Invalid_block_argument s -> Some s | _ -> None)
    (fun s -> Invalid_block_argument s) ;
  register_error_kind
    `Branch
    ~id: "badProtocolArgument"
    ~title: "Bad Protocol Argument"
    ~description: "Protocol argument could not be parsed"
    ~pp:
      (fun ppf s ->
         Format.fprintf ppf "Value provided for -protocol flag (%s) does not correspond to any known protocol" s)
    Data_encoding.(obj1 (req "value" string))
    (function Invalid_protocol_argument s -> Some s | _ -> None)
    (fun s -> Invalid_protocol_argument s) ;
  register_error_kind
    `Branch
    ~id: "invalidPortArgument"
    ~title: "Bad Port Argument"
    ~description: "Port argument could not be parsed"
    ~pp:
      (fun ppf s ->
         Format.fprintf ppf "Value provided for -port flag (%s) could not be parsed" s)
    Data_encoding.(obj1 (req "value" string))
    (function Invalid_port_arg s -> Some s | _ -> None)
    (fun s -> Invalid_port_arg s)


let (//) = Filename.concat

module Cfg_file = struct

  type t =  {
    base_dir: string ;
    node_addr: string ;
    node_port: int ;
    tls: bool ;
    web_port: int ;
  }

  let default = {
    base_dir = Client_commands.default_base_dir ;
    node_addr = "localhost" ;
    node_port = 8732 ;
    tls = false ;
    web_port = 8080 ;
  }

  open Data_encoding

  let encoding =
    conv
      (fun { base_dir ; node_addr ; node_port ; tls ; web_port } ->
         (base_dir, Some node_addr, Some node_port,
          Some tls, Some web_port))
      (fun (base_dir, node_addr, node_port, tls, web_port) ->
         let node_addr = Option.unopt ~default:default.node_addr node_addr in
         let node_port = Option.unopt ~default:default.node_port node_port in
         let tls = Option.unopt ~default:default.tls tls in
         let web_port = Option.unopt ~default:default.web_port web_port in
         { base_dir ; node_addr ; node_port ; tls ; web_port })
      (obj5
         (req "base_dir" string)
         (opt "node_addr" string)
         (opt "node_port" int16)
         (opt "tls" bool)
         (opt "web_port" int16))

  let from_json json =
    Data_encoding.Json.destruct encoding json

  let read fp =
    Data_encoding_ezjsonm.read_file fp >>=? fun json ->
    return (from_json json)

  let write out cfg =
    Utils.write_file ~bin:false out
      (Data_encoding.Json.construct encoding cfg |>
       Data_encoding_ezjsonm.to_string)

end

type cli_args = {
  block: Node_rpc_services.Blocks.block ;
  protocol: Protocol_hash.t option ;
  print_timings: bool ;
  log_requests: bool ;
}

let default_cli_args = {
  block = Client_commands.default_block ;
  protocol = None ;
  print_timings = false ;
  log_requests = false ;
}


open Cli_entries

let string_parameter : (string, Client_commands.full_context) parameter =
  parameter (fun _ x -> return x)

let block_parameter =
  parameter
    (fun _ block -> match Node_rpc_services.Blocks.parse_block block with
       | Error _ -> fail (Invalid_block_argument block)
       | Ok block -> return block)

let protocol_parameter =
  parameter
    (fun _ arg ->
       try
         let (hash,_commands) =
           List.find (fun (hash,_commands) ->
               (Protocol_hash.to_short_b58check hash) = arg
             ) (Client_commands.get_versions ())
         in
         return (Some hash)
       with Not_found -> fail (Invalid_protocol_argument arg)
    )

(* Command-line only args (not in config file) *)
let base_dir_arg =
  default_arg
    ~parameter:"-base-dir"
    ~doc:"The directory where the Tezos client will store all its data."
    ~default:Client_commands.default_base_dir
    string_parameter
let config_file_arg =
  arg
    ~parameter:"-config-file"
    ~doc:"The main configuration file."
    string_parameter
let timings_switch =
  switch
    ~parameter:"-timings"
    ~doc:"Show RPC request times if present."
let block_arg =
  default_arg
    ~parameter:"-block"
    ~doc:"The block on which to apply contextual commands."
    ~default:(Node_rpc_services.Blocks.to_string default_cli_args.block)
    block_parameter
let protocol_arg =
  arg
    ~parameter:"-protocol"
    ~doc:"Use contextual commands of a specific protocol."
    protocol_parameter
let log_requests_switch =
  switch
    ~parameter:"-log-requests"
    ~doc:"Causes all requests and responses to the node to be logged."

(* Command-line args which can be set in config file as well *)
let addr_arg =
  default_arg
    ~parameter:"-addr"
    ~doc:"The IP address of the node."
    ~default:Cfg_file.default.node_addr
    string_parameter
let port_arg =
  default_arg
    ~parameter:"-port"
    ~doc:"The RPC port of the node."
    ~default:(string_of_int Cfg_file.default.node_port)
    (parameter
       (fun _ x -> try
           return (int_of_string x)
         with Failure _ ->
           fail (Invalid_port_arg x)))
let tls_switch =
  switch
    ~parameter:"-tls"
    ~doc:"Use TLS to connect to node."

let global_options =
  args9 base_dir_arg
    config_file_arg
    timings_switch
    block_arg
    protocol_arg
    log_requests_switch
    addr_arg
    port_arg
    tls_switch

let parse_config_args (ctx : Client_commands.full_context) argv =
  parse_initial_options
    global_options
    ctx
    argv >>|?
  fun ((base_dir,
        config_file,
        timings,
        block,
        protocol,
        log_requests,
        node_addr,
        node_port,
        tls), remaining) ->
  let config_file =
    match config_file with
    | None -> base_dir // "config"
    | Some config_file -> config_file in
  let config_dir = Filename.dirname config_file in
  let protocol =
    match protocol with
    | None -> None
    | Some p -> p
  in
  let cfg =
    if not (Sys.file_exists config_file) then
      { Cfg_file.default with base_dir = base_dir }
    else
      match
        Utils.read_file ~bin:false config_file
        |> Data_encoding_ezjsonm.from_string
      with
      | exception (Sys_error msg) ->
          Format.eprintf
            "Error: can't read the configuration file: %s\n%s@."
            config_file msg ;
          exit 1
      | exception _ ->
          Format.eprintf "Warning: config file not found@." ;
          { Cfg_file.default with base_dir = base_dir }
      | Error msg ->
          Format.eprintf
            "Error: can't parse the configuration file: %s\n%s@."
            config_file msg ;
          exit 1
      | Ok cfg_json ->
          try Cfg_file.from_json cfg_json
          with exn ->
            Format.eprintf
              "Error: can't parse the configuration file: %s\n%a@."
              config_file (fun ppf exn -> Json_encoding.print_error ppf exn) exn ;
            exit 1 in
  let tls = cfg.tls || tls in
  let cfg = { cfg with tls ; node_port ; node_addr } in
  if Sys.file_exists base_dir && not (Sys.is_directory base_dir) then begin
    Format.eprintf "Error: %s is not a directory.@." base_dir ;
    exit 1 ;
  end ;
  Utils.mkdir base_dir ;
  if Sys.file_exists config_dir && not (Sys.is_directory config_dir) then begin
    Format.eprintf "Error: %s is not a directory.@." config_dir ;
    exit 1 ;
  end ;
  Utils.mkdir config_dir ;
  if not (Sys.file_exists config_file) then Cfg_file.write config_file cfg ;
  (cfg, { block ; print_timings = timings ; log_requests ; protocol }, remaining)
