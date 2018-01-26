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
  block: Block_services.block ;
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
    (fun _ block -> match Block_services.parse_block block with
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
  arg
    ~parameter:"-base-dir"
    ~doc:("The directory where the Tezos client will store all its data. By default "
          ^ Client_commands.default_base_dir)
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
    ~default:(Block_services.to_string default_cli_args.block)
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
  arg
    ~parameter:"-addr"
    ~doc:"The IP address of the node."
    string_parameter
let port_arg =
  arg
    ~parameter:"-port"
    ~doc:"The RPC port of the node."
    (parameter
       (fun _ x -> try
           return (int_of_string x)
         with Failure _ ->
           fail (Invalid_port_arg x)))
let tls_switch =
  switch
    ~parameter:"-tls"
    ~doc:"Use TLS to connect to node."

let read_config_file config_file = match
    Utils.read_file ~bin:false config_file
    |> Data_encoding_ezjsonm.from_string
  with
  | exception (Sys_error msg) ->
      failwith
        "Error: can't read the configuration file: %s@,%s"
        config_file msg
  | Error msg ->
      failwith
        "Can't parse the configuration file: %s@,%s"
        config_file msg
  | Ok cfg_json ->
      try return @@ Cfg_file.from_json cfg_json
      with exn ->
        failwith
          "Can't parse the configuration file: %s@,%a"
          config_file (fun ppf exn -> Json_encoding.print_error ppf exn) exn

let default_config_file_name = "config"

let commands config_file cfg =
  let open Cli_entries in
  let group = { Cli_entries.name = "config" ;
                title = "Commands for editing and viewing the client's config file." } in
  [ command ~group ~desc:"show the config file"
      no_options
      (fixed [ "config" ; "show" ])
      (fun () (cctxt : Client_commands.full_context) ->
         let pp_cfg ppf cfg = Format.fprintf ppf "%a" Data_encoding_ezjsonm.pp (Data_encoding.Json.construct Cfg_file.encoding cfg) in
         if not @@ Sys.file_exists config_file then
           cctxt#warning
             "@[<v 2>Warning: no config file at %s,@,\
              displaying the default configuration.@]"
             config_file >>= fun () ->
           cctxt#warning "%a@," pp_cfg Cfg_file.default >>= return
         else
           read_config_file config_file >>=? fun cfg ->
           cctxt#message "%a@," pp_cfg cfg >>= return) ;

    command ~group ~desc:"reset the config file to the factory defaults"
      no_options
      (fixed [ "config" ; "reset" ])
      (fun () _cctxt ->
         return Cfg_file.(write config_file default)) ;

    command ~group ~desc:"update the config based on the current cli values"
      no_options
      (fixed [ "config" ; "update" ])
      (fun () _cctxt ->
         return Cfg_file.(write config_file cfg)) ;

    command ~group ~desc:"create a config file based on the current CLI values"
      (args1
         (default_arg
            ~parameter:"-path"
            ~doc:"path at which to create the file"
            ~default:(cfg.base_dir // default_config_file_name)
            (parameter (fun _ctx str -> return str))))
      (fixed [ "config" ; "init" ])
      (fun config_file _cctxt ->
         if not (Sys.file_exists config_file)
         then return Cfg_file.(write config_file cfg) (* Should be default or command would have failed *)
         else failwith "Config file already exists at location") ;
  ]

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
    argv >>=?
  fun ((base_dir,
        config_file,
        timings,
        block,
        protocol,
        log_requests,
        node_addr,
        node_port,
        tls), remaining) ->
  begin match base_dir with
    | None ->
        let base_dir = Client_commands.default_base_dir in
        if not (Sys.file_exists base_dir)
        then Utils.mkdir base_dir ;
        return base_dir
    | Some dir ->
        if not (Sys.file_exists dir)
        then failwith "Specified -base-dir does not exist. Please create the directory and try again."
        else if Sys.is_directory dir
        then return dir
        else failwith "Specified -base-dir must be a directory"
  end >>=? fun base_dir ->
  begin match config_file with
    | None -> return @@ base_dir // default_config_file_name
    | Some config_file ->
        if Sys.file_exists config_file
        then return config_file
        else failwith "Config file specified in option does not exist. Use `client config init` to create one."
  end >>=? fun config_file ->
  let config_dir = Filename.dirname config_file in
  let protocol =
    match protocol with
    | None -> None
    | Some p -> p
  in
  begin
    if not (Sys.file_exists config_file) then
      return { Cfg_file.default with base_dir = base_dir }
    else
      read_config_file config_file
  end >>|? fun cfg ->
  let tls = cfg.tls || tls in
  let node_addr = Option.unopt ~default:cfg.node_addr node_addr in
  let node_port = Option.unopt ~default:cfg.node_port node_port in
  let cfg = { cfg with tls ; node_port ; node_addr } in
  if Sys.file_exists base_dir && not (Sys.is_directory base_dir) then begin
    Format.eprintf "%s is not a directory.@." base_dir ;
    exit 1 ;
  end ;
  if Sys.file_exists config_dir && not (Sys.is_directory config_dir) then begin
    Format.eprintf "%s is not a directory.@." config_dir ;
    exit 1 ;
  end ;
  Utils.mkdir config_dir ;
  (cfg, { block ; print_timings = timings ; log_requests ; protocol }, commands config_file cfg, remaining)
