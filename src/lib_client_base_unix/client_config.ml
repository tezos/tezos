(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
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
         Format.fprintf ppf "Value %s is not a value block reference." s)
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
         Format.fprintf ppf "Value %s does not correspond to any known protocol." s)
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
         Format.fprintf ppf "Value %s is not a valid TCP port." s)
    Data_encoding.(obj1 (req "value" string))
    (function Invalid_port_arg s -> Some s | _ -> None)
    (fun s -> Invalid_port_arg s)


let default_base_dir =
  let home = try Sys.getenv "HOME" with Not_found -> "/root" in
  Filename.concat home ".tezos-client"

let default_block = `Head 0

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
    base_dir = default_base_dir ;
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
    Lwt_utils_unix.Json.read_file fp >>=? fun json ->
    return (from_json json)

  let write out cfg =
    Lwt_utils_unix.Json.write_file out
      (Data_encoding.Json.construct encoding cfg)

end

type cli_args = {
  block: Block_services.block ;
  protocol: Protocol_hash.t option ;
  print_timings: bool ;
  log_requests: bool ;
}

let default_cli_args = {
  block = default_block ;
  protocol = None ;
  print_timings = false ;
  log_requests = false ;
}


open Clic

let string_parameter () : (string, #Client_context.full) parameter =
  parameter (fun _ x -> return x)

let block_parameter () =
  parameter
    (fun _ block ->
       match Block_services.parse_block block with
       | Error _ -> fail (Invalid_block_argument block)
       | Ok block -> return block)

let protocol_parameter () =
  parameter
    (fun _ arg ->
       try
         let (hash,_commands) =
           List.find (fun (hash,_commands) ->
               String.has_prefix ~prefix:arg
                 (Protocol_hash.to_b58check hash))
             (Client_commands.get_versions ())
         in
         return (Some hash)
       with Not_found -> fail (Invalid_protocol_argument arg)
    )

(* Command-line only args (not in config file) *)
let base_dir_arg () =
  arg
    ~long:"base-dir"
    ~short:'d'
    ~placeholder:"path"
    ~doc:("client data directory\n\
           The directory where the Tezos client will store all its data.\n\
           By default: '" ^ default_base_dir ^"'.")
    (string_parameter ())
let config_file_arg () =
  arg
    ~long:"config-file"
    ~short:'c'
    ~placeholder:"path"
    ~doc:"configuration file"
    (string_parameter ())
let timings_switch () =
  switch
    ~long:"timings"
    ~short:'t'
    ~doc:"show RPC request times"
    ()
let block_arg () =
  default_arg
    ~long:"block"
    ~short:'b'
    ~placeholder:"hash|tag"
    ~doc:"block on which to apply contextual commands"
    ~default:(Block_services.to_string default_cli_args.block)
    (block_parameter ())
let protocol_arg () =
  arg
    ~long:"protocol"
    ~short:'p'
    ~placeholder:"hash"
    ~doc:"use commands of a specific protocol"
    (protocol_parameter ())
let log_requests_switch () =
  switch
    ~long:"log-requests"
    ~short:'l'
    ~doc:"log all requests to the node"
    ()

(* Command-line args which can be set in config file as well *)
let addr_arg () =
  arg
    ~long:"addr"
    ~short:'A'
    ~placeholder:"IP addr|host"
    ~doc:"IP address of the node"
    (string_parameter ())
let port_arg () =
  arg
    ~long:"port"
    ~short:'P'
    ~placeholder:"number"
    ~doc:"RPC port of the node"
    (parameter
       (fun _ x -> try
           return (int_of_string x)
         with Failure _ ->
           fail (Invalid_port_arg x)))
let tls_switch () =
  switch
    ~long:"tls"
    ~short:'S'
    ~doc:"use TLS to connect to node."
    ()

let read_config_file config_file =
  Lwt_utils_unix.Json.read_file config_file >>=? fun cfg_json ->
  try return @@ Cfg_file.from_json cfg_json
  with exn ->
    failwith
      "Can't parse the configuration file: %s@,%a"
      config_file (fun ppf exn -> Json_encoding.print_error ppf exn) exn

let default_config_file_name = "config"

let commands config_file cfg =
  let open Clic in
  let group = { Clic.name = "config" ;
                title = "Commands for editing and viewing the client's config file" } in
  [ command ~group ~desc:"Show the config file."
      no_options
      (fixed [ "config" ; "show" ])
      (fun () (cctxt : #Client_context.full) ->
         let pp_cfg ppf cfg = Format.fprintf ppf "%a" Data_encoding.Json.pp (Data_encoding.Json.construct Cfg_file.encoding cfg) in
         if not @@ Sys.file_exists config_file then
           cctxt#warning
             "@[<v 2>Warning: no config file at %s,@,\
              displaying the default configuration.@]"
             config_file >>= fun () ->
           cctxt#warning "%a@," pp_cfg Cfg_file.default >>= return
         else
           read_config_file config_file >>=? fun cfg ->
           cctxt#message "%a@," pp_cfg cfg >>= return) ;

    command ~group ~desc:"Reset the config file to the factory defaults."
      no_options
      (fixed [ "config" ; "reset" ])
      (fun () _cctxt ->
         Cfg_file.(write config_file default)) ;

    command ~group
      ~desc:"Update the config based on the current cli values.\n\
             Loads the current configuration (default or as specified \
             with `-config-file`), applies alterations from other \
             command line arguments (such as the node's address, \
             etc.), and overwrites the updated configuration file."
      no_options
      (fixed [ "config" ; "update" ])
      (fun () _cctxt ->
         Cfg_file.(write config_file cfg)) ;

    command ~group
      ~desc:"Create a config file based on the current CLI values.\n\
             If the `-file` option is not passed, this will initialize \
             the default config file, based on default parameters, \
             altered by other command line options (such as the node's \
             address, etc.).\n\
             Otherwise, it will create a new config file, based on the \
             default parameters (or the the ones specified with \
             `-config-file`), altered by other command line \
             options.\n\
             The command will always fail if the file already exists."
      (args1
         (default_arg
            ~long:"output"
            ~short:'o'
            ~placeholder:"path"
            ~doc:"path at which to create the file"
            ~default:(cfg.base_dir // default_config_file_name)
            (parameter (fun _ctx str -> return str))))
      (fixed [ "config" ; "init" ])
      (fun config_file _cctxt ->
         if not (Sys.file_exists config_file)
         then Cfg_file.(write config_file cfg) (* Should be default or command would have failed *)
         else failwith "Config file already exists at location") ;
  ]

let global_options () =
  args9 (base_dir_arg ())
    (config_file_arg ())
    (timings_switch ())
    (block_arg ())
    (protocol_arg ())
    (log_requests_switch ())
    (addr_arg ())
    (port_arg ())
    (tls_switch ())

let parse_config_args (ctx : #Client_context.full) argv =
  parse_global_options
    (global_options ())
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
        let base_dir = default_base_dir in
        unless (Sys.file_exists base_dir) begin fun () ->
          Lwt_utils_unix.create_dir base_dir >>= return
        end >>=? fun () ->
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
  end >>=? fun cfg ->
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
  Lwt_utils_unix.create_dir config_dir >>= fun () ->
  return
    (cfg,
     { block ; print_timings = timings ; log_requests ; protocol },
     commands config_file cfg, remaining)
