(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2018 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

(* Tezos Command line interface - Configuration and Arguments Parsing *)

type error += Invalid_chain_argument of string
type error += Invalid_block_argument of string
type error += Invalid_protocol_argument of string
type error += Invalid_port_arg of string
type error += Invalid_remote_signer_argument of string
type error += Invalid_wait_arg of string
let () =
  register_error_kind
    `Branch
    ~id: "badChainArgument"
    ~title: "Bad Chain Argument"
    ~description: "Chain argument could not be parsed"
    ~pp:
      (fun ppf s ->
         Format.fprintf ppf "Value %s is not a value chain reference." s)
    Data_encoding.(obj1 (req "value" string))
    (function Invalid_chain_argument s -> Some s | _ -> None)
    (fun s -> Invalid_chain_argument s) ;
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
    (fun s -> Invalid_port_arg s) ;
  register_error_kind
    `Branch
    ~id: "invalid_remote_signer_argument"
    ~title: "Unexpected URI of remote signer"
    ~description: "The remote signer argument could not be parsed"
    ~pp:
      (fun ppf s ->
         Format.fprintf ppf "Value '%s' is not a valid URI." s)
    Data_encoding.(obj1 (req "value" string))
    (function Invalid_remote_signer_argument s -> Some s | _ -> None)
    (fun s -> Invalid_remote_signer_argument s) ;
  register_error_kind
    `Branch
    ~id: "invalidWaitArgument"
    ~title: "Bad Wait Argument"
    ~description: "Wait argument could not be parsed"
    ~pp:
      (fun ppf s ->
         Format.fprintf ppf "Value %s is not a valid number of confirmation, nor 'none'." s)
    Data_encoding.(obj1 (req "value" string))
    (function Invalid_wait_arg s -> Some s | _ -> None)
    (fun s -> Invalid_wait_arg s)

let home = try Sys.getenv "HOME" with Not_found -> "/root"

let default_base_dir =
  Filename.concat home ".tezos-client"

let default_chain = `Main
let default_block = `Head 0

let (//) = Filename.concat

module Cfg_file = struct

  type t =  {
    base_dir: string ;
    node_addr: string ;
    node_port: int ;
    tls: bool ;
    web_port: int ;
    remote_signer: Uri.t option ;
    confirmations: int option ;
    password_filename: string option ;
  }

  let default = {
    base_dir = default_base_dir ;
    node_addr = "localhost" ;
    node_port = 8732 ;
    tls = false ;
    web_port = 8080 ;
    remote_signer = None ;
    confirmations = Some 0 ;
    password_filename = None ;
  }

  open Data_encoding

  let encoding =
    conv
      (fun { base_dir ; node_addr ; node_port ; tls ; web_port ;
             remote_signer ; confirmations ; password_filename } ->
        (base_dir, Some node_addr, Some node_port,
         Some tls, Some web_port, remote_signer, confirmations, password_filename ))
      (fun (base_dir, node_addr, node_port, tls, web_port,
            remote_signer, confirmations, password_filename) ->
        let node_addr = Option.unopt ~default:default.node_addr node_addr in
        let node_port = Option.unopt ~default:default.node_port node_port in
        let tls = Option.unopt ~default:default.tls tls in
        let web_port = Option.unopt ~default:default.web_port web_port in
        { base_dir ; node_addr ; node_port ; tls ; web_port ;
          remote_signer ; confirmations ; password_filename })
      (obj8
         (req "base_dir" string)
         (opt "node_addr" string)
         (opt "node_port" int16)
         (opt "tls" bool)
         (opt "web_port" int16)
         (opt "remote_signer" RPC_client.uri_encoding)
         (opt "confirmations" int8)
         (opt "password_filename" string)
      )

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
  chain: Chain_services.chain ;
  block: Shell_services.block ;
  confirmations: int option ;
  password_filename: string option ;
  protocol: Protocol_hash.t option ;
  print_timings: bool ;
  log_requests: bool ;
}

let default_cli_args = {
  chain = default_chain ;
  block = default_block ;
  confirmations = Some 0 ;
  password_filename = None ;
  protocol = None ;
  print_timings = false ;
  log_requests = false ;
}


open Clic

let string_parameter () : (string, #Client_context.full) parameter =
  parameter (fun _ x -> return x)

let chain_parameter () =
  parameter
    (fun _ chain ->
       match Chain_services.parse_chain chain with
       | Error _ -> fail (Invalid_chain_argument chain)
       | Ok chain -> return chain)

let block_parameter () =
  parameter
    (fun _ block ->
       match Block_services.parse_block block with
       | Error _ -> fail (Invalid_block_argument block)
       | Ok block -> return block)

let wait_parameter () =
  parameter
    (fun _ wait ->
       match wait with
       | "no" | "none" -> return_none
       | _ ->
           try
             let w = int_of_string wait in
             if 0 <= w then
               return_some w
             else
               fail (Invalid_wait_arg wait)
           with _ -> fail (Invalid_wait_arg wait))

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
         return_some hash
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
let chain_arg () =
  default_arg
    ~long:"chain"
    ~placeholder:"hash|tag"
    ~doc:"chain on which to apply contextual commands (possible tags \
          are 'main' and 'test')"
    ~default:(Chain_services.to_string default_cli_args.chain)
    (chain_parameter ())
let block_arg () =
  default_arg
    ~long:"block"
    ~short:'b'
    ~placeholder:"hash|tag"
    ~doc:"block on which to apply contextual commands (possible tags \
          are 'head' and 'genesis')"
    ~default:(Block_services.to_string default_cli_args.block)
    (block_parameter ())
let wait_arg () =
  arg
    ~long:"wait"
    ~short:'w'
    ~placeholder:"none|<int>"
    ~doc:"how many confirmation blocks before to consider an operation as included"
    (wait_parameter ())
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
let remote_signer_arg () =
  arg
    ~long:"remote-signer"
    ~short:'R'
    ~placeholder:"uri"
    ~doc:"URI of the remote signer"
    (parameter
       (fun _ x -> Tezos_signer_backends.Remote.parse_base_uri x))
let password_filename_arg () =
  arg
    ~long:"password-filename"
    ~short:'f'
    ~placeholder:"filename"
    ~doc:"path to the password filename"
    (string_parameter ())

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
  args13
    (base_dir_arg ())
    (config_file_arg ())
    (timings_switch ())
    (chain_arg ())
    (block_arg ())
    (wait_arg ())
    (protocol_arg ())
    (log_requests_switch ())
    (addr_arg ())
    (port_arg ())
    (tls_switch ())
    (remote_signer_arg ())
    (password_filename_arg ())


type parsed_config_args = {
  parsed_config_file : Cfg_file.t option ;
  parsed_args : cli_args option ;
  config_commands : Client_context.full command list ;
  base_dir : string option ;
  require_auth : bool ;
  password_filename : string option ;
}
let default_parsed_config_args = {
  parsed_config_file = None ;
  parsed_args = None ;
  config_commands = [] ;
  base_dir = None ;
  require_auth = false ;
  password_filename = None ;
}

let parse_config_args (ctx : #Client_context.full) argv =
  parse_global_options
    (global_options ())
    ctx
    argv >>=?
  fun ((base_dir,
        config_file,
        timings,
        chain,
        block,
        confirmations,
        protocol,
        log_requests,
        node_addr,
        node_port,
        tls,
        remote_signer,
        password_filename), remaining) ->
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
  Tezos_signer_backends.Remote.read_base_uri_from_env () >>=? fun remote_signer_env ->
  let remote_signer =
    Option.first_some remote_signer
      (Option.first_some remote_signer_env cfg.remote_signer) in
  let confirmations = Option.unopt ~default:cfg.confirmations confirmations in
  let cfg = { cfg with tls ; node_port ; node_addr ;
                       remote_signer ; confirmations ; password_filename } in
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
    ({ default_parsed_config_args with
       parsed_config_file = Some cfg ;
       parsed_args =
         Some { chain ; block ;
                confirmations ;
                print_timings = timings ;
                log_requests ;
                password_filename ;
                protocol } ;
       config_commands = commands config_file cfg
     },
     remaining)

type t =
  string option *
  string option *
  bool *
  Shell_services.chain *
  Shell_services.block *
  int option option *
  Protocol_hash.t option option *
  bool *
  string option *
  int option *
  bool *
  Uri.t option *
  string option

module type Remote_params =
sig
  val authenticate: Signature.public_key_hash list ->
    MBytes.t -> Signature.t tzresult Lwt.t
  val logger : RPC_client.logger
end

let other_registrations : (_ -> (module Remote_params) -> _) option  =
  Some (fun parsed_config_file (module Remote_params) ->
      Option.iter parsed_config_file.Cfg_file.remote_signer ~f:(fun signer ->
          Client_keys.register_signer
            (module Tezos_signer_backends.Remote.Make(struct
                 let default = signer
                 include Remote_params
               end))
        ))

let clic_commands ~base_dir:_ ~config_commands ~builtin_commands
    ~other_commands ~require_auth:_ =
  config_commands @ builtin_commands @ other_commands

let logger = None
