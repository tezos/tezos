(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(* Tezos Command line interface - Configuration and Arguments Parsing *)

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
    node_addr = "127.0.0.1" ;
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
        let open Utils in
        let node_addr = unopt ~default:default.node_addr node_addr in
        let node_port = unopt ~default:default.node_port node_port in
        let tls = unopt ~default:default.tls tls in
        let web_port = unopt ~default:default.web_port web_port in
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

exception Found of string

let preparse name argv =
  try
    for i = 0 to Array.length argv - 2 do
      if argv.(i) = name then raise (Found argv.(i+1))
    done ;
    None
  with Found s -> Some s

let preparse_bool name argv =
  try
    for i = 0 to Array.length argv - 1 do
      if argv.(i) = name then raise (Found "")
    done ;
    false
  with Found _ -> true

let preparse_args argv =
  let base_dir =
    match preparse "-base-dir" argv with
    | None -> Client_commands.default_base_dir
    | Some base_dir -> base_dir in
  let config_file =
    match preparse "-config-file" argv with
    | None -> base_dir // "config"
    | Some config_file -> config_file in
  let config_dir = Filename.dirname config_file in
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
  let tls = cfg.tls || preparse_bool "-tls" argv in
  let node_addr =
    match preparse "-addr" argv with
    | None -> cfg.node_addr
    | Some node_addr -> node_addr in
  let node_port =
    match preparse "-port" argv with
    | None -> cfg.node_port
    | Some port ->
        try int_of_string port
        with _ ->
          Format.eprintf
            "Error: can't parse the -port option: %S.@." port ;
          exit 1 in
  let block =
    match preparse "-block" Sys.argv with
    | None -> Client_commands.default_cfg.block
    | Some block ->
        match Node_rpc_services.Blocks.parse_block block with
        | Error _ ->
            Format.eprintf
              "Error: can't parse the -block option: %S.@."
              block ;
            exit 1
        | Ok block -> block in
  let cfg = { cfg with tls ; node_port ; node_addr } in
  if Sys.file_exists base_dir && not (Sys.is_directory base_dir) then begin
    Format.eprintf "Error: %s is not a directory.@." base_dir ;
    exit 1 ;
  end ;
  IO.mkdir base_dir ;
  if Sys.file_exists config_dir && not (Sys.is_directory config_dir) then begin
    Format.eprintf "Error: %s is not a directory.@." config_dir ;
    exit 1 ;
  end ;
  IO.mkdir config_dir ;
  if not (Sys.file_exists config_file) then Cfg_file.write config_file cfg ;
  (cfg, block)

(* Entry point *)

type cli_args = {
  block: Node_rpc_services.Blocks.block ;
  print_timings: bool ;
  force: bool ;
}

let default_cli_args = {
  block = Client_commands.default_cfg.block ;
  print_timings = false ;
  force = false ;
}

exception Bad of Error_monad.error list

let parse_args usage dispatcher argv =
  (* Init config reference which will be updated as args are parsed *)
  let parsed_args = ref default_cli_args in
  (* Command-line only args (not in config file) *)
  let cli_args = [
    "-base-dir", Arg.String (fun _ -> ( (* preparsed *) )),
      "The directory where the Tezos client will store all its data.\n\
       default: " ^ Client_commands.default_base_dir ;
    "-config-file", Arg.String (fun _ -> ( (* preparsed *) )),
      "The main configuration file.\n\
       default: " ^ Client_commands.default_base_dir // "config" ;
    "-timings",
      Arg.Bool (fun x -> parsed_args := { !parsed_args with print_timings = x }),
      "Show RPC request times.\n\
       default: " ^ string_of_bool default_cli_args.print_timings ;
    "-force",
      Arg.Bool (fun x -> parsed_args := { !parsed_args with force = x }),
      "Show less courtesy than the average user.\n\
       default: " ^ string_of_bool default_cli_args.force ;
    "-block", Arg.String (fun _ -> ( (* preparsed *) )),
      "The block on which to apply contextual commands.\n\
       default: " ^ Node_rpc_services.Blocks.to_string default_cli_args.block ;
  ] in
  (* Command-line args which can be set in config file as well *)
  let file_args = [
    (* Network options *)
    "-addr", Arg.String (fun _ -> ( (* preparsed *) )),
      "The IP address at which the node's RPC server can be reached.\n\
       default: " ^ Cfg_file.default.node_addr ;
    "-port", Arg.Int (fun _ -> ( (* preparsed *) )),
      "The TCP port at which the node's RPC server can be reached.\n\
       default: " ^ string_of_int Cfg_file.default.node_port ;
    "-tls", Arg.Bool (fun _ -> ( (* preparsed *) )),
      "Use TLS to connect to node.\n\
       default: " ^ string_of_bool Cfg_file.default.tls ;
  ] in
  let all_args = cli_args @ file_args in
  try
    let args = ref all_args in
    let anon dispatch n = match dispatch (`Arg n) with
      | `Nop -> ()
      | `Args nargs -> args := nargs @ !args
      | `Fail err -> raise (Bad err)
      | `Res _ -> assert false in
    let dispatch = dispatcher () in
    Arg.parse_argv_dynamic
      ~current:(ref 0) argv args (anon dispatch) "\000" ;
    match dispatch `End with
    | `Res res -> return (res, !parsed_args)
    | `Fail err -> Lwt.return (Error err)
    | `Nop | `Args _ -> assert false
  with
  | Bad err -> Lwt.return (Error err)
  | Arg.Bad msg ->
      (* FIXME: this is an ugly hack to circumvent [Arg]
         spuriously printing options at the end of the error
         message. *)
      let msg = String.trim (List.hd (Utils.split '\000' msg)) in
      Error_monad.failwith "%s" msg
  | Arg.Help _ ->
      raise (Arg.Help (usage all_args ^ "\n"))
