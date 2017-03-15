(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(* Tezos Command line interface - Configuration and Arguments Parsing *)

open Client_commands

module Cfg_file = struct
  open Data_encoding

  let encoding =
    conv
      (fun { incoming_addr ; incoming_port ; tls ; web_port } ->
        (Some incoming_addr, Some incoming_port, Some tls, Some web_port))
      (fun (incoming_addr, incoming_port, tls, web_port) ->
        let open Utils in
        let incoming_addr = unopt ~default:default_cfg.incoming_addr incoming_addr in
        let incoming_port = unopt ~default:default_cfg.incoming_port incoming_port in
        let tls = unopt ~default:default_cfg.tls tls in
        let web_port = unopt ~default:default_cfg.web_port web_port in
        { default_cfg with
          incoming_addr ; incoming_port ; tls ; web_port })
      (obj4
        (opt "incoming_addr" string)
        (opt "incoming_port" int16)
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
    for i = 0 to Array.length argv - 1 do
      if argv.(i) = name && i <> Array.length argv - 1 then
        raise (Found argv.(i+1))
    done ;
    None
  with Found s -> Some s

(* Entry point *)

let parse_args ?(extra = (fun _cfg -> [])) usage dispatcher argv cctxt =
  let open Lwt in
  (* Init config reference which will be updated as args are parsed *)
  let cfg = ref cctxt.Client_commands.config in
  let set_block x =
    match Node_rpc_services.Blocks.parse_block x with
    | Error _ -> raise (Arg.Bad "Can't parse -block")
    | Ok block -> cfg := { !cfg with block }
  in
  (* Command-line only args (not in config file) *)
  let cli_args = [
    "-base-dir", Arg.String (fun x -> cfg := { !cfg with base_dir = x }),
      "The directory where the Tezos client will store all its data.\n\
       default: " ^ Client_commands.(default_cfg.base_dir);
    "-config-file", Arg.String (fun x -> cfg := { !cfg with config_file = x }),
      "The main configuration file.\n\
       default: " ^ Client_commands.(default_cfg.config_file);
    "-timings", Arg.Bool (fun x -> cfg := { !cfg with print_timings = x }),
      "Show RPC request times.\n\
       default: " ^ string_of_bool Client_commands.(default_cfg.print_timings);
    "-force", Arg.Bool (fun x -> cfg := { !cfg with force = x }),
      "Show less courtesy than the average user.\n\
       default: " ^ string_of_bool Client_commands.(default_cfg.force);
    "-block", Arg.String set_block,
      "The block on which to apply contextual commands.\n\
       default: " ^ Node_rpc_services.Blocks.to_string Client_commands.(default_cfg.block);
  ] in
  (* Command-line args which can be set in config file as well *)
  let file_args = [
    (* Network options *)
    "-addr", Arg.String (fun x -> cfg := { !cfg with incoming_addr = x }),
      "The IP address at which the node's RPC server can be reached.\n\
       default: " ^ Client_commands.(default_cfg.incoming_addr);
    "-port", Arg.Int (fun x -> cfg := { !cfg with incoming_port = x }),
      "The TCP port at which the node's RPC server can be reached.\n\
       default: " ^ string_of_int Client_commands.(default_cfg.incoming_port);
    "-tls", Arg.Bool (fun x -> cfg := { !cfg with tls = x }),
      "Use TLS to connect to node.\n\
       default: " ^ string_of_bool Client_commands.(default_cfg.tls);
  ] in
  let all_args = cli_args @ file_args @ (extra cfg) in
  catch
    (fun () ->
       let args = ref all_args in
       let anon dispatch n = match dispatch (`Arg n) with
         | `Nop -> ()
         | `Args nargs -> args := nargs @ !args
         | `Fail exn -> raise exn
         | `Res _ -> assert false in
       Arg.parse_argv_dynamic
         ~current:(ref 0) argv args (anon (dispatcher ())) "\000" ;
       let dispatch = dispatcher () in
       (if Sys.file_exists !cfg.config_file then begin
          try
            (* Parse config file and init [cfg] with options defined in it *)
            let config_file = !cfg.config_file in
            Cfg_file.read config_file >>= begin function
              | Error _err ->
                  cctxt.Client_commands.error
                     "Error: can't parse the configuration file: %s\n%!"
                     config_file
              | Ok c ->
                  cfg := c ;
                  Lwt.return ()
            end >>= fun () ->
            (* parse once again to overwrite file options by cli ones *)
            Arg.parse_argv_dynamic
              ~current:(ref 0) argv args (anon dispatch) "\000" ;
            Lwt.return ()
          with Sys_error msg ->
            cctxt.Client_commands.error
              "Error: can't read the configuration file: %s\n%!" msg
        end else begin
          try
            (* parse once again with contextual options *)
            Arg.parse_argv_dynamic
              ~current:(ref 0) argv args (anon dispatch) "\000" ;
            Lwt_utils.create_dir (Filename.dirname !cfg.config_file)
            >>= fun () ->
            Cfg_file.write !cfg.config_file !cfg ;
            Lwt.return ()
          with Sys_error msg ->
            cctxt.Client_commands.warning
              "Warning: can't create the default configuration file: %s\n%!"
              msg
        end) >>= fun () ->
      begin match dispatch `End with
        | `Res res -> Lwt.return (res, !cfg)
        | `Fail exn -> fail exn
        | `Nop | `Args _ -> assert false
      end)
    (function
      | Arg.Bad msg ->
          (* FIXME: this is an ugly hack to circumvent [Arg]
             spuriously printing options at the end of the error
             message. *)
          let msg = List.hd (Utils.split '\000' msg) in
          Lwt.fail (Arg.Help (msg ^ usage all_args ^ "\n"))
      | Arg.Help _ ->
          Lwt.fail (Arg.Help (usage all_args ^ "\n"))
      | exn -> Lwt.fail exn)

let preparse_args argv cctxt : cfg Lwt.t =
  let cfg =
    match preparse "-base-dir" argv with
    | None -> default_cfg
    | Some base_dir -> default_cfg_of_base_dir base_dir
  in
  let cfg =
    match preparse "-config-file" argv with
    | None -> cfg
    | Some config_file -> { cfg with config_file }
  in
  let no_config () =
    cctxt.Client_commands.warning
       "Warning: config file not found\n%!" in
  let corrupted_config msg =
    cctxt.Client_commands.error
       "Error: can't parse the configuration file: %s\n%s\n%!"
       cfg.config_file msg in
  begin
    if Sys.file_exists cfg.config_file then try
      match
        Utils.read_file ~bin:false cfg.config_file
        |> Data_encoding_ezjsonm.from_string
      with
      | exception _ ->
        no_config () >>= fun () ->
        Lwt.return cfg
      | Error msg -> corrupted_config msg
      | Ok cfg_json ->
        try Lwt.return (Cfg_file.from_json cfg_json) with
        | Invalid_argument msg
        | Failure msg -> corrupted_config msg
    with Sys_error msg ->
      cctxt.Client_commands.error
        "Error: can't read the configuration file: %s\n%!" msg
    else Lwt.return cfg
  end >>= fun cfg ->
  let cfg =
    match preparse "-tls" argv with
    | None -> cfg
    | Some _ -> { cfg with tls = true }
  in
  let cfg =
    match preparse "-addr" argv with
    | None -> cfg
    | Some incoming_addr -> { cfg with incoming_addr }
  in
  begin
    match preparse "-port" argv with
    | None -> Lwt.return cfg
    | Some port ->
        try
          let incoming_port = int_of_string port in
          Lwt.return { cfg with incoming_port }
        with _ ->
          cctxt.Client_commands.error
            "Error: can't parse the -port option: %S.\n%!" port
  end >>= fun cfg ->
  match preparse "-block" Sys.argv with
  | None -> Lwt.return cfg
  | Some x ->
      match Node_rpc_services.Blocks.parse_block x with
      | Error _ ->
          cctxt.Client_commands.error
            "Error: can't parse the -block option: %S.\n%!" x
      | Ok block -> Lwt.return { cfg with block }
