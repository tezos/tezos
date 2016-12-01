(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(* Tezos Command line interface - Configuration and Arguments Parsing *)

open Config_file

let (//) = Filename.concat

let home =
  try Sys.getenv "HOME"
  with Not_found -> "/root"

class string_option_cp ?group name ?short_name default help =
  object (self)
    inherit [string] option_cp
        string_wrappers ?group name ?short_name default help
    method get_spec =
      let set = function
        | ""
        | "none" -> self#set None | s -> self#set (Some s) in
      Arg.String set
  end

let file_group = new group

(* Command line options *)

let cli_group = new group

let base_dir =
  new filename_cp ~group:cli_group ["base-dir"] (home // ".tezos-client")
    "The directory where the Tezos client will store all its data."

let config_file =
  new filename_cp ~group:cli_group ["config-file"] (base_dir#get // "config")
    "The main configuration file."

let print_timings =
  new bool_cp ~group:cli_group ["timings"] false
    "Show RPC request times."

let force =
  new bool_cp ~group:cli_group ["force"] false
    "Show less courtesy than the average user."

let block =
  new string_cp ~group:cli_group ["block"] "prevalidation"
    "The block on which to apply contextual commands."

let block () =
  match Node_rpc_services.Blocks.parse_block block#get with
  | Ok s -> s
  | Error _ -> raise (Arg.Bad "Can't parse -block")

let () =
  let config_file_forced = ref false in
  let update_config _old_file _new_file = config_file_forced := true in
  let update_base_dir old_dir new_dir =
    if new_dir <> old_dir then
      if not !config_file_forced then begin
        config_file#set (new_dir // "config");
        config_file_forced := false
      end
  in
  config_file#add_hook update_config;
  base_dir#add_hook update_base_dir

(** Network options *)

let in_both_groups cp =
  file_group # add cp ; cli_group # add cp ; cp

let incoming_addr = in_both_groups @@
  new string_cp [ "addr" ] ~short_name:"A" "127.0.0.1"
    "The IP address at which the node's RPC server can be reached."

let incoming_port = in_both_groups @@
  new int_cp [ "port" ] ~short_name:"P" 8732
    "The TCP port at which the node's RPC server can be reached."

(* Version specific options *)

let contextual_options : (unit -> unit) ref Protocol_hash_table.t =
  Protocol_hash_table.create 7

let register_config_option version option =
  let callback () =
    file_group # add option ;
    cli_group # add option in
  try
    let cont = Protocol_hash_table.find contextual_options version in
    cont := fun () -> callback () ; !cont ()
  with Not_found ->
    Protocol_hash_table.add contextual_options version (ref callback)

(* Entry point *)

let parse_args ?version usage dispatcher =
  let open Lwt in
  try begin match version with
    | None -> ()
    | Some version ->
        try
          !(Protocol_hash_table.find contextual_options version) ()
        with Not_found -> () end ;
    let base_args = cli_group#command_line_args "-" in
    let args = ref base_args in
    let anon dispatch n = match dispatch (`Arg n) with
      | `Nop -> ()
      | `Args nargs -> args := nargs @ !args
      | `Fail exn -> raise exn
      | `Res _ -> assert false in
    Arg.parse_argv_dynamic
      ~current:(ref 0) Sys.argv args (anon (dispatcher ())) (usage base_args) ;
    let dispatch = dispatcher () in
    (if Sys.file_exists config_file#get then begin
        try
          file_group#read config_file#get ;
          (* parse once again to overwrite file options by cli ones *)
          Arg.parse_argv_dynamic
            ~current:(ref 0) Sys.argv args (anon dispatch) (usage base_args) ;
          Lwt.return ()
        with Sys_error msg ->
          Cli_entries.error
            "Error: can't read the configuration file: %s\n%!" msg
      end else begin
       try
         (* parse once again with contextual options *)
         Arg.parse_argv_dynamic
           ~current:(ref 0) Sys.argv args (anon dispatch) (usage base_args) ;
         Lwt_utils.create_dir (Filename.dirname config_file#get) >>= fun () ->
         file_group#write config_file#get ;
         Lwt.return ()
       with Sys_error msg ->
         Cli_entries.warning
           "Warning: can't create the default configuration file: %s\n%!" msg
     end) >>= fun () ->
    begin match dispatch `End with
      | `Res res ->
          res
      | `Fail exn -> fail exn
      | `Nop | `Args _ -> assert false
    end
  with exn -> Lwt.fail exn

exception Found of string
let preparse name argv =
  try
    for i = 0 to Array.length argv - 1 do
      if argv.(i) = name && i <> Array.length argv - 1 then
        raise (Found argv.(i+1))
    done ;
    None
  with Found s -> Some s

let preparse_args () : Node_rpc_services.Blocks.block Lwt.t =
  begin
    match preparse "-base-dir" Sys.argv with
    | None -> ()
    | Some dir -> base_dir#set dir
  end ;
  begin
    match preparse "-config-file" Sys.argv with
    | None -> config_file#set @@ base_dir#get // "config"
    | Some file -> config_file#set file
  end ;
  begin
    if Sys.file_exists config_file#get then try
      (file_group#read config_file#get ;
      Lwt.return ())
    with Sys_error msg ->
      Cli_entries.error
        "Error: can't read the configuration file: %s\n%!" msg
    else Lwt.return ()
  end >>= fun () ->
  begin
    match preparse "-addr" Sys.argv with
    | None -> ()
    | Some addr -> incoming_addr#set addr
  end ;
  begin
    match preparse "-port" Sys.argv with
    | None -> Lwt.return ()
    | Some port ->
        try
          incoming_port#set (int_of_string port) ;
          Lwt.return ()
        with _ ->
          Cli_entries.error
            "Error: can't parse the -port option: %S.\n%!" port
  end >>= fun () ->
  match preparse "-block" Sys.argv with
  | None -> Lwt.return `Prevalidation
  | Some x ->
      match Node_rpc_services.Blocks.parse_block x with
      | Error _ ->
          Cli_entries.error
            "Error: can't parse the -block option: %S.\n%!" x
      | Ok b -> Lwt.return b
