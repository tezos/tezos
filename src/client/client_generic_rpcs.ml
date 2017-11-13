(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(* Tezos Command line interface - Generic JSON RPC interface *)

open Lwt.Infix
open Client_commands
open Cli_entries
open Json_schema

(*-- Assisted, schema directed input fill in --------------------------------*)

exception Unsupported_construct

type input = {
  int : int -> int -> string option -> string list -> int Lwt.t ;
  float : string option -> string list -> float Lwt.t ;
  string : string option -> string list -> string Lwt.t ;
  bool : string option -> string list -> bool Lwt.t ;
  continue : string option -> string list -> bool Lwt.t ;
  display : string -> unit Lwt.t ;
}

(* generic JSON generation from a schema with callback for random or
   interactive filling *)
let fill_in input schema =
  let rec element path { title ; kind }=
    match kind with
    | Integer { minimum ; maximum } ->
        let minimum =
          match minimum with
          | None -> min_int
          | Some (m, `Inclusive) -> int_of_float m
          | Some (m, `Exclusive) -> int_of_float m + 1 in
        let maximum =
          match maximum with
          | None -> max_int
          | Some (m, `Inclusive) -> int_of_float m
          | Some (m, `Exclusive) -> int_of_float m - 1 in
        input.int minimum maximum title path >>= fun i ->
        Lwt.return (`Float (float i))
    | Number _ ->
        input.float title path >>= fun f ->
        Lwt.return (`Float f)
    | Boolean ->
        input.bool title path >>= fun f ->
        Lwt.return (`Bool f)
    | String _ ->
        input.string title path >>= fun f ->
        Lwt.return (`String f)
    | Combine ((One_of | Any_of), elts) ->
        let nb = List.length elts in
        input.int 0 (nb - 1) (Some "Select the schema to follow") path >>= fun n ->
        element path (List.nth elts n)
    | Combine ((All_of | Not), _) ->  Lwt.fail Unsupported_construct
    | Def_ref name ->
        Lwt.return (`String (Json_query.json_pointer_of_path name))
    | Id_ref _ | Ext_ref _ ->
        Lwt.fail Unsupported_construct
    | Array (elts, _) ->
        let rec fill_loop acc n ls =
          match ls with
          | [] -> Lwt.return acc
          | elt :: elts ->
              element (string_of_int n :: path) elt >>= fun json ->
              fill_loop (json :: acc) (succ n) elts
        in
        fill_loop [] 0 elts >>= fun acc ->
        Lwt.return (`A (List.rev acc))
    | Object { properties } ->
        let rec fill_loop acc ls =
          match ls with
          | [] -> Lwt.return acc
          | (n, elt, _, _) :: elts ->
              element (n :: path) elt >>= fun json ->
              fill_loop ((n, json) :: acc) elts
        in
        fill_loop [] properties >>= fun acc ->
        Lwt.return (`O (List.rev acc))
    | Monomorphic_array (elt, specs) ->
        let rec fill_loop acc min n max =
          if n > max then
            Lwt.return acc
          else
            element (string_of_int n :: path) elt >>= fun json ->
            (if n < min then Lwt.return true else input.continue title path) >>= function
            | true -> fill_loop (json :: acc) min (succ n) max
            | false -> Lwt.return (json :: acc)
        in
        let max = match specs.max_items with None -> max_int | Some m -> m in
        fill_loop [] specs.min_items 0 max >>= fun acc ->
        Lwt.return (`A (List.rev acc))
    | Any -> Lwt.fail Unsupported_construct
    | Dummy -> Lwt.fail Unsupported_construct
    | Null -> Lwt.return `Null
  in
  element [] (Json_schema.root schema)

let random_fill_in schema =
  let display _ = Lwt.return () in
  let int min max _ _ =
    let max = Int64.of_int max
    and min = Int64.of_int min in
    let range = Int64.sub max min in
    let random_int64 = Int64.add (Random.int64 range) min in
    Lwt.return (Int64.to_int random_int64) in
  let string _title _ = Lwt.return "" in
  let float _ _ = Lwt.return (Random.float infinity) in
  let bool _ _ = Lwt.return (Random.int 2 = 0) in
  let continue _ _ = Lwt.return (Random.int 4 = 0) in
  Lwt.catch
    (fun () ->
       fill_in
         { int ; float ; string ; bool ; display ; continue }
         schema >>= fun json ->
       Lwt.return (Ok json))
    (fun e ->
       let msg = Printf.sprintf "Fill-in failed %s\n%!" (Printexc.to_string e) in
       Lwt.return (Error msg))

let editor_fill_in schema =
  let tmp = Filename.temp_file "tezos_rpc_call_" ".json" in
  let rec init () =
    (* write a temp file with instructions *)
    random_fill_in schema >>= function
    | Error msg -> Lwt.return (Error msg)
    | Ok json ->
        Lwt_io.(with_file ~mode:Output tmp (fun fp ->
            write_line fp (Data_encoding_ezjsonm.to_string json))) >>= fun () ->
        edit ()
  and edit () =
    (* launch the user's editor on it *)
    let editor_cmd =
      try let ed = Sys.getenv "EDITOR" in Lwt_process.shell (ed ^ " " ^ tmp)
      with Not_found ->
      try let ed = Sys.getenv "VISUAL" in Lwt_process.shell (ed ^ " " ^ tmp)
      with Not_found ->
        if Sys.win32 then
          (* TODO: I have no idea what I'm doing here *)
          ("", [| "notepad.exe" ; tmp |])
        else
          (* TODO: vi on MacOSX ? *)
          ("", [| "nano" ; tmp |])
    in
    (Lwt_process.open_process_none editor_cmd) # status >>= function
    | Unix.WEXITED 0 ->
        reread () >>= fun json ->
        delete () >>= fun () ->
        Lwt.return json
    | Unix.WSIGNALED x | Unix.WSTOPPED x | Unix.WEXITED x ->
        let msg = Printf.sprintf "FAILED %d \n%!" x in
        delete () >>= fun () ->
        Lwt.return (Error msg)
  and reread () =
    (* finally reread the file *)
    Lwt_io.(with_file ~mode:Input tmp (fun fp -> read fp)) >>= fun text ->
    match Data_encoding_ezjsonm.from_string text with
    | Ok r -> Lwt.return (Ok r)
    | Error msg -> Lwt.return (Error (Printf.sprintf "bad input: %s" msg))
  and delete () =
    (* and delete the temp file *)
    Lwt_unix.unlink tmp
  in
  init ()

(*-- Nice list display ------------------------------------------------------*)

module StringMap = Map.Make(String)

let rec count =
  let open RPC.Description in
  function
  | Dynamic _ -> 1
  | Static { service ; subdirs } ->
      let service =
        match service with
        | None -> 0
        | Some _ -> 1 in
      let subdirs =
        match subdirs with
        | None -> 0
        | Some (Suffixes subdirs) ->
            StringMap.fold (fun _ t r -> r + count t) subdirs 0
        | Some (Arg (_, subdir)) -> count subdir in
      service + subdirs

(*-- Commands ---------------------------------------------------------------*)

let list url cctxt =
  let args = Utils.split '/' url in
  Client_node_rpcs.describe cctxt.rpc_config
    ~recurse:true args >>=? fun tree ->
  let open RPC.Description in
  let collected_args = ref [] in
  let collect arg =
    if not (arg.RPC.Arg.descr <> None && List.mem arg !collected_args) then
      collected_args := arg :: !collected_args in
  let display_paragraph ppf description =
    Format.fprintf ppf "@,    @[%a@]"
      (fun ppf words -> List.iter (Format.fprintf ppf "%s@ ") words)
      (Utils.split ' ' description)
  in
  let display_arg ppf arg =
    match arg.RPC.Arg.descr with
    | None -> Format.fprintf ppf "%s" arg.RPC.Arg.name
    | Some descr ->
        Format.fprintf ppf "<%s>%a" arg.RPC.Arg.name display_paragraph descr
  in
  let display_service ppf (_path, tpath, service) =
    Format.fprintf ppf "- /%s" (String.concat "/" tpath) ;
    match service.description with
    | None | Some "" -> ()
    | Some description -> display_paragraph ppf description
  in
  let rec display ppf (path, tpath, tree) =
    match tree with
    | Dynamic description -> begin
        Format.fprintf ppf "- /%s <dynamic>" (String.concat "/" tpath) ;
        match description with
        | None | Some "" -> ()
        | Some description -> display_paragraph ppf description
      end
    | Static { service = None ; subdirs = None } -> ()
    | Static { service = Some service ; subdirs = None } ->
        display_service ppf (path, tpath, service)
    | Static { service ; subdirs = Some (Suffixes subdirs) } -> begin
        match service, StringMap.bindings subdirs with
        | None, [] -> ()
        | None, [ n, solo ] ->
            display ppf (path @ [ n ], tpath @ [ n ], solo)
        | None, items when count tree >= 3 && path <> [] ->
            Format.fprintf ppf "@[<v 2>+ %s/@,%a@]"
              (String.concat "/" path) (display_list tpath) items
        | Some service, items when count tree >= 3 && path <> [] ->
            Format.fprintf ppf "@[<v 2>+ %s@,%a@,%a@]"
              (String.concat "/" path)
              display_service (path, tpath, service)
              (display_list tpath) items
        | None, (n, t) :: items ->
            Format.fprintf ppf "%a"
              display (path @ [ n ], tpath @ [ n ], t) ;
            List.iter
              (fun (n, t) ->
                 Format.fprintf ppf "@,%a"
                   display (path @ [ n ], tpath @ [ n ], t))
              items
        | Some service, items ->
            display_service ppf (path, tpath, service) ;
            List.iter
              (fun (n, t) ->
                 Format.fprintf ppf "@,%a"
                   display (path @ [ n ], tpath @ [ n ], t))
              items
      end
    | Static { service = None ; subdirs = Some (Arg (arg, solo)) } ->
        collect arg ;
        let name = Printf.sprintf "<%s>" arg.RPC.Arg.name in
        display ppf (path @ [ name ], tpath @ [ name ], solo)
    | Static { service = Some service ;
               subdirs = Some (Arg (arg, solo)) } ->
        collect arg ;
        display_service ppf (path, tpath, service) ;
        Format.fprintf ppf "@," ;
        let name = Printf.sprintf "<%s>" arg.RPC.Arg.name in
        display ppf (path @ [ name ], tpath @ [ name ], solo)
  and display_list tpath =
    Format.pp_print_list
      (fun ppf (n,t) -> display ppf ([ n ], tpath @ [ n ], t))
  in
  cctxt.message "@ @[<v 2>Available services:@ @ %a@]@."
    display (args, args, tree) >>= fun () ->
  if !collected_args <> [] then begin
    cctxt.message "@,@[<v 2>Dynamic parameter description:@ @ %a@]@."
      (Format.pp_print_list display_arg) !collected_args >>= fun () ->
    return ()
  end else return ()


let schema url cctxt =
  let args = Utils.split '/' url in
  let open RPC.Description in
  Client_node_rpcs.describe cctxt.rpc_config ~recurse:false args >>=? function
  | Static { service = Some { input ; output } } ->
      let json = `O [ "input", Json_schema.to_json input ;
                      "output", Json_schema.to_json output ] in
      cctxt.message "%a" Json_repr.(pp (module Ezjsonm)) json >>= fun () ->
      return ()
  | _ ->
      cctxt.message
        "No service found at this URL (but this is a valid prefix)\n%!" >>= fun () ->
      return ()

let format url cctxt =
  let args = Utils.split '/' url in
  let open RPC.Description in
  Client_node_rpcs.describe cctxt.rpc_config ~recurse:false args >>=? function
  | Static { service = Some { input ; output } } ->
      cctxt.message
        "@[<v 0>\
         @[<v 2>Input format:@,%a@]@,\
         @[<v 2>Output format:@,%a@]@,\
         @]"
        Json_schema.pp input
        Json_schema.pp output >>= fun () ->
      return ()
  | _ ->
      cctxt.message
        "No service found at this URL (but this is a valid prefix)\n%!" >>= fun () ->
      return ()

let fill_in schema =
  let open Json_schema in
  match (root schema).kind with
  | Null -> Lwt.return (Ok `Null)
  | Any | Object { properties = [] } -> Lwt.return (Ok (`O []))
  | _ -> editor_fill_in schema

let call url cctxt =
  let args = Utils.split '/' url in
  let open RPC.Description in
  Client_node_rpcs.describe cctxt.rpc_config ~recurse:false args >>=? function
  | Static { service = Some { input } } -> begin
      fill_in input >>= function
      | Error msg ->
          cctxt.error "%s" msg >>= fun () ->
          return ()
      | Ok json ->
          Client_rpcs.get_json cctxt.rpc_config `POST args json >>=? fun json ->
          cctxt.message "%a"
            Json_repr.(pp (module Ezjsonm)) json >>= fun () ->
          return ()
    end
  | _ ->
      cctxt.message
        "No service found at this URL (but this is a valid prefix)\n%!" >>= fun () ->
      return ()

let call_with_json url json (cctxt: Client_commands.context) =
  let args = Utils.split '/' url in
  match Data_encoding_ezjsonm.from_string json with
  | Error err ->
      cctxt.error
        "Failed to parse the provided json: %s\n%!"
        err
  | Ok json ->
      Client_rpcs.get_json cctxt.rpc_config `POST args json >>=? fun json ->
      cctxt.message "%a"
        Json_repr.(pp (module Ezjsonm)) json >>= fun () ->
      return ()

let group =
  { Cli_entries.name = "rpc" ;
    title = "Commands for the low level RPC layer" }

let commands = [

  command ~desc: "list all understood protocol versions"
    no_options
    (fixed [ "list" ; "versions" ])
    (fun () cctxt ->
       Lwt_list.iter_s
         (fun (ver, _) -> cctxt.Client_commands.message "%a" Protocol_hash.pp_short ver)
         (Client_commands.get_versions ()) >>= fun () ->
       return ()) ;

  command ~group ~desc: "list available RPCs (low level command for advanced users)"
    no_options
    (prefixes [ "rpc" ; "list" ] @@ stop)
    (fun () -> (list "/"));

  command ~group ~desc: "list available RPCs (low level command for advanced users)"
    no_options
    (prefixes [ "rpc" ; "list" ] @@ string ~name:"url" ~desc: "the RPC's prefix to be described" @@ stop)
    (fun () -> list) ;

  command ~group ~desc: "get the input and output JSON schemas of an RPC"
    no_options
    (prefixes [ "rpc" ; "schema" ] @@ string ~name: "url" ~desc: "the RPC's URL" @@ stop)
    (fun () -> schema) ;

  command ~group ~desc: "get the humanoid readable input and output formats of an RPC"
    no_options
    (prefixes [ "rpc" ; "format" ] @@ string ~name: "url" ~desc: "the RPC's URL" @@ stop)
    (fun () -> format) ;

  command ~group ~desc: "call an RPC (low level command for advanced users)"
    no_options
    (prefixes [ "rpc" ; "call" ] @@ string ~name: "url" ~desc: "the RPC's URL" @@ stop)
    (fun () -> call) ;

  command ~group ~desc: "call an RPC (low level command for advanced users)"
    no_options
    (prefixes [ "rpc" ; "call" ] @@ string ~name: "url" ~desc: "the RPC's URL"
     @@ prefix "with" @@ string ~name:"input" ~desc:"the JSON input to the RPC" @@ stop)
    (fun () -> call_with_json)

]
