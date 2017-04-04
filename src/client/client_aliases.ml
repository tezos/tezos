(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(* Tezos Command line interface - Local Storage for Configuration *)

open Lwt.Infix
open Cli_entries

module type Entity = sig
  type t
  val encoding : t Data_encoding.t
  val of_source :
    Client_commands.context ->
    string -> t tzresult Lwt.t
  val to_source :
    Client_commands.context ->
    t -> string tzresult Lwt.t
  val name : string
end

module type Alias = sig
  type t
  val load :
    Client_commands.context ->
    (string * t) list tzresult Lwt.t
  val find :
    Client_commands.context ->
    string -> t tzresult Lwt.t
  val find_opt :
    Client_commands.context ->
    string -> t option tzresult Lwt.t
  val rev_find :
    Client_commands.context ->
    t -> string option tzresult Lwt.t
  val name :
    Client_commands.context ->
    t -> string tzresult Lwt.t
  val mem :
    Client_commands.context ->
    string -> bool tzresult Lwt.t
  val add :
    Client_commands.context ->
    string -> t -> unit tzresult Lwt.t
  val del :
    Client_commands.context ->
    string -> unit tzresult Lwt.t
  val update :
    Client_commands.context ->
    string -> t -> unit tzresult Lwt.t
  val save :
    Client_commands.context ->
    (string * t) list -> unit tzresult Lwt.t
  val of_source :
    Client_commands.context ->
    string -> t tzresult Lwt.t
  val to_source :
    Client_commands.context ->
    t -> string tzresult Lwt.t
  val alias_param :
    ?name:string ->
    ?desc:string ->
    ('a, Client_commands.context, 'ret) Cli_entries.params ->
    (string * t -> 'a, Client_commands.context, 'ret) Cli_entries.params
  val fresh_alias_param :
    ?name:string ->
    ?desc:string ->
    ('a, Client_commands.context, 'ret) Cli_entries.params ->
    (string -> 'a, Client_commands.context, 'ret) Cli_entries.params
  val source_param :
    ?name:string ->
    ?desc:string ->
    ('a, Client_commands.context, 'ret) Cli_entries.params ->
    (t -> 'a, Client_commands.context, 'ret) Cli_entries.params
end

module Alias = functor (Entity : Entity) -> struct

  open Client_commands

  let encoding =
    let open Data_encoding in
    list (obj2
            (req "name" string)
            (req "value" Entity.encoding))

  let dirname cctxt =
    cctxt.config.base_dir

  let filename cctxt =
    Filename.concat (dirname cctxt) (Entity.name ^ "s")

  let load cctxt =
    let filename = filename cctxt in
    if not (Sys.file_exists filename) then
      return []
    else
      Data_encoding_ezjsonm.read_file filename
      |> generic_trace
        "couldn't to read the %s alias file" Entity.name >>=? fun json ->
      match Data_encoding.Json.destruct encoding json with
      | exception _ -> (* TODO print_error *)
          failwith "didn't understand the %s alias file" Entity.name
      | list ->
          return list

  let find_opt cctxt name =
    load cctxt >>=? fun list ->
    try return (Some (List.assoc name list))
    with Not_found -> return None

  let find cctxt name =
    load cctxt >>=? fun list ->
    try return (List.assoc name list)
    with Not_found ->
      failwith "no %s alias named %s" Entity.name name

  let rev_find cctxt v =
    load cctxt >>=? fun list ->
    try return (Some (List.find (fun (_, v') -> v = v') list |> fst))
    with Not_found -> return None

  let mem cctxt name =
    load cctxt >>=? fun list ->
    try
      ignore (List.assoc name list) ;
      return true
    with
    | Not_found -> return false

  let save cctxt list =
    Lwt.catch
      (fun () ->
         let dirname = dirname cctxt in
         Lwt_utils.create_dir dirname >>= fun () ->
         let filename = filename cctxt in
         let json = Data_encoding.Json.construct encoding list in
         Data_encoding_ezjsonm.write_file filename json)
      (fun exn -> Lwt.return (error_exn exn))
    |> generic_trace "could not write the %s alias file." Entity.name

  let add cctxt name value =
    let keep = ref false in
    load cctxt >>=? fun list ->
    begin
      if cctxt.config.force then
        return ()
      else
        iter_s (fun (n, v) ->
            if n = name && v = value then begin
              keep := true ;
              cctxt.message
                "The %s alias %s already exists with the same value."
                Entity.name n >>= fun () ->
              return ()
            end else if n = name && v <> value then begin
              failwith
                "another %s is already aliased as %s, \
                 use -force true to update"
                Entity.name n
            end else if n <> name && v = value then begin
              failwith
                "this %s is already aliased as %s, \
                 use -force true to insert duplicate"
                Entity.name n
            end else begin
              return ()
            end)
          list
    end >>=? fun () ->
    let list = List.filter (fun (n, _) -> n <> name) list in
    let list = (name, value) :: list in
    if !keep then
      return ()
    else
      save cctxt list >>=? fun () ->
      cctxt.Client_commands.message
        "New %s alias '%s' saved." Entity.name name >>= fun () ->
      return ()

  let del cctxt name =
    load cctxt >>=? fun list ->
    let list = List.filter (fun (n, _) -> n <> name) list in
    save cctxt list

  let update cctxt name value =
    load cctxt >>=? fun list ->
    let list =
      List.map
        (fun (n, v) -> (n, if n = name then value else v))
        list in
    save cctxt list

  let save cctxt list =
    save cctxt list >>=? fun () ->
    cctxt.Client_commands.message
      "Successful update of the %s alias file." Entity.name >>= fun () ->
    return ()

  include Entity

  let alias_param
      ?(name = "name") ?(desc = "existing " ^ Entity.name ^ " alias") next =
    param ~name ~desc
      (fun cctxt s ->
         find cctxt s >>= function
         | Ok v -> Lwt.return (s, v)
         | Error err -> cctxt.error "%a" pp_print_error err)
      next

  let fresh_alias_param
      ?(name = "new") ?(desc = "new " ^ Entity.name ^ " alias") next =
    param ~name ~desc
      (fun cctxt s ->
         begin
           load cctxt >>=? fun list ->
           begin
             if cctxt.config.force then
               return ()
             else
               iter_s
                 (fun (n, _v) ->
                    if n = s then
                      failwith
                        "the %s alias %s already exists, use -force true to update"
                        Entity.name n
                    else
                      return ())
                 list
           end
         end >>= function
         | Ok () -> Lwt.return s
         | Error err -> cctxt.error "%a" pp_print_error err)
      next

  let source_param ?(name = "src") ?(desc = "source " ^ Entity.name) next =
    let desc =
      desc ^ "\n"
      ^ "can be an alias, file or literal (autodetected in this order)\n\
         use 'file:path', 'text:literal' or 'alias:name' to force" in
    param ~name ~desc
      (fun cctxt s ->
         let read path =
           Lwt.catch
             (fun () ->
                Lwt_io.(with_file ~mode:Input path read) >>= fun content ->
                return content)
             (fun exn ->
                failwith
                  "cannot read file (%s)" (Printexc.to_string exn))
           >>=? fun content ->
           of_source cctxt content in
         begin
           match Utils.split ~limit:1 ':' s with
           | [ "alias" ; alias ]->
               find cctxt alias
           | [ "text" ; text ] ->
               of_source cctxt text
           | [ "file" ; path ] ->
               read path
           | _ ->
               find cctxt s >>= function
               | Ok v -> return v
               | Error _ ->
                   read s >>= function
                   | Ok v -> return v
                   | Error _ -> of_source cctxt s
         end >>= function
         | Ok s -> Lwt.return s
         | Error err -> cctxt.error "%a" pp_print_error err)
      next

   let name cctxt d =
     rev_find cctxt d >>=? function
     | None -> Entity.to_source cctxt d
     | Some name -> return name

end
