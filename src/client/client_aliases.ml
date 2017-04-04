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
    string -> t Lwt.t
  val to_source :
    Client_commands.context ->
    t -> string Lwt.t
  val name : string
end

module type Alias = sig
  type t
  val load :
    Client_commands.context ->
    (string * t) list Lwt.t
  val find :
    Client_commands.context ->
    string -> t Lwt.t
  val find_opt :
    Client_commands.context ->
    string -> t option Lwt.t
  val rev_find :
    Client_commands.context ->
    t -> string option Lwt.t
  val name :
    Client_commands.context ->
    t -> string Lwt.t
  val mem :
    Client_commands.context ->
    string -> bool Lwt.t
  val add :
    Client_commands.context ->
    string -> t -> unit Lwt.t
  val del :
    Client_commands.context ->
    string -> unit Lwt.t
  val update :
    Client_commands.context ->
    string -> t -> unit Lwt.t
  val save :
    Client_commands.context ->
    (string * t) list -> unit Lwt.t
  val of_source :
    Client_commands.context ->
    string -> t Lwt.t
  val to_source :
    Client_commands.context ->
    t -> string Lwt.t
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
    if not (Sys.file_exists filename) then Lwt.return [] else
      Data_encoding_ezjsonm.read_file filename >>= function
      | Error _ ->
          cctxt.Client_commands.error
            "couldn't to read the %s alias file" Entity.name
      | Ok json ->
          match Data_encoding.Json.destruct encoding json with
          | exception _ -> (* TODO print_error *)
              cctxt.Client_commands.error
                "didn't understand the %s alias file" Entity.name
          | list ->
              Lwt.return list

  let find_opt cctxt name =
    load cctxt >>= fun list ->
    try Lwt.return (Some (List.assoc name list))
    with Not_found -> Lwt.return_none

  let find cctxt name =
    load cctxt >>= fun list ->
    try Lwt.return (List.assoc name list)
    with Not_found ->
      cctxt.Client_commands.error "no %s alias named %s" Entity.name name

  let rev_find cctxt v =
    load cctxt >>= fun list ->
    try Lwt.return (Some (List.find (fun (_, v') -> v = v') list |> fst))
    with Not_found -> Lwt.return_none

  let mem cctxt name =
    load cctxt >>= fun list ->
    try
      ignore (List.assoc name list) ;
      Lwt.return_true
    with
    | Not_found -> Lwt.return_false

  let save cctxt list =
    Lwt.catch
      (fun () ->
         let dirname = dirname cctxt in
         (if not (Sys.file_exists dirname) then Lwt_utils.create_dir dirname
          else Lwt.return ()) >>= fun () ->
         let filename = filename cctxt in
         let json = Data_encoding.Json.construct encoding list in
         Data_encoding_ezjsonm.write_file filename json >>= function
         | Error _ -> Lwt.fail (Failure "Json.write_file")
         | Ok () -> Lwt.return ())
      (fun exn ->
         cctxt.Client_commands.error
           "could not write the %s alias file: %s."
           Entity.name (Printexc.to_string exn))

  let add cctxt name value =
    let keep = ref false in
    load cctxt >>= fun list ->
    (if not cctxt.config.force then
       Lwt_list.iter_s (fun (n, v) ->
           if n = name && v = value then
             (keep := true ;
              cctxt.message
                "The %s alias %s already exists with the same value." Entity.name n)
           else if n = name && v <> value then
             cctxt.error
               "another %s is already aliased as %s, use -force true to update" Entity.name n
           else if n <> name && v = value then
             cctxt.error
               "this %s is already aliased as %s, use -force true to insert duplicate" Entity.name n
           else Lwt.return ())
         list else Lwt.return ()) >>= fun () ->
    let list = List.filter (fun (n, _) -> n <> name) list in
    let list = (name, value) :: list in
    if !keep then
      Lwt.return ()
    else
      save cctxt list >>= fun () ->
      cctxt.Client_commands.message
        "New %s alias '%s' saved." Entity.name name

  let del cctxt name =
    load cctxt >>= fun list ->
    let list = List.filter (fun (n, _) -> n <> name) list in
    save cctxt list

  let update cctxt name value =
    load cctxt >>= fun list ->
    let list =
      List.map
        (fun (n, v) -> (n, if n = name then value else v))
        list in
    save cctxt list

  let save cctxt list =
    save cctxt  list >>= fun () ->
    cctxt.Client_commands.message
      "Successful update of the %s alias file." Entity.name

  include Entity

  let alias_param ?(name = "name") ?(desc = "existing " ^ Entity.name ^ " alias") next =
    param ~name ~desc
      (fun cctxt s -> find cctxt s >>= fun v -> Lwt.return (s, v))
      next

  let fresh_alias_param ?(name = "new") ?(desc = "new " ^ Entity.name ^ " alias") next =
    param ~name ~desc
      (fun cctxt s ->
         load cctxt >>= fun list ->
         if not cctxt.config.force then
           Lwt_list.iter_s (fun (n, _v) ->
               if n = s then
                 cctxt.Client_commands.error
                   "the %s alias %s already exists, use -force true to update" Entity.name n
               else Lwt.return ())
             list >>= fun () ->
           Lwt.return s
         else Lwt.return s)
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
             (fun () -> Lwt_io.(with_file ~mode:Input path read))
             (fun exn -> Lwt.fail_with @@ Format.asprintf "cannot read file (%s)" (Printexc.to_string exn))
           >>= of_source cctxt in
         match Utils.split ~limit:1 ':' s with
         | [ "alias" ; alias ]->
             find cctxt alias
         | [ "text" ; text ] ->
             of_source cctxt text
         | [ "file" ; path ] ->
             read path
         | _ ->
             Lwt.catch
               (fun () -> find cctxt s)
               (fun _ ->
                  Lwt.catch
                    (fun () -> read s)
                    (fun _ -> of_source cctxt s)))
      next

   let name cctxt d =
     rev_find cctxt d >>= function
     | None -> Entity.to_source cctxt d
     | Some name -> Lwt.return name

end
