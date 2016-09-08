(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(* Tezos Command line interface - Local Storage for Configuration *)

open Lwt
open Cli_entries

module type Entity = sig
  type t
  val encoding : t Data_encoding.t
  val of_source : string -> t Lwt.t
  val to_source : t -> string Lwt.t
  val name : string
end

module type Alias = sig
  type t
  val load : unit -> (Lwt_io.file_name * t) list Lwt.t
  val find : Lwt_io.file_name -> t Lwt.t
  val find_opt : Lwt_io.file_name -> t option Lwt.t
  val rev_find : t -> Lwt_io.file_name option Lwt.t
  val name : t -> string Lwt.t
  val mem : Lwt_io.file_name -> bool Lwt.t
  val add : Lwt_io.file_name -> t -> unit Lwt.t
  val del : Lwt_io.file_name -> unit Lwt.t
  val save : (Lwt_io.file_name * t) list -> unit Lwt.t
  val to_source : t -> string Lwt.t
  val alias_param :
    ?n:string ->
    ?desc:string ->
    'a Cli_entries.params ->
    (Lwt_io.file_name * t -> 'a) Cli_entries.params
  val fresh_alias_param :
    ?n:string ->
    ?desc:string ->
    'a Cli_entries.params -> (string -> 'a) Cli_entries.params
  val source_param :
    ?n:string ->
    ?desc:string ->
    'a Cli_entries.params -> (t -> 'a) Cli_entries.params
end

module Alias = functor (Entity : Entity) -> struct

  let encoding =
    let open Data_encoding in
    list (obj2
            (req "name" string)
            (req "value" Entity.encoding))

  let filename () =
    Client_config.(base_dir#get // Entity.name ^ "s")

  let load () =
    let filename = filename () in
    if not (Sys.file_exists filename) then return [] else
      Data_encoding.Json.read_file filename >>= function
      | None ->
          error "couldn't to read the %s alias file" Entity.name
      | Some json ->
          match Data_encoding.Json.destruct encoding json with
          | exception _ -> (* TODO print_error *)
              error "didn't understand the %s alias file" Entity.name
          | list ->
              return list

  let find_opt name =
    load () >>= fun list ->
    try return (Some (List.assoc name list))
    with Not_found -> return None

  let find name =
    load () >>= fun list ->
    try return (List.assoc name list)
    with Not_found -> error "no %s alias named %s" Entity.name name

  let rev_find v =
    load () >>= fun list ->
    try return (Some (List.find (fun (_, v') -> v = v') list |> fst))
    with Not_found -> return None

  let mem name =
    load () >>= fun list ->
    try
      ignore (List.assoc name list) ;
      Lwt.return true
    with
    | Not_found -> Lwt.return false

  let save list =
    catch
      (fun () ->
         let dirname = Client_config.base_dir#get in
         (if not (Sys.file_exists dirname) then Utils.create_dir dirname
          else return ()) >>= fun () ->
         let filename = filename () in
         let json = Data_encoding.Json.construct encoding list in
         Data_encoding.Json.write_file filename json >>= function
         | false -> fail (Failure "Json.write_file")
         | true -> return ())
      (fun exn ->
         error "could not write the %s alias file: %s."
           Entity.name (Printexc.to_string exn))

  let add name value =
    let keep = ref false in
    load () >>= fun list ->
    (if not Client_config.force#get then
       Lwt_list.iter_s (fun (n, v) ->
           if n = name && v = value then
             (message "The %s alias %s already exists with the same value." Entity.name n ;
              keep := true ;
              return ())
           else if n = name && v <> value then
             error "another %s is already aliased as %s, use -force true to update" Entity.name n
           else if n <> name && v = value then
             error "this %s is already aliased as %s, use -force true to insert duplicate" Entity.name n
           else return ())
         list else return ()) >>= fun () ->
    let list = List.filter (fun (n, _) -> n <> name) list in
    let list = (name, value) :: list in
    if !keep then
      return ()
    else
      save list >>= fun () ->
      message "New %s alias '%s' saved." Entity.name name ;
      return ()

  let del name =
    load () >>= fun list ->
    let list = List.filter (fun (n, _) -> n <> name) list in
    save list

  let save list =
    save list >>= fun () ->
    message "Successful update of the %s alias file." Entity.name ;
    return ()

  include Entity

  let alias_param ?(n = "name") ?(desc = "existing " ^ name ^ " alias") next =
    Param (n, desc, (fun s -> find s >>= fun v -> return (s, v)), next)

  let fresh_alias_param ?(n = "new") ?(desc = "new " ^ name ^ " alias") next =
    Param (n,
           desc,
           (fun s ->
              load () >>= fun list ->
              if not Client_config.force#get then
                Lwt_list.iter_s (fun (n, _v) ->
                    if n = name then
                      error "the %s alias %s already exists, use -force true to update" Entity.name n
                    else return ())
                  list >>= fun () ->
                return s
              else return s),
           next)

  let source_param ?(n = "src") ?(desc = "source " ^ name) next =
    Param (n,
           desc ^ "\n"
           ^ "can be an alias, file or litteral (autodetected in this order)\n\
              use 'file:path', 'text:litteral' or 'alias:name' to force",
           (fun s ->
              let read path =
                catch
                  (fun () -> Lwt_io.(with_file ~mode:Input path read))
                  (fun exn -> param_error "cannot read file (%s)" (Printexc.to_string exn))
                >>= of_source in
              match Utils.split ~limit:1 ':' s with
              | [ "alias" ; alias ]->
                  find alias
              | [ "text" ; text ] ->
                  of_source text
              | [ "file" ; path ] ->
                  read path
              | _ ->
                  catch
                    (fun () -> find s)
                    (fun _ ->
                       catch
                         (fun () -> read s)
                         (fun _ -> of_source s))),
           next)

   let name d =
     rev_find d >>= function
     | None -> Entity.to_source d
     | Some name -> Lwt.return name

end
