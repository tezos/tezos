(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Logging.Db

type t = { root : string }

let init root =
  IO.check_dir root >>=? fun () ->
  return { root }

type key = string list
type value = MBytes.t

let file_of_key { root } key =
  String.concat Filename.dir_sep (root :: key)

let dir_of_key { root } key =
  let dir = List.rev @@ List.tl (List.rev key) in
  String.concat Filename.dir_sep (root :: dir)

let known s k =
  let file = file_of_key s k in
  Lwt.return (Sys.file_exists file && not (Sys.is_directory file))

let known_dir s k =
  let file = file_of_key s k in
  Lwt.return (Sys.file_exists file && Sys.is_directory file)

let read_opt s k =
  let file = file_of_key s k in
  if Sys.file_exists file && not (Sys.is_directory file) then
    Lwt.catch
      (fun () ->
         IO.with_file_in file
           (fun ba -> Lwt.return (Some ba)))
      (fun e ->
         warn "warn: can't read %s: %s"
           file (Printexc.to_string e);
         Lwt.return_none)
  else
    Lwt.return_none

type error += Unknown of string list

let () =
  Error_monad.register_error_kind
    `Permanent
    ~id:"store.unkown_key"
    ~title:"Unknown key in store"
    ~description: ""
    ~pp:(fun ppf key ->
        Format.fprintf ppf
          "@[<v 2>Unknown key %s@]"
          (String.concat "/" key))
    Data_encoding.(obj1 (req "key" (list string)))
    (function Unknown key -> Some key | _ -> None)
    (fun key -> Unknown key)

let read t key =
  read_opt t key >>= function
  | None -> fail (Unknown key)
  | Some v -> return v

let read_exn t key =
  read_opt t key >>= function
  | None -> Lwt.fail Not_found
  | Some v -> Lwt.return v

let remove s k =
  IO.remove_file ~cleanup:true (file_of_key s k)

let store s k v =
  let file = file_of_key s k in
  IO.remove_file ~cleanup:false file >>= fun () ->
  IO.with_file_out file v

let fold s k ~init ~f =
  let dir = file_of_key s k in
  IO.fold dir
    ~init
    ~f:(fun file acc ->
        if IO.is_directory (Filename.concat dir file) then
          f (`Dir (k @ [file])) acc
        else
          f (`Key (k @ [file])) acc)

let fold_keys s k ~init ~f =
  let rec loop k acc =
    fold s k ~init:acc
      ~f:(fun file acc ->
          match file with
          | `Key k -> f k acc
          | `Dir k -> loop k acc) in
  loop k init

let keys t = fold_keys t ~init:[] ~f:(fun k acc -> Lwt.return (k :: acc))

let remove_dir s k =
  let rec loop k =
    fold s k ~init:()
      ~f:(fun file () ->
          match file with
          | `Key k -> remove s k
          | `Dir k -> loop k) in
  loop  k
