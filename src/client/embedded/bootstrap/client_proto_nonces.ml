(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Cli_entries

(* TODO locking... *)

type t = (Block_hash.t * Nonce.t) list

let encoding : t Data_encoding.t =
  let open Data_encoding in
  list
    (obj2
       (req "block" Block_hash.encoding)
       (req "nonce" Nonce.encoding))

let filename () =
  Client_config.(base_dir#get // "nonces")

let load () =
  let filename = filename () in
  if not (Sys.file_exists filename) then
    Lwt.return []
  else
    Data_encoding_ezjsonm.read_file filename >>= function
    | None -> error "couldn't to read the nonces file"
    | Some json ->
        match Data_encoding.Json.destruct encoding json with
        | exception _ -> (* TODO print_error *)
            error "didn't understand the nonces file"
        | list ->
            Lwt.return list

let check_dir dirname =
  if not (Sys.file_exists dirname) then
    Lwt_utils.create_dir dirname
  else
    Lwt.return ()

let save list =
  Lwt.catch
    (fun () ->
       let dirname = Client_config.base_dir#get in
       check_dir dirname >>= fun () ->
       let filename = filename () in
       let json = Data_encoding.Json.construct encoding list in
       Data_encoding_ezjsonm.write_file filename json >>= function
       | false -> failwith "Json.write_file"
       | true -> return ())
    (fun exn ->
       error "could not write the nonces file: %s." (Printexc.to_string exn))

let mem block_hash =
  load () >|= fun data ->
  List.mem_assoc block_hash data

let find block_hash =
  load () >|= fun data ->
  try Some (List.assoc block_hash data)
  with Not_found -> None

let add block_hash nonce =
  load () >>= fun data ->
  save ((block_hash, nonce) ::
        List.remove_assoc block_hash data)

let del block_hash =
  load () >>= fun data ->
  save (List.remove_assoc block_hash data)

let dels hashes =
  load () >>= fun data ->
  save @@
  List.fold_left
    (fun data hash -> List.remove_assoc hash data)
    data hashes
