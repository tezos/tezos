(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(*
 * Copyright (c) 2013-2014 Thomas Gazagnaire <thomas@gazagnaire.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

let (>>=) = Lwt.(>>=)
let (>|=) = Lwt.(>|=)
let (//) = Filename.concat

exception Error of string

let error =
  Printf.ksprintf
    (fun str ->
       Printf.eprintf "fatal: %s\n%!" str;
       Lwt.fail (Error str))

let mkdir dir =
  let safe_mkdir dir =
    if not (Sys.file_exists dir) then
      try Unix.mkdir dir 0o755
      with Unix.Unix_error(Unix.EEXIST,_,_) -> () in
  let rec aux dir =
    if not (Sys.file_exists dir) then begin
      aux (Filename.dirname dir);
      safe_mkdir dir;
    end in
  aux dir

let check_dir root =
  if Sys.file_exists root && not (Sys.is_directory root) then
    error "%s is not a directory!" root
  else begin
    let mkdir dir =
      if not (Sys.file_exists dir) then mkdir dir in
    mkdir root;
    Lwt.return_unit
  end

let files = Lwt_pool.create 50 (fun () -> Lwt.return_unit)

let with_file fn =
  Lwt_pool.use files fn

let read_bigstring fd =
  Lwt_bytes.map_file ~fd ~shared:false ()

let with_file_in file fn =
  with_file
    (fun () ->
       let fd = Unix.(openfile file [O_RDONLY; O_NONBLOCK] 0o644) in
       try
         let b = read_bigstring fd in
         fn b >>= fun r ->
         Unix.close fd;
         Lwt.return r
       with e ->
         Unix.close fd;
         Lwt.fail e)

let write_bigstring fd ba =
  let rec rwrite fd buf ofs len =
    Lwt_bytes.write fd buf ofs len >>= fun n ->
    if n = 0 && len <> 0 then Lwt.fail End_of_file
    else if n < len then rwrite fd buf (ofs + n) (len - n)
    else Lwt.return_unit in
  rwrite fd ba 0 (Bigarray.Array1.dim ba)

let with_file_out file ba =
  mkdir (Filename.dirname file);
  with_file
    (fun () ->
       Lwt_unix.(openfile file [O_RDWR; O_NONBLOCK; O_CREAT] 0o644) >>= fun fd ->
       try
         write_bigstring fd ba >>= fun r ->
         Lwt_unix.close fd >>= fun () ->
         Lwt.return r
       with e ->
         Lwt_unix.close fd >>= fun () ->
         Lwt.fail e)

let remove_file file =
  if Sys.file_exists file then Unix.unlink file;
  Lwt.return_unit

let is_directory f =
  try Sys.is_directory f with _ -> false

let list_files root =
  let files = Lwt_unix.files_of_directory root in
  Lwt_stream.fold_s
    (fun file accu ->
       if file = "." || file = ".." then
         Lwt.return accu
       else
         Lwt.return (file :: accu))
    files [] >>= fun l ->
  Lwt.return (List.sort compare l)

let rec_files root =
  let rec aux accu dir =
    let files = Lwt_unix.files_of_directory (root // dir) in
    Lwt_stream.fold_s
      (fun file accu ->
         if file = "." || file = ".." then
           Lwt.return accu
         else
           let file = if dir = "" then file else dir // file in
           if is_directory (root // file) then
             aux accu file
           else
             Lwt.return (file :: accu))
      files accu in
  aux [] ""

let remove_rec root =
  let rec aux dir =
    let files = Lwt_unix.files_of_directory (root // dir) in
    Lwt_stream.iter_s
      (fun file ->
         if file = "." || file = ".." then
           Lwt.return_unit
         else
           let file = if dir = "" then file else dir // file in
           if is_directory (root // file) then begin
             aux file >>= fun () ->
             Lwt.return_unit
           end else begin
             Unix.unlink (root // file) ;
             Lwt.return_unit
           end)
      files >>= fun () ->
    Unix.rmdir (root // dir) ;
    Lwt.return_unit
  in
  if Sys.file_exists root then aux "" else Lwt.return_unit
