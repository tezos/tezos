(* For this source file only.
 * Copyright (c) 2013-2014 Thomas Gazagnaire <thomas@gazagnaire.org>
 * Copyright (c) 2016 Dynamic Ledger Solutions, Inc. <contact@tezos.com>
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

open Error_monad

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
    failwith "%s is not a directory!" root
  else begin
    let mkdir dir =
      if not (Sys.file_exists dir) then mkdir dir in
    mkdir root;
    return ()
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
       Lwt_unix.(openfile file
                   [O_RDWR; O_NONBLOCK; O_CREAT] 0o644) >>= fun fd ->
       try
         write_bigstring fd ba >>= fun r ->
         Lwt_unix.close fd >>= fun () ->
         Lwt.return r
       with e ->
         Lwt_unix.close fd >>= fun () ->
         Lwt.fail e)

let is_directory f =
  try Sys.is_directory f with _ -> false

let is_empty dir =
  Lwt_unix.opendir dir >>= fun hdir ->
  Lwt_unix.readdir_n hdir 3 >>= fun files ->
  let res = Array.length files = 2 in
  Lwt_unix.closedir hdir >>= fun () ->
  Lwt.return res

let rec cleanup_dir dir =
  Lwt_unix.file_exists dir >>= function
  | true ->
      is_empty dir >>= fun empty ->
      if empty && dir <> "/" then begin
        Lwt_unix.rmdir dir >>= fun () ->
        cleanup_dir (Filename.dirname dir)
      end else
        Lwt.return_unit
  | false ->
      Lwt.return_unit

let remove_file ?(cleanup = false) file =
  Lwt_unix.file_exists file >>= function
  | true ->
      Lwt_unix.unlink file >>= fun () ->
      if cleanup then
        Lwt.catch
          (fun () -> cleanup_dir (Filename.dirname file))
          (fun _ -> Lwt.return_unit)
      else
        Lwt.return_unit
  | false ->
      Lwt.return_unit

let fold root ~init ~f =
  if is_directory root then begin
    let files = Lwt_unix.files_of_directory root in
    Lwt_stream.fold_s
      (fun file acc ->
         if file = "." || file = ".." then
           Lwt.return acc
         else
           f file acc)
      files init
  end else
    Lwt.return init

