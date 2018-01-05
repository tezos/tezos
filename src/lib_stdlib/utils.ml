(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

module Infix = struct

  let (<<) g f = fun a -> g (f a)

  let (--) i j =
    let rec loop acc j =
      if j < i then acc else loop (j :: acc) (pred j) in
    loop [] j

end

let display_paragraph ppf description =
  Format.fprintf ppf "@[%a@]"
    (Format.pp_print_list ~pp_sep:Format.pp_print_newline
       (fun ppf line ->
          Format.pp_print_list ~pp_sep:Format.pp_print_space
            (fun ppf w ->
               (* replace &nbsp; by real spaces... *)
               Format.fprintf ppf "%s@ "
                 (Stringext.replace_all ~pattern:"\xC2\xA0" ~with_:" " w))
            ppf
            (TzString.split ' ' line)))
    (TzString.split ~dup:false '\n' description)

let finalize f g = try let res = f () in g (); res with exn -> g (); raise exn

let read_file ?(bin=false) fn =
  let ic = (if bin then open_in_bin else open_in) fn in
  finalize (fun () ->
      let len = in_channel_length ic in
      really_input_string ic len)
    (fun () -> close_in ic)

let write_file ?(bin=false) fn contents =
  let oc = (if bin then open_out_bin else open_out) fn in
  finalize (fun () ->
      let contents = Bytes.unsafe_of_string contents in
      output oc contents 0 @@ Bytes.length contents
    )
    (fun () -> close_out oc)

let mkdir ?(perm=0o755) dir =
  let safe_mkdir dir =
    if not (Sys.file_exists dir) then
      try Unix.mkdir dir perm
      with Unix.Unix_error(Unix.EEXIST,_,_) -> () in
  let rec aux dir =
    if not (Sys.file_exists dir) then begin
      aux (Filename.dirname dir);
      safe_mkdir dir;
    end in
  aux dir
