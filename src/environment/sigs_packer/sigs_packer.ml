(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

let dump_file oc file =
  let ic = open_in file in
  let buflen = 8096 in
  let buf = Bytes.create buflen in
  let rec loop () =
    let len = input ic buf 0 buflen in
    if len <> 0 then begin
      Printf.fprintf oc "%s" (Bytes.to_string (if len = buflen then buf else Bytes.sub buf 0 len)) ;
      loop ()
    end
  in
  loop () ;
  close_in ic

let opened_modules = [
  "Pervasives" ;
  "Error_monad" ;
  "Hash" ;
  "Tezos_data" ;
]

let include_mli oc file =
  let unit =
    String.capitalize_ascii
      (Filename.chop_extension (Filename.basename file)) in
  Printf.fprintf oc "module %s : sig\n" unit ;
  Printf.fprintf oc "# 1 %S\n" file ;
  dump_file oc file ;
  Printf.fprintf oc "end\n" ;
  if unit = "Result" then
    Printf.fprintf oc
      "type ('a, 'b) result = ('a, 'b) Result.result = \
      \ Ok of 'a | Error of 'b\n" ;
  if List.mem unit opened_modules then Printf.fprintf oc "open %s\n" unit

let () =
  Printf.fprintf stdout "module type T = sig\n" ;
  for i = 1 to Array.length Sys.argv - 1 do
    let file = Sys.argv.(i) in
    include_mli stdout file ;
  done ;
  Printf.fprintf stdout "end\n%!"
