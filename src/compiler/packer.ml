(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
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
      Printf.fprintf oc "%s"
        (if len = buflen then Bytes.unsafe_to_string buf else Bytes.sub_string buf 0 len) ;
      loop ()
    end
  in
  loop () ;
  close_in ic

let include_ml oc file =
  let unit =
    String.capitalize_ascii
      (Filename.chop_extension (Filename.basename file)) in
  (* FIXME insert .mli... *)
  Printf.fprintf oc "module %s " unit ;
  if Sys.file_exists (file ^ "i") then begin
    Printf.fprintf oc ": sig\n" ;
    Printf.fprintf oc "# 1 %S\n" (file ^ "i");
    dump_file oc (file ^ "i") ;
    Printf.fprintf oc "end " ;
  end ;
  Printf.fprintf oc "= struct\n" ;
  Printf.fprintf oc "# 1 %S\n" file ;
  dump_file oc file ;
  Printf.fprintf oc "end\n%!"

let opened_modules = [
  "Tezos_protocol_environment" ;
  "Pervasives" ;
  "Error_monad" ;
  "Hash" ;
  "Logging" ;
  "Tezos_data" ;
]

let dump oc files =
  Printf.fprintf oc
    "module Make (Tezos_protocol_environment : Tezos_protocol_environment_sigs_v1.T) = struct\n" ;
  Printf.fprintf oc "[@@@ocaml.warning \"-33\"]\n" ;
  List.iter (Printf.fprintf oc "open %s\n") opened_modules ;
  Printf.fprintf oc "[@@@ocaml.warning \"+33\"]\n" ;
  for i = 0 to Array.length files - 1 do
    include_ml oc files.(i) ;
  done ;
  Printf.fprintf oc "  include %s\n"
    (String.capitalize_ascii
       (Filename.basename
          (Filename.chop_extension files.(Array.length files - 1)))) ;
  Printf.fprintf oc "end\n%!"

let main () =
  dump stdout (Array.sub Sys.argv 1 (Array.length Sys.argv - 2))
