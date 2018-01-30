(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(** The OCaml compiler not being implemented with Lwt, the compilation
    take place in a separated process (by using [Lwt_process.exec]).

    The [main] function is the entry point for the forked process.
    While [Updater.compile] is the 'forking' function to be called by
    the [tezos-node] process.

*)

(** Semi-generic compilation functions *)

let pack_objects output objects =
  let output = output ^ ".cmo" in
  Compmisc.init_path true;
  Bytepackager.package_files
    Format.err_formatter Env.initial_safe_string objects output ;
  Warnings.check_fatal () ;
  output

let link_shared output objects =
  Compenv.(readenv Format.err_formatter Before_link) ;
  Compmisc.init_path true;
  Bytelink.link Format.err_formatter objects output ;
  Warnings.check_fatal ()

let compile_ml ?for_pack ml =
  let target = Filename.chop_extension ml in
  Clflags.for_package := for_pack ;
  Compenv.(readenv Format.err_formatter (Before_compile ml));
  Compile.implementation Format.err_formatter ml target ;
  Clflags.for_package := None ;
  target ^ ".cmo"

let () =
  Clflags.native_code := false

let driver = Compiler.{ compile_ml ; link_shared ; pack_objects }
