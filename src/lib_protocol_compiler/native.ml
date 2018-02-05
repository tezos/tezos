(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
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

open Compiler

(* TODO: fail in the presence of "external" *)

module Backend = struct
  (* See backend_intf.mli. *)

  let symbol_for_global' = Compilenv.symbol_for_global'
  let closure_symbol = Compilenv.closure_symbol

  let really_import_approx = Import_approx.really_import_approx
  let import_symbol = Import_approx.import_symbol

  let size_int = Arch.size_int
  let big_endian = Arch.big_endian

  let max_sensible_number_of_arguments =
    (* The "-1" is to allow for a potential closure environment parameter. *)
    Proc.max_arguments_for_tailcalls - 1
end

let backend = (module Backend : Backend_intf.S)

(** Semi-generic compilation functions *)

let pack_objects output objects =
  let output = output ^ ".cmx" in
  Compmisc.init_path true;
  Asmpackager.package_files
    ~backend Format.err_formatter Env.initial_safe_string objects output ;
  Warnings.check_fatal () ;
  output

let link_shared output objects =
  Compenv.(readenv Format.err_formatter Before_link) ;
  Compmisc.init_path true;
  Asmlink.link_shared Format.err_formatter objects output ;
  Warnings.check_fatal ()

let compile_ml ?for_pack ml =
  let target = Filename.chop_extension ml in
  Clflags.for_package := for_pack ;
  Compenv.(readenv Format.err_formatter (Before_compile ml));
  Optcompile.implementation ~backend Format.err_formatter ml target ;
  Clflags.for_package := None ;
  target ^ ".cmx"

let () =
  Clflags.native_code := true

let driver = { compile_ml ; link_shared ; pack_objects }
