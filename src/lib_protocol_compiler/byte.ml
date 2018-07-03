(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

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
