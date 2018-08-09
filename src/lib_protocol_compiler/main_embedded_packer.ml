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

let srcdir = Sys.argv.(1)
let version = Sys.argv.(2)

let srcdir =
  if Filename.basename srcdir = "TEZOS_PROTOCOL" then
    Filename.dirname srcdir
  else
    srcdir

let hash, sources =
  match Lwt_main.run (Lwt_utils_unix.Protocol.read_dir srcdir) with
  | Ok (None, proto) ->
      (Protocol.hash proto, proto)
  | Ok (Some hash, proto) ->
      (hash, proto)
  | Error err ->
      Format.kasprintf Pervasives.failwith
        "Failed to read TEZOS_PROTOCOL: %a" pp_print_error err

let () =
  Format.printf {|
module Source = struct
  let hash =
    Some (Tezos_crypto.Protocol_hash.of_b58check_exn %S)
  let sources = Tezos_base.Protocol.%a
end
@.|}
    (Protocol_hash.to_b58check hash)
    Protocol.pp_ocaml sources

let () =
  Format.printf {|
module Registered =
  Tezos_protocol_updater.Registered_protocol.Register_embedded
    (Tezos_embedded_protocol_environment_%s.Environment)
    (Tezos_embedded_raw_protocol_%s.Main)
    (Source)
@.|}
    version version
