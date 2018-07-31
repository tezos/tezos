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

let (//) = Filename.concat

let () =
  Random.self_init () ;
  let anonymous = ref [] in
  let args_spec = [ ] in
  let usage_msg =
    Printf.sprintf "Usage: %s [options] <srcdir>" Sys.argv.(0) in
  Arg.parse args_spec (fun s -> anonymous := s :: !anonymous) usage_msg ;
  let source_dir =
    match List.rev !anonymous with
    | [ source_dir ] when Filename.basename source_dir = "TEZOS_PROTOCOL"->
        Filename.dirname source_dir
    | [ source_dir ] -> source_dir
    | _ -> Arg.usage args_spec usage_msg ; Pervasives.exit 1 in
  let hash, protocol =
    match Lwt_main.run (Lwt_utils_unix.Protocol.read_dir source_dir) with
    | Ok (None, proto) ->
        (Protocol.hash proto, proto)
    | Ok (Some hash, proto) ->
        (hash, proto)
    | Error err ->
        Format.kasprintf Pervasives.failwith
          "Failed to read TEZOS_PROTOCOL: %a" pp_print_error err in
  (* Generate the 'functor' *)
  Packer.dump stdout hash
    (Array.map
       begin fun { Protocol.name ; _ }  ->
         let name_lowercase = String.uncapitalize_ascii name in
         source_dir // name_lowercase ^ ".ml"
       end
       (Array.of_list protocol.components))
