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

open Proto_demo

let protocol =
  Protocol_hash.of_b58check_exn
    "ProtoDemoDemoDemoDemoDemoDemoDemoDemoDemoDemoD3c8k9"

let bake (cctxt : full) : unit tzresult Lwt.t =
  let protocol_data = MBytes.create 0 in
  Demo_block_services.Helpers.Preapply.block cctxt [] ~protocol_data >>=? fun (shell, _) ->
  let block : Block_header.t = { shell = shell; protocol_data } in
  Demo_block_services.Helpers.Forge.block_header cctxt block >>=? fun encoded_header ->
  Shell_services.Injection.block cctxt encoded_header [] >>=? fun block_hash ->
  cctxt#message "Injected block %a" Block_hash.pp_short block_hash >>= fun () ->
  return_unit

let demo (cctxt : full) : unit tzresult Lwt.t =
  let block = cctxt#block in
  let chain = `Main in
  let msg = "test" in
  cctxt#message "Calling the 'echo' RPC with value %s." msg >>= fun () ->
  Services.echo cctxt (chain, block) msg >>=? fun reply ->
  cctxt#message "Received value: %s" reply >>= fun () ->
  return_unit

let error (cctxt : full) : unit tzresult Lwt.t =
  let block = cctxt#block in
  let chain = `Main in
  Services.failing cctxt (chain, block) 42 >>=? fun () ->
  return_unit

let commands () : full Clic.command list =
  let open Clic in
  let group = {name = "demo" ; title = "Some demo command" } in
  [
    command ~group ~desc: "A demo command"
      no_options
      (fixed [ "demo" ])
      (fun () (cctxt : full) -> demo cctxt) ;
    command ~group ~desc: "A failing command"
      no_options
      (fixed [ "fail" ])
      (fun () (cctxt : full) -> error cctxt) ;
    command ~group ~desc: "Bake an empty block"
      no_options
      (fixed [ "bake" ])
      (fun () cctxt -> bake cctxt) ;
  ]

let () =
  Client_commands.register protocol @@ fun _network ->
  List.map (Clic.map_command (new wrap_full)) @@
  commands ()
