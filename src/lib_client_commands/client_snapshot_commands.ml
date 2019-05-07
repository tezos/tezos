(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2019 Nomadic Labs. <nomadic@tezcore.com>                    *)
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

let group =
  { Clic.name = "snapshot" ;
    title = "Commands for managing storage snapshots" }

let rolling_mode =
  Clic.switch
    ~long:"rolling"
    ~doc:("export a snapshot in rolling mode") ()

let exported_block =
  Clic.arg
    ~doc: "block to export"
    ~long: "block"
    ~placeholder: "<BLOCK_HASH>"
    (Clic.parameter (fun _ s -> return s))

let commands () =
  let open Clic in
  [
    command ~group ~desc: "Export a snapshot"
      (args2 rolling_mode exported_block)
      (prefixes [ "snapshot" ; "export" ]
       @@ stop)
      (fun (export_mode, block_hash) (ctxt : #Client_context.full) ->
         let export_mode =
           (function | true -> "rolling" | false -> "full") export_mode in
         (function
           | Some block_hash ->
               Shell_services.Chain.snapshot_export_block
                 ctxt
                 ~chain:ctxt#chain
                 ~export_mode
                 ~block_hash
           | None ->
               Shell_services.Chain.snapshot_export_last_checkpoint
                 ctxt
                 ~chain:ctxt#chain
                 ~export_mode
         ) block_hash  >>=? fun status ->
         ctxt#message "%s" status >>= return
      ) ;
  ]
