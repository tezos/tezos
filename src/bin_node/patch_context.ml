(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2018 Nomadic Labs. <nomadic@tezcore.com>                    *)
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

open Genesis_chain

let patch_context json ctxt =
  begin
    match json with
    | None -> Lwt.return ctxt
    | Some json ->
        Tezos_storage.Context.set ctxt
          ["sandbox_parameter"]
          (Data_encoding.Binary.to_bytes_exn Data_encoding.json json)
  end >>= fun ctxt ->
  let module Proto = (val Registered_protocol.get_exn genesis.protocol) in
  Proto.init ctxt {
    level = 0l ;
    proto_level = 0 ;
    predecessor = genesis.block ;
    timestamp = genesis.time ;
    validation_passes = 0 ;
    operations_hash = Operation_list_list_hash.empty ;
    fitness = [] ;
    context = Context_hash.zero ;
  } >>= function
  | Error _ -> assert false (* FIXME error *)
  | Ok { context = ctxt ; _ } ->
      Lwt.return ctxt
