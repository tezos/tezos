(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Nomadic Development. <contact@tezcore.com>             *)
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

module type FILTER = sig
  type config
  val config_encoding : config Data_encoding.t
  val default_config : config
  module Proto : Registered_protocol.T
  val pre_filter : config -> Proto.operation_data -> bool
  val post_filter : config ->
    validation_state_before: Proto.validation_state ->
    validation_state_after: Proto.validation_state ->
    Proto.operation_data * Proto.operation_receipt -> bool Lwt.t
end

module No_filter (Proto : Registered_protocol.T) = struct
  type config = unit
  let config_encoding = Data_encoding.empty
  let default_config = ()
  module Proto = Proto
  let pre_filter _ _ = true
  let post_filter _ ~validation_state_before:_ ~validation_state_after:_ _ = Lwt.return_true
end

let table : (module FILTER) Protocol_hash.Table.t =
  Protocol_hash.Table.create 5

let register (module Filter : FILTER) =
  assert (not (Protocol_hash.Table.mem table Filter.Proto.hash)) ;
  Protocol_hash.Table.add table Filter.Proto.hash (module Filter)

let find = Protocol_hash.Table.find_opt table
