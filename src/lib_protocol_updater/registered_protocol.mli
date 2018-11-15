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

module type T = sig
  module P : sig
    val hash: Protocol_hash.t
    include Tezos_protocol_environment_shell.PROTOCOL
  end
  include (module type of (struct include P end))
  module Block_services :
    (module type of (struct include Block_services.Make(P)(P) end))
  val complete_b58prefix : Context.t -> string -> string list Lwt.t
end

type t = (module T)

val mem: Protocol_hash.t -> bool

val list: unit -> t list

val get: Protocol_hash.t -> t option
val get_exn: Protocol_hash.t -> t

val list_embedded: unit -> Protocol_hash.t list

val get_embedded_sources: Protocol_hash.t -> Protocol.t option
val get_embedded_sources_exn: Protocol_hash.t -> Protocol.t

module Register_embedded
    (Env : Tezos_protocol_environment_shell.V1)
    (Proto : Env.Updater.PROTOCOL)
    (Source : sig
       val hash: Protocol_hash.t option
       val sources: Protocol.t
     end) :
  T with type P.block_header_data = Proto.block_header_data
     and type P.operation_data = Proto.operation_data
     and type P.operation_receipt = Proto.operation_receipt
     and type P.validation_state = Proto.validation_state
