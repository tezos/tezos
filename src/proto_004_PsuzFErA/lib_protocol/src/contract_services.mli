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

open Alpha_context

val list:
  'a #RPC_context.simple -> 'a -> Contract.t list shell_tzresult Lwt.t

type info = {
  manager: public_key_hash ;
  balance: Tez.t ;
  spendable: bool ;
  delegate: bool * public_key_hash option ;
  counter: counter ;
  script: Script.t option ;
}

val info_encoding: info Data_encoding.t

val info:
  'a #RPC_context.simple -> 'a -> Contract.t -> info shell_tzresult Lwt.t

val balance:
  'a #RPC_context.simple -> 'a -> Contract.t -> Tez.t shell_tzresult Lwt.t

val manager:
  'a #RPC_context.simple -> 'a -> Contract.t -> public_key_hash shell_tzresult Lwt.t

val manager_key:
  'a #RPC_context.simple -> 'a -> Contract.t -> (public_key_hash * public_key option) shell_tzresult Lwt.t

val delegate:
  'a #RPC_context.simple -> 'a -> Contract.t -> public_key_hash shell_tzresult Lwt.t

val delegate_opt:
  'a #RPC_context.simple -> 'a -> Contract.t -> public_key_hash option shell_tzresult Lwt.t

val is_delegatable:
  'a #RPC_context.simple -> 'a -> Contract.t -> bool shell_tzresult Lwt.t

val is_spendable:
  'a #RPC_context.simple -> 'a -> Contract.t -> bool shell_tzresult Lwt.t

val counter:
  'a #RPC_context.simple -> 'a -> Contract.t -> counter shell_tzresult Lwt.t

val script:
  'a #RPC_context.simple -> 'a -> Contract.t -> Script.t shell_tzresult Lwt.t

val script_opt:
  'a #RPC_context.simple -> 'a -> Contract.t -> Script.t option shell_tzresult Lwt.t

val storage:
  'a #RPC_context.simple -> 'a -> Contract.t -> Script.expr shell_tzresult Lwt.t

val storage_opt:
  'a #RPC_context.simple -> 'a -> Contract.t -> Script.expr option shell_tzresult Lwt.t

val big_map_get_opt:
  'a #RPC_context.simple -> 'a -> Contract.t -> Script.expr * Script.expr ->
  Script.expr option shell_tzresult Lwt.t


val register: unit -> unit
