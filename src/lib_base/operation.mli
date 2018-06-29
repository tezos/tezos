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

type shell_header = {
  branch: Block_hash.t ;
}
val shell_header_encoding: shell_header Data_encoding.t

type t = {
  shell: shell_header ;
  proto: MBytes.t ;
}

include S.HASHABLE with type t := t
                    and type hash := Operation_hash.t
val of_bytes_exn: MBytes.t -> t

val bounded_encoding: ?max_size:int -> unit -> t Data_encoding.t
val bounded_list_encoding:
  ?max_length:int ->
  ?max_size:int ->
  ?max_operation_size:int ->
  ?max_pass:int ->
  unit -> (Operation_list_list_hash.path * t list) Data_encoding.t
val bounded_hash_list_encoding:
  ?max_length:int ->
  ?max_pass:int ->
  unit -> (Operation_list_list_hash.path * Operation_hash.t list) Data_encoding.t

