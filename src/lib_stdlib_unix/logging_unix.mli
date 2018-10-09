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

module Output : sig
  type t =
    | Null
    | Stdout
    | Stderr
    | File of string
    | Syslog of Lwt_log.syslog_facility

  val encoding : t Data_encoding.t
  val of_string : string -> t option
  val to_string : t -> string
  val pp : Format.formatter -> t -> unit
end

type cfg = {
  output : Output.t ;
  default_level : Logging.level ;
  rules : string option ;
  template : Logging.template ;
}

val default_cfg : cfg

val create_cfg :
  ?output:Output.t ->
  ?default_level:Logging.level ->
  ?rules:string ->
  ?template:Logging.template -> unit -> cfg

val level_encoding : Logging.level Data_encoding.t
val cfg_encoding : cfg Data_encoding.t

val init: ?cfg:cfg -> unit -> unit Lwt.t
val close: unit -> unit Lwt.t
