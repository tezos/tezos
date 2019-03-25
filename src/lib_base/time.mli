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

module Protocol : sig

  (* The out-of-protocol view of in-protocol timestamps. The precision of
     in-protocol timestamps are only precise to the second.

     Note that the out-of-protocol view does not necessarily match the
     in-protocol representation.  *)

  type t
  val epoch : t
  include Compare.S with type t := t

  val add: t -> int64 -> t
  val diff: t -> t -> int64

  val of_notation : string -> t option
  val of_notation_exn : string -> t
  val to_notation : t -> string

  val of_seconds : int64 -> t
  val to_seconds : t -> int64

  val encoding : t Data_encoding.t
  val rfc_encoding : t Data_encoding.t
  val rpc_arg : t RPC_arg.t

  val pp_hum : Format.formatter -> t -> unit

end

module System : sig

  type t = Ptime.t
  val now : unit -> t

  module Span : sig
    type t = Ptime.Span.t
    val sleep : t -> unit Lwt.t
    val multiply_exn : float -> t -> t
    val of_seconds_exn : float -> t
    val rpc_arg : t RPC_arg.t
    val encoding : t Data_encoding.t
  end

  val of_protocol_opt: Protocol.t -> t option
  val of_protocol_exn: Protocol.t -> t
  val to_protocol: t -> Protocol.t (* Truncating *)

  val of_seconds_opt: int64 -> t option
  val of_seconds_exn: int64 -> t
  val to_seconds: t -> int64 (* Truncating *)

  val of_notation_opt : string -> t option
  val of_notation_exn : string -> t
  val to_notation : t -> string

  val encoding : t Data_encoding.t
  val rfc_encoding : t Data_encoding.t
  val rpc_arg : t RPC_arg.t
  val pp_hum : Format.formatter -> t -> unit

  type 'a stamped = {
    data: 'a ;
    stamp: t ;
  }
  val stamped_encoding : 'a Data_encoding.t -> 'a stamped Data_encoding.t
  val stamp : ?time:t -> 'a -> 'a stamped
  val recent : ('a * t) option -> ('a * t) option -> ('a * t) option

  val hash: t -> int
  include Compare.S with type t := t
  module Set : Set.S with type elt = t
  module Map : Map.S with type key = t
  module Table : Hashtbl.S with type key = t

end

