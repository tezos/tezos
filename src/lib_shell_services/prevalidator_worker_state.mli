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

module Request : sig
  type 'a t =
    | Flush : Block_hash.t -> unit t
    | Notify : P2p_peer.Id.t * Mempool.t -> unit t
    | Inject : Operation.t -> unit t
    | Arrived : Operation_hash.t * Operation.t -> unit t
    | Advertise : unit t
  type view = View : _ t -> view
  val view : 'a t -> view
  val encoding : view Data_encoding.t
  val pp : Format.formatter -> view -> unit
end

module Event : sig
  type t =
    | Request of (Request.view * Worker_types.request_status * error list option)
    | Debug of string
  val level : t -> Internal_event.level
  val encoding : t Data_encoding.t
  val pp : Format.formatter -> t -> unit
end

module Worker_state : sig
  type view =
    { head : Block_hash.t ;
      timestamp : Time.System.t ;
      fetching : Operation_hash.Set.t ;
      pending : Operation_hash.Set.t ;
      applied : Operation_hash.t list ;
      delayed : Operation_hash.Set.t }
  val encoding : view Data_encoding.t
  val pp : Format.formatter -> view -> unit
end
