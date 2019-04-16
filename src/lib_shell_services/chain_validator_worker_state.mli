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
  type view = Block_hash.t
  val encoding : view Data_encoding.encoding
  val pp : Format.formatter -> view -> unit
end

module Event : sig
  type update =
    | Ignored_head
    | Branch_switch
    | Head_incrememt
  type t =
    | Processed_block of
        { request : Request.view ;
          request_status : Worker_types.request_status ;
          update : update ;
          fitness : Fitness.t }
    | Could_not_switch_testchain of error list
  val level : t -> Internal_event.level
  val encoding : t Data_encoding.encoding
  val pp : Format.formatter -> t -> unit
end

module Worker_state : sig
  type view =
    { active_peers : P2p_peer.Id.t list ;
      bootstrapped_peers : P2p_peer.Id.t list ;
      bootstrapped : bool }
  val encoding : view Data_encoding.encoding
  val pp : Format.formatter -> view -> unit
end
