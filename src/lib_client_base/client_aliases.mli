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


module type Entity = sig
  type t
  val encoding : t Data_encoding.t
  val of_source : string -> t tzresult Lwt.t
  val to_source : t -> string tzresult Lwt.t
  val name : string
end

module type Alias = sig
  type t
  type fresh_param
  val load :
    #Client_context.wallet ->
    (string * t) list tzresult Lwt.t
  val set :
    #Client_context.wallet ->
    (string * t) list ->
    unit tzresult Lwt.t
  val find :
    #Client_context.wallet ->
    string -> t tzresult Lwt.t
  val find_opt :
    #Client_context.wallet ->
    string -> t option tzresult Lwt.t
  val rev_find :
    #Client_context.wallet ->
    t -> string option tzresult Lwt.t
  val name :
    #Client_context.wallet ->
    t -> string tzresult Lwt.t
  val mem :
    #Client_context.wallet ->
    string -> bool tzresult Lwt.t
  val add :
    force:bool ->
    #Client_context.wallet ->
    string -> t -> unit tzresult Lwt.t
  val del :
    #Client_context.wallet ->
    string -> unit tzresult Lwt.t
  val update :
    #Client_context.wallet ->
    string -> t -> unit tzresult Lwt.t
  val of_source : string -> t tzresult Lwt.t
  val to_source : t -> string tzresult Lwt.t
  val alias_parameter : unit -> (string * t, #Client_context.wallet) Clic.parameter
  val alias_param :
    ?name:string ->
    ?desc:string ->
    ('a, (#Client_context.wallet as 'b)) Clic.params ->
    (string * t -> 'a, 'b) Clic.params
  val fresh_alias_param :
    ?name:string ->
    ?desc:string ->
    ('a, (< .. > as 'obj)) Clic.params ->
    (fresh_param -> 'a, 'obj) Clic.params
  val force_switch :
    unit -> (bool, _) Clic.arg
  val of_fresh :
    #Client_context.wallet ->
    bool ->
    fresh_param ->
    string tzresult Lwt.t
  val source_param :
    ?name:string ->
    ?desc:string ->
    ('a, (#Client_context.wallet as 'obj)) Clic.params ->
    (t -> 'a, 'obj) Clic.params
  val source_arg :
    ?long:string ->
    ?placeholder:string ->
    ?doc:string ->
    unit -> (t option, (#Client_context.wallet as 'obj)) Clic.arg
  val autocomplete:
    #Client_context.wallet -> string list tzresult Lwt.t
end
module Alias (Entity : Entity) : Alias with type t = Entity.t
