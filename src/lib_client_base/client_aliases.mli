(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)


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
  val of_source  : string -> t tzresult Lwt.t
  val to_source  : t -> string tzresult Lwt.t
  val alias_param :
    ?name:string ->
    ?desc:string ->
    ('a, (#Client_context.wallet as 'b)) Cli_entries.params ->
    (string * t -> 'a, 'b) Cli_entries.params
  val fresh_alias_param :
    ?name:string ->
    ?desc:string ->
    ('a, (< .. > as 'obj)) Cli_entries.params ->
    (fresh_param -> 'a, 'obj) Cli_entries.params
  val force_switch :
    unit -> (bool, #Client_context.full) Cli_entries.arg
  val of_fresh :
    #Client_context.wallet ->
    bool ->
    fresh_param ->
    string tzresult Lwt.t
  val source_param :
    ?name:string ->
    ?desc:string ->
    ('a, (#Client_context.wallet as 'obj)) Cli_entries.params ->
    (t -> 'a, 'obj) Cli_entries.params
  val source_arg :
    ?long:string ->
    ?placeholder:string ->
    ?doc:string ->
    unit -> (t option, (#Client_context.wallet as 'obj)) Cli_entries.arg
  val autocomplete:
    #Client_context.wallet -> string list tzresult Lwt.t
end
module Alias (Entity : Entity) : Alias with type t = Entity.t
