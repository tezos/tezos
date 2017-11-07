(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)


module type Entity = sig
  type t
  val encoding : t Data_encoding.t
  val of_source :
    Client_commands.context ->
    string -> t tzresult Lwt.t
  val to_source :
    Client_commands.context ->
    t -> string tzresult Lwt.t
  val name : string
end

module type Alias = sig
  type t
  type fresh_param
  val load :
    Client_commands.context ->
    (string * t) list tzresult Lwt.t
  val find :
    Client_commands.context ->
    string -> t tzresult Lwt.t
  val find_opt :
    Client_commands.context ->
    string -> t option tzresult Lwt.t
  val rev_find :
    Client_commands.context ->
    t -> string option tzresult Lwt.t
  val name :
    Client_commands.context ->
    t -> string tzresult Lwt.t
  val mem :
    Client_commands.context ->
    string -> bool tzresult Lwt.t
  val add :
    force:bool ->
    Client_commands.context ->
    string -> t -> unit tzresult Lwt.t
  val del :
    Client_commands.context ->
    string -> unit tzresult Lwt.t
  val update :
    Client_commands.context ->
    string -> t -> unit tzresult Lwt.t
  val save :
    Client_commands.context ->
    (string * t) list -> unit tzresult Lwt.t
  val of_source  :
    Client_commands.context ->
    string -> t tzresult Lwt.t
  val to_source  :
    Client_commands.context ->
    t -> string tzresult Lwt.t
  val alias_param :
    ?name:string ->
    ?desc:string ->
    ('a, Client_commands.context, 'ret) Cli_entries.params ->
    (string * t -> 'a, Client_commands.context, 'ret) Cli_entries.params
  val fresh_alias_param :
    ?name:string ->
    ?desc:string ->
    ('a, Client_commands.context, 'ret) Cli_entries.params ->
    (fresh_param -> 'a, Client_commands.context, 'ret) Cli_entries.params
  val of_fresh :
    Client_commands.context ->
    bool ->
    fresh_param ->
    string tzresult Lwt.t
  val source_param :
    ?name:string ->
    ?desc:string ->
    ('a, Client_commands.context, 'ret) Cli_entries.params ->
    (t -> 'a, Client_commands.context, 'ret) Cli_entries.params
  val autocomplete:
    Client_commands.context -> string list tzresult Lwt.t
end
module Alias (Entity : Entity) : Alias with type t = Entity.t
