(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
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
    string -> t Lwt.t
  val to_source :
    Client_commands.context ->
    t -> string Lwt.t
  val name : string
end

module type Alias = sig
  type t
  val load :
    Client_commands.context ->
    (string * t) list Lwt.t
  val find :
    Client_commands.context ->
    string -> t Lwt.t
  val find_opt :
    Client_commands.context ->
    string -> t option Lwt.t
  val rev_find :
    Client_commands.context ->
    t -> string option Lwt.t
  val name :
    Client_commands.context ->
    t -> string Lwt.t
  val mem :
    Client_commands.context ->
    string -> bool Lwt.t
  val add :
    Client_commands.context ->
    string -> t -> unit Lwt.t
  val del :
    Client_commands.context ->
    string -> unit Lwt.t
  val save :
    Client_commands.context ->
    (string * t) list -> unit Lwt.t
  val to_source  :
    Client_commands.context ->
    t -> string Lwt.t
  val alias_param :
    ?name:string ->
    ?desc:string ->
    ('a, Client_commands.context, 'ret) Cli_entries.params ->
    (string * t -> 'a, Client_commands.context, 'ret) Cli_entries.params
  val fresh_alias_param :
    ?name:string ->
    ?desc:string ->
    ('a, Client_commands.context, 'ret) Cli_entries.params ->
    (string -> 'a, Client_commands.context, 'ret) Cli_entries.params
  val source_param :
    ?name:string ->
    ?desc:string ->
    ('a, Client_commands.context, 'ret) Cli_entries.params ->
    (t -> 'a, Client_commands.context, 'ret) Cli_entries.params
end
module Alias (Entity : Entity) : Alias with type t = Entity.t
