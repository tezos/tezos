(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

module Tag : sig

  type t = string list

  val add: t -> string -> t
  val remove: t -> string -> t
  val encoding: t Data_encoding.t

end

module type Entity = sig
  val name : string
end

module Tags (Entity : Entity) : sig

  include Client_aliases.Alias with type t = Tag.t

  val tag_param:
    ?name:string ->
    ?desc:string ->
    ('a, Client_commands.full_context, 'ret) Cli_entries.params ->
    (Tag.t -> 'a, Client_commands.full_context, 'ret) Cli_entries.params

  val rev_find_by_tag:
    Client_commands.full_context ->
    string ->
    string option tzresult Lwt.t

  val filter:
    Client_commands.full_context ->
    (string * t -> bool) ->
    (string * t) list tzresult Lwt.t

  val filter_by_tag:
    Client_commands.full_context ->
    string ->
    (string * t) list tzresult Lwt.t

end
