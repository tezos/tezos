(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
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
    ('a, 'ctx) Clic.params ->
    (Tag.t -> 'a, 'ctx) Clic.params

  val rev_find_by_tag:
    #Client_context.full ->
    string ->
    string option tzresult Lwt.t

  val filter:
    #Client_context.full ->
    (string * t -> bool) ->
    (string * t) list tzresult Lwt.t

  val filter_by_tag:
    #Client_context.full ->
    string ->
    (string * t) list tzresult Lwt.t

end
