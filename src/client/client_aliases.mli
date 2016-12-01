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
  val of_source : string -> t Lwt.t
  val to_source : t -> string Lwt.t
  val name : string
end

module type Alias = sig
  type t
  val load : unit -> (Lwt_io.file_name * t) list Lwt.t
  val find : Lwt_io.file_name -> t Lwt.t
  val find_opt : Lwt_io.file_name -> t option Lwt.t
  val rev_find : t -> Lwt_io.file_name option Lwt.t
  val name : t -> string Lwt.t
  val mem : Lwt_io.file_name -> bool Lwt.t
  val add : Lwt_io.file_name -> t -> unit Lwt.t
  val del : Lwt_io.file_name -> unit Lwt.t
  val save : (Lwt_io.file_name * t) list -> unit Lwt.t
  val to_source : t -> string Lwt.t
  val alias_param :
    ?name:string ->
    ?desc:string ->
    'a Cli_entries.params ->
    (Lwt_io.file_name * t -> 'a) Cli_entries.params
  val fresh_alias_param :
    ?name:string ->
    ?desc:string ->
    'a Cli_entries.params -> (string -> 'a) Cli_entries.params
  val source_param :
    ?name:string ->
    ?desc:string ->
    'a Cli_entries.params -> (t -> 'a) Cli_entries.params
end
module Alias (Entity : Entity) : Alias with type t = Entity.t
