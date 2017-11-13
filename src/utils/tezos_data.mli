(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Hash

module type DATA = sig

  type t

  val compare: t -> t -> int
  val equal: t -> t -> bool

  val (=): t -> t -> bool
  val (<>): t -> t -> bool
  val (<): t -> t -> bool
  val (<=): t -> t -> bool
  val (>=): t -> t -> bool
  val (>): t -> t -> bool
  val min: t -> t -> t
  val max: t -> t -> t

  val pp: Format.formatter -> t -> unit

  val encoding: t Data_encoding.t
  val to_bytes: t -> MBytes.t
  val of_bytes: MBytes.t -> t option

end

module Fitness : DATA with type t = MBytes.t list

module type HASHABLE_DATA = sig

  include DATA

  type hash
  val hash: t -> hash
  val hash_raw: MBytes.t -> hash

end

module Operation : sig

  type shell_header = {
    net_id: Net_id.t ;
    branch: Block_hash.t ;
  }
  val shell_header_encoding: shell_header Data_encoding.t

  type t = {
    shell: shell_header ;
    proto: MBytes.t ;
  }

  include HASHABLE_DATA with type t := t
                         and type hash := Operation_hash.t
  val of_bytes_exn: MBytes.t -> t

end

module Block_header : sig

  type shell_header = {
    net_id: Net_id.t ;
    level: Int32.t ;
    proto_level: int ; (* uint8 *)
    predecessor: Block_hash.t ;
    timestamp: Time.t ;
    validation_passes: int ; (* uint8 *)
    operations_hash: Operation_list_list_hash.t ;
    fitness: MBytes.t list ;
  }

  val shell_header_encoding: shell_header Data_encoding.t

  type t = {
    shell: shell_header ;
    proto: MBytes.t ;
  }

  include HASHABLE_DATA with type t := t
                         and type hash := Block_hash.t
  val of_bytes_exn: MBytes.t -> t

end

module Protocol : sig

  type t = {
    expected_env: env_version ;
    components: component list ;
  }

  and component = {
    name: string ;
    interface: string option ;
    implementation: string ;
  }

  and env_version = V1

  val component_encoding: component Data_encoding.t
  val env_version_encoding: env_version Data_encoding.t

  include HASHABLE_DATA with type t := t
                         and type hash := Protocol_hash.t
  val of_bytes_exn: MBytes.t -> t

end
