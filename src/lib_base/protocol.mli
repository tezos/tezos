(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

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

val pp_ocaml: Format.formatter -> t -> unit

include S.HASHABLE with type t := t
                    and type hash := Protocol_hash.t
val of_bytes_exn: MBytes.t -> t

module Meta: sig

  type t = {
    hash: Protocol_hash.t option ;
    expected_env_version: env_version option ;
    modules: string list ;
  }

  val encoding: t Data_encoding.t

end

val read_dir: string -> Protocol_hash.t * t
val write_dir: string -> ?hash:Protocol_hash.t -> t -> unit Lwt.t
