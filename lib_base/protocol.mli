(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
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

include S.HASHABLE with type t := t
                    and type hash := Protocol_hash.t
val of_bytes_exn: MBytes.t -> t

