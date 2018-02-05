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

(** An OCaml source component of a protocol implementation. *)
and component = {
  (* The OCaml module name. *)
  name : string ;
  (* The OCaml interface source code *)
  interface : string option ;
  (* The OCaml source code *)
  implementation : string ;
}

and env_version = V1

val component_encoding: component Data_encoding.t
val env_version_encoding: env_version Data_encoding.t

include S.HASHABLE with type t := t
                    and type hash := Protocol_hash.t
