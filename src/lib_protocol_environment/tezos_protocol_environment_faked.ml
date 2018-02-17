(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

module Context = struct
  type t

  type key = string list
  type value = MBytes.t
  let mem _ _ = assert false
  let dir_mem _ _ = assert false
  let get _ _ = assert false
  let set _ _ _ = assert false
  let del _ _ = assert false
  let remove_rec _ _ = assert false
  let fold _ _ ~init:_ ~f:_ = assert false
  let keys _ _ = assert false
  let fold_keys _ _ ~init:_ ~f:_ = assert false

  let set_protocol _ _ = assert false
  let fork_test_chain _ ~protocol:_ ~expiration:_ = assert false

end

include Tezos_protocol_environment.Make(Context)
