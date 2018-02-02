(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

val fork_node:
  ?timeout:int -> ?port:int -> ?sandbox:string -> unit -> int
(** [fork_node ()] forks a node in sandbox mode listening to rpc on
    `localhost:port` (where the default port is 18732) and returns the
    PID of the forked process. It waits `timeout` seconds (default 4)
    before to return and it may fails with an exception whenever the node
    died during the wait.  *)
