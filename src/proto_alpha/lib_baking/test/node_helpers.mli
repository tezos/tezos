(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

val fork_node:
  ?exe:string -> ?timeout:int -> ?port:int ->
  ?sandbox:Data_encoding.json ->
  unit -> int
(** [fork_node ()] forks a node in sandbox mode listening to rpc on
    `localhost:port` (where the default port is 18732) and returns the
    PID of the forked process. It waits `timeout` seconds (default 4)
    before to return and it may fails with an exception whenever the node
    died during the wait.  *)
