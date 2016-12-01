(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(** A global thread that resumes the first time {!exit} is called
    anywhere in the program. Called by the main to wait for any other
    thread in the system to call {!exit}. *)
val termination_thread: int Lwt.t

(** Awakens the {!termination_thread} with the given return value, and
    raises an exception that cannot be caught, except by a
    catch-all. Should only be called once. *)
val exit: int -> 'a
