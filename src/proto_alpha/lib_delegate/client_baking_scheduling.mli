(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)


val sleep_until: Time.t -> unit Lwt.t option

val wait_for_first_event:
  name:string ->
  'event tzresult Lwt_stream.t ->
  'event Lwt.t

val main :
  name:string ->
  cctxt:(#Proto_alpha.full as 'a) ->
  stream:'event tzresult Lwt_stream.t ->
  state_maker:(Block_hash.t -> 'event -> 'state tzresult Lwt.t) ->
  pre_loop:('a -> 'state -> 'event -> unit tzresult Lwt.t) ->
  compute_timeout:('state -> 'timesup Lwt.t) ->
  timeout_k:('a -> 'state -> 'timesup -> unit tzresult Lwt.t) ->
  event_k:('a -> 'state -> 'event -> unit tzresult Lwt.t) ->
  unit tzresult Lwt.t

(** [main ~name ~cctxt ~stream ~state_maker ~pre_loop ~timeout_maker ~timeout_k
    ~event_k] is an infinitely running loop that
    monitors new events arriving on [stream]. The loop exits when the
    [stream] gives an error.

    The function [pre_loop] is called before the loop starts.

    The loop maintains a state (of type ['state]) initialized by [state_maker]
    and passed to the callbacks [timeout_maker] (used to set up waking-up
    timeouts), [timeout_k] (when a computed timeout happens), and [event_k]
    (when a new event arrives on the stream).
*)
