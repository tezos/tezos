(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type 'a inner_stopper = {
  id: int ;
  push: ('a option -> unit) ;
  mutable active : bool;
  input : 'a input;
}

and 'a input =
  { mutable watchers : 'a inner_stopper list;
    mutable cpt : int; }

type stopper = unit -> unit

let create_input () =
  { watchers = [];
    cpt = 0 }

let create_fake_stream () =
  let str, push = Lwt_stream.create () in
  str, (fun () -> push None)

let notify input info =
  List.iter (fun w -> w.push (Some info)) input.watchers

let shutdown_output output =
  if output.active then begin
    output.active <- false;
    output.push None;
    output.input.watchers <-
      List.filter (fun w -> w.id <> output.id) output.input.watchers;
  end

let create_stream input =
  input.cpt <- input.cpt + 1;
  let id = input.cpt in
  let stream, push = Lwt_stream.create () in
  let output = { id; push; input; active = true } in
  input.watchers <- output :: input.watchers;
  stream, (fun () -> shutdown_output output)

let shutdown f = f ()
