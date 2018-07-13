(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

open Lwt.Infix

let rec producer queue = function
  | 0 ->
      Format.eprintf "Done producing." ;
      Lwt.return_unit
  | n ->
      Lwt_pipe.push queue () >>= fun () ->
      producer queue (pred n)

let rec consumer queue = function
  | 0 ->
      Format.eprintf "Done consuming." ;
      Lwt.return_unit
  | n ->
      Lwt_pipe.pop queue >>= fun _ ->
      consumer queue (pred n)

let rec gen acc f = function
  | 0 -> acc
  | n -> gen (f () :: acc) f (pred n)

let run qsize nbp nbc p c =
  let q = Lwt_pipe.create ~size:(qsize, fun () -> qsize) () in
  let producers = gen [] (fun () -> producer q p) nbp in
  let consumers = gen [] (fun () -> consumer q c) nbc in
  Lwt.join producers <&> Lwt.join consumers

let main () =
  let qsize = ref 10 in
  let nb_producers = ref 10 in
  let nb_consumers = ref 10 in
  let produced_per_producer = ref 10 in
  let consumed_per_consumer = ref 10 in
  let spec = Arg.[
      "-qsize", Set_int qsize, "<int> Size of the pipe";
      "-nc", Set_int nb_consumers, "<int> Number of consumers";
      "-np", Set_int nb_producers, "<int> Number of producers";
      "-n", Set_int consumed_per_consumer, "<int> Number of consumed items per consumers";
      "-p", Set_int produced_per_producer, "<int> Number of produced items per producers";
      "-v", Unit (fun () -> Lwt_log_core.(add_rule "*" Info)), " Log up to info msgs";
      "-vv", Unit (fun () -> Lwt_log_core.(add_rule "*" Debug)), " Log up to debug msgs";
    ]
  in
  let anon_fun _ = () in
  let usage_msg = "Usage: %s <num_peers>.\nArguments are:" in
  Arg.parse spec anon_fun usage_msg;
  run !qsize !nb_producers
    !nb_consumers !produced_per_producer !consumed_per_consumer

let () = Lwt_main.run @@ main ()
