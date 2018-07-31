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

module Helpers = Proto_alpha_helpers
module Assert = Helpers.Assert

(* Test for the rpc call Block_services.raw_context
   A similar test is bin_client/test/test_basic.sh
*)
let run blkid =

  let open Block_services in
  let is_equal a = function
    | Ok b -> a = b
    | _ -> false
  in
  let is_not_found : raw_context tzresult -> bool = function
    | Error [RPC_context.Not_found _] -> true
    | _ -> false
  in

  (* files and directories that are in context *)
  let dir_depth0 = Cut in
  let dir_depth2 = Dir [("02", Dir [("29", Cut)]);
                        ("a9", Dir [("ce", Cut)]);
                        ("c5", Dir [("5c", Cut)]);
                        ("da", Dir [("c9", Cut)]);
                        ("e7", Dir [("67", Cut)]);
                       ] in

  let tests = [(([""],0), is_equal dir_depth0);
               ((["delegates";"ed25519"],2), is_equal dir_depth2);
               (* (([""],-1), is_not_found); *)
               ((["not-existent"],1), is_not_found);
               ((["not-existent"],0), is_not_found);
               (* ((["not-existent"],-1), is_not_found); *)
              ] in

  let success = ref true in
  iter_s (fun ((path,depth),predicate) ->
      Helpers.rpc_raw_context blkid path depth >>= fun result ->
      let res = predicate result in
      Format.eprintf "/%s (%d) -> %B@." (String.concat "/" path) depth res ;
      success := !success  && res ;
      return_unit
    ) tests >>=? fun () ->
  if !success then
    return_unit
  else
    failwith "Error!"

let exe = try Sys.argv.(1) with _ -> "tezos-node"
let rpc_port = try int_of_string Sys.argv.(2) with _ -> 18500

let main () =
  Helpers.init ~exe ~rpc_port () >>=? fun (_node_pid, genesis) ->
  run (`Hash (genesis, 0))

let tests = [
  "main", (fun _ -> main ()) ;
]

let wrap (n, f) =
  Alcotest_lwt.test_case n `Quick begin fun _ () ->
    f () >>= function
    | Ok () -> Lwt.return_unit
    | Error error ->
        Format.kasprintf Pervasives.failwith "%a" pp_print_error error
  end

let () =
  Alcotest.run ~argv:[|""|] "tezos-client-alpha" [
    "rpcs", List.map wrap tests
  ]
