(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

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
  let is_not_found : raw_context_result tzresult -> bool = function
    | Error [RPC_context.Not_found _] -> true
    | _ -> false
  in

  (* files and directories that are in context *)
  let version = Key (MBytes.of_hex (`Hex "616c706861")) in
  let dir_depth0 = Cut in
  let dir_depth2 = Dir [("02", Dir [("29", Cut)]);
                        ("a9", Dir [("ce", Cut)]);
                        ("c5", Dir [("5c", Cut)]);
                        ("da", Dir [("c9", Cut)]);
                        ("e7", Dir [("67", Cut)]);
                       ] in

  let tests = [(("version",1), is_equal version);
               (("",0), is_equal dir_depth0);
               (("delegates",2), is_equal dir_depth2);
               (("",-1), is_not_found);
               (("not-existent",1), is_not_found);
               (("not-existent",0), is_not_found);
               (("not-existent",-1), is_not_found);
              ] in
  iter_s (fun ((path,depth),predicate) ->
      Helpers.rpc_raw_context blkid [path] depth >>= fun result ->
      return (assert (predicate result))
    ) tests

let exe = try Sys.argv.(1) with _ -> "tezos-node"
let sandbox = try Sys.argv.(2) with _ -> "sandbox.json"
let rpc_port = try int_of_string Sys.argv.(3) with _ -> 18500

let main () =
  Helpers.init ~exe ~sandbox ~rpc_port () >>=? fun (_node_pid, genesis) ->
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
