(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

include Logging.Make (struct let name = "test-p2p-banned_peers" end)

let assert_equal_bool ~msg a b =
  if a <> b then Alcotest.fail msg

let a = fun (peer,addr) ->
  (P2p_peer.Id.hash_string [peer], Ipaddr.V6.of_string_exn addr)

let foo = a ("foo","ffff::3")
let bar = a ("bar","ffff:00::ff")
let baz = a ("baz","a::2")
let peers = [foo;bar;baz]

let test_empty _ =
  let empty = P2p_acl.create 10 in
  List.iter (fun (_peer,addr) ->
      assert_equal_bool ~msg:__LOC__ false (P2p_acl.banned_addr empty addr)
    ) peers ;
  Lwt.return_unit
;;

let test_ban _ =
  let set = P2p_acl.create 10 in
  List.iter (fun (_,addr) -> P2p_acl.IPGreylist.add set addr Time.epoch) peers;
  List.iter (fun (_,addr) ->
      assert_equal_bool ~msg:__LOC__ true (P2p_acl.banned_addr set addr)
    ) peers ;
  Lwt.return_unit
;;

let test_gc _ =
  let set = P2p_acl.create 10 in
  List.iter (fun (_,addr) -> P2p_acl.IPGreylist.add set addr Time.epoch) peers;
  List.iter (fun (_peer,addr) ->
      assert_equal_bool ~msg:__LOC__ true (P2p_acl.banned_addr set addr)
    ) peers ;
  (* remove all peers *)
  P2p_acl.IPGreylist.remove_old set ~older_than:Time.max_value ;
  List.iter (fun (_peer,addr) ->
      assert_equal_bool ~msg:__LOC__ false (P2p_acl.banned_addr set addr)
    ) peers ;
  Lwt.return_unit

let () =
  let wrap (n, f) =
    Alcotest_lwt.test_case n `Quick (fun _ () -> f ()) in
  Alcotest.run ~argv:[|""|] "tezos-p2p" [
    "p2p.peerset",
    List.map wrap [
      "empty", test_empty ;
      "ban", test_ban;
      "gc", test_gc;
    ]
  ]
