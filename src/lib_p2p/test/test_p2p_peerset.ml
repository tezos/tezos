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

let a = fun s -> P2p_peer.Id.hash_string [s]

let test_empty _ =
  let peers = List.map a [ "foo"; "bar"; "baz" ; ] in
  let empty = P2p_acl.PeerRing.create 10 in
  List.iter (fun peer ->
      assert_equal_bool ~msg:__LOC__ false (P2p_acl.PeerRing.mem empty peer)
    ) peers

let test_add _ =
  let peers = List.map a [ "foo"; "bar"; "baz" ; ]  in
  let set = P2p_acl.PeerRing.create 10 in
  List.iter (fun peer -> P2p_acl.PeerRing.add set peer) peers;
  List.iter (fun peer ->
      assert_equal_bool ~msg:__LOC__ true (P2p_acl.PeerRing.mem set peer)
    ) peers

let test_remove _ =
  let peers = List.map a [ "foo"; "bar"; "baz" ; ]  in
  let set = P2p_acl.PeerRing.create 10 in
  List.iter (fun peer -> P2p_acl.PeerRing.add set peer) peers;
  assert_equal_bool ~msg:__LOC__ true (P2p_acl.PeerRing.mem set (a "bar"));
  P2p_acl.PeerRing.remove set (a "bar");
  assert_equal_bool ~msg:__LOC__ false (P2p_acl.PeerRing.mem set (a "bar"))

let test_overflow _ =
  let peers = List.map a [ "foo"; "bar"; "baz" ; ]  in
  let set = P2p_acl.PeerRing.create 3 in
  List.iter (fun peer -> P2p_acl.PeerRing.add set peer) peers;
  assert_equal_bool ~msg:__LOC__ true (P2p_acl.PeerRing.mem set (a "baz"));
  P2p_acl.PeerRing.add set (a "zor");
  assert_equal_bool ~msg:__LOC__ true (P2p_acl.PeerRing.mem set (a "zor"));
  assert_equal_bool ~msg:__LOC__ false (P2p_acl.PeerRing.mem set (a "baz"))

let () =
  Alcotest.run ~argv:[|""|] "tezos-p2p" [
    "p2p.peerset", [
      "empty", `Quick, test_empty ;
      "add", `Quick, test_add;
      "overflow", `Quick, test_overflow;
      "remove", `Quick, test_remove;
    ]
  ]
