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

include
  Internal_event.Legacy_logging.Make
    (struct let name = "test-p2p-banned_peers" end)

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
  assert_equal_bool ~msg:__LOC__ false (P2p_acl.PeerRing.mem set (a "foo"));
  assert_equal_bool ~msg:__LOC__ true (P2p_acl.PeerRing.mem set (a "bar"));
  assert_equal_bool ~msg:__LOC__ true (P2p_acl.PeerRing.mem set (a "baz"))

let () =
  Alcotest.run ~argv:[|""|] "tezos-p2p" [
    "p2p.peerset", [
      "empty", `Quick, test_empty ;
      "add", `Quick, test_add;
      "overflow", `Quick, test_overflow;
      "remove", `Quick, test_remove;
    ]
  ]
