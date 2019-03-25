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
  Internal_event.Legacy_logging.Make (struct
    let name = "test-p2p-banned_peers"
  end)

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
  List.iter (fun (_,addr) -> P2p_acl.IPGreylist.add set addr Ptime.epoch) peers;
  List.iter (fun (_,addr) ->
      assert_equal_bool ~msg:__LOC__ true (P2p_acl.banned_addr set addr)
    ) peers ;
  Lwt.return_unit
;;

let test_gc _ =
  let set = P2p_acl.create 10 in
  List.iter (fun (_,addr) -> P2p_acl.IPGreylist.add set addr Ptime.epoch) peers;
  List.iter (fun (_peer,addr) ->
      assert_equal_bool ~msg:__LOC__ true (P2p_acl.banned_addr set addr)
    ) peers ;
  (* remove all peers *)
  P2p_acl.IPGreylist.remove_old set ~older_than:Ptime.max ;
  List.iter (fun (_peer,addr) ->
      assert_equal_bool ~msg:__LOC__ false (P2p_acl.banned_addr set addr)
    ) peers ;
  Lwt.return_unit

let () =
  let init_logs = lazy (Internal_event_unix.init  ()) in
  let wrap (n, f) =
    Alcotest_lwt.test_case n `Quick begin fun _ () ->
      Lazy.force init_logs >>= fun () ->
      f () end in
  Alcotest.run ~argv:[|""|] "tezos-p2p" [
    "p2p.peerset",
    List.map wrap [
      "empty", test_empty ;
      "ban", test_ban;
      "gc", test_gc;
    ]
  ]
