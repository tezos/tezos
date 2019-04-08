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
    (struct let name = "test-p2p-banned_ip" end)

let assert_equal ?(eq = (=)) ?prn ~msg a b =
  let msg = match prn with
    | None -> msg
    | Some prn ->
        Format.asprintf "@[<v 2>%s@,n(%a)@,<>@,(%a)@]" msg prn a prn b in
  if not (eq a b) then Alcotest.fail msg

let assert_equal_bool = assert_equal

let a = Ipaddr.V6.of_string_exn
let p = Ipaddr.V6.Prefix.of_string_exn

let timenow = Systime_os.now ()

let of_list l =
  List.fold_left (fun acc k ->
      P2p_acl.IpSet.add_prefix k timenow acc
    ) P2p_acl.IpSet.empty l

let test_empty _ =
  let addrs = List.map a [ "::" ; "ffff::" ; "a::2" ; ] in
  List.iter (fun addr ->
      assert_equal_bool ~msg:__LOC__ false (P2p_acl.IpSet.mem addr P2p_acl.IpSet.empty)
    ) addrs

let test_inclusion _ =
  let set = P2p_acl.IpSet.add_prefix (p "ffff::/16") timenow P2p_acl.IpSet.empty in
  let included = List.map a [ "ffff::3" ; "ffff:ffff::" ; "ffff:00::ff" ; ] in
  let not_included = List.map a [  "fffe::3" ; "ffee:ffff::" ; "::" ; ] in
  List.iter (fun addr ->
      assert_equal_bool ~msg:__LOC__ true (P2p_acl.IpSet.mem addr set)
    ) included ;
  List.iter (fun addr ->
      assert_equal_bool ~msg:__LOC__ false (P2p_acl.IpSet.mem addr set)
    ) not_included;

  let set = P2p_acl.IpSet.add_prefix (p "f000::/4") timenow P2p_acl.IpSet.empty in
  assert_equal ~msg:__LOC__ false (P2p_acl.IpSet.mem (a "e000::") set) ;

  (* Add one IP *)
  let set = P2p_acl.IpSet.add_prefix (p "::/128") timenow P2p_acl.IpSet.empty in
  assert_equal ~msg:__LOC__ false (P2p_acl.IpSet.mem (a "1::") set) ;

  let set = P2p_acl.IpSet.add_prefix (p "ffff:eeee::/32") timenow P2p_acl.IpSet.empty in
  assert_equal ~msg:__LOC__ false (P2p_acl.IpSet.mem (a "eeee:ffff::1") set) ;
  assert_equal ~msg:__LOC__ true (P2p_acl.IpSet.mem (a "ffff:eeee::1") set) ;

  let set = P2p_acl.IpSet.add_prefix (p "::/17") timenow P2p_acl.IpSet.empty in
  assert_equal ~msg:__LOC__ true (P2p_acl.IpSet.mem (a "0000:0000::") set) ;
  assert_equal ~msg:__LOC__ true (P2p_acl.IpSet.mem (a "0000:7000::") set) ;
  assert_equal ~msg:__LOC__ false (P2p_acl.IpSet.mem (a "0000:8000::1") set) ;

  let setlist = [p "e000::/4" ; p "a000::/4" ; p "ffff::/16"] in
  let set = of_list setlist in
  assert_equal ~msg:__LOC__ true (P2p_acl.IpSet.mem (a "ffff::1") set) ;
  assert_equal ~msg:__LOC__ true (P2p_acl.IpSet.mem (a "a111:8000::1") set) ;

  let set = of_list [p "e000::/4" ; p "a000::/4" ;
                     p "1234:5678::1/128"; p "ffff::/16"] in
  assert_equal ~msg:__LOC__ true (P2p_acl.IpSet.mem (a "1234:5678::1") set) ;
  assert_equal ~msg:__LOC__ true (P2p_acl.IpSet.mem (a "a111:8000::1") set) ;
  assert_equal ~msg:__LOC__ false (P2p_acl.IpSet.mem (a "b111:8000::1") set) ;
  assert_equal ~msg:__LOC__ false (P2p_acl.IpSet.mem (a "1234:5678::100") set)


let test_contiguous _ =
  let set = of_list [p "::/1" ; p "8000::/1"] in
  List.iter (fun addr ->
      assert_equal ~msg:__LOC__ true (P2p_acl.IpSet.mem addr set)
    ) [a "00::" ; a "01::" ; a "ff::" ]

module PSet = Set.Make(Ipaddr.V6.Prefix)

let test_fold _ =
  let addr_list = [p "::/1" ; p "8000::/1" ; p "ffff:ffff::/32" ; ] in
  let pset = PSet.of_list addr_list in
  let ipv6set =
    P2p_acl.IpSet.fold (fun prefix _value s ->
        PSet.add prefix s
      ) (of_list addr_list) PSet.empty ;
  in
  assert_equal ~eq:PSet.equal ~msg:__LOC__ ipv6set pset

let print_pset ppf pset =
  PSet.iter (fun p ->
      Format.fprintf ppf "%a " Ipaddr.V6.Prefix.pp p
    ) pset

let print_list ppf l =
  List.iter (fun p ->
      Format.fprintf ppf "%a " Ipaddr.V6.Prefix.pp p
    ) l

let test_to_list _ =
  let to_list s = P2p_acl.IpSet.fold (fun k _v acc -> k::acc) s [] in
  let list_eq = List.for_all2 (fun x y -> Ipaddr.V6.Prefix.compare x y = 0) in
  let assert_equal_set ~msg a b =
    let a = List.sort compare a in
    let b = List.sort compare (to_list b) in
    assert_equal ~prn:print_list ~eq:list_eq ~msg a b in

  let set = P2p_acl.IpSet.add_prefix (p "::/0") timenow P2p_acl.IpSet.empty in
  assert_equal ~eq:list_eq ~prn:print_list ~msg:__LOC__ [p "::/0"] (to_list set) ;

  let set = of_list [p "::/1" ; p "8000::/1"] in
  assert_equal ~eq:list_eq ~prn:print_list ~msg:__LOC__  [p "8000::/1"; p "::/1" ] (to_list set) ;

  let setlist = [p "1234:5678::/32"] in
  let set = of_list setlist in
  assert_equal_set ~msg:__LOC__ setlist set ;

  let setlist = [p "e000::/4" ; p "a000::/4" ;
                 p "ffff::/16" ;
                 p "1234:5678::/32" ;
                ] in
  let set = of_list setlist in
  assert_equal_set ~msg:__LOC__ setlist set

let () =
  Alcotest.run ~argv:[|""|] "tezos-p2p" [
    "p2p.ipv6set", [
      "empty", `Quick, test_empty ;
      "inclusion", `Quick, test_inclusion ;
      "contiguous", `Quick, test_contiguous ;
      "test_fold", `Quick, test_fold ;
      "to_list", `Quick, test_to_list ;
    ]
  ]
