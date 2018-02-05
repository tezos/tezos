(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Mem_context

(** Context creation *)

let create_block2 ctxt =
  set ctxt ["a"; "b"] (MBytes.of_string "Novembre") >>= fun ctxt ->
  set ctxt ["a"; "c"] (MBytes.of_string "Juin") >>= fun ctxt ->
  set ctxt ["version";] (MBytes.of_string "0.0") >>= fun ctxt ->
  Lwt.return ctxt

let create_block3a ctxt =
  del ctxt ["a"; "b"] >>= fun ctxt ->
  set ctxt ["a"; "d"] (MBytes.of_string "Mars") >>= fun ctxt ->
  Lwt.return ctxt

let create_block3b ctxt =
  del ctxt ["a"; "c"] >>= fun ctxt ->
  set ctxt ["a"; "d"] (MBytes.of_string "Février") >>= fun ctxt ->
  Lwt.return ctxt

type t = {
  genesis: Mem_context.t ;
  block2: Mem_context.t ;
  block3a: Mem_context.t ;
  block3b: Mem_context.t ;
}

let wrap_context_init f _base_dir =
  let genesis = Mem_context.empty in
  create_block2 genesis >>= fun block2 ->
  create_block3a block2 >>= fun block3a ->
  create_block3b block2 >>= fun block3b ->
  f { genesis; block2 ; block3a; block3b } >>= fun result ->
  return result

(** Simple test *)

let c = function
  | None -> None
  | Some s -> Some (MBytes.to_string s)

let test_simple { block2 = ctxt } =
  get ctxt ["version"] >>= fun version ->
  Assert.equal_string_option ~msg:__LOC__ (c version) (Some "0.0") ;
  get ctxt ["a";"b"] >>= fun novembre ->
  Assert.equal_string_option (Some "Novembre") (c novembre) ;
  get ctxt ["a";"c"] >>= fun juin ->
  Assert.equal_string_option ~msg:__LOC__ (Some "Juin") (c juin) ;
  Lwt.return ()

let test_continuation { block3a = ctxt } =
  get ctxt ["version"] >>= fun version ->
  Assert.equal_string_option ~msg:__LOC__ (Some "0.0") (c version) ;
  get ctxt ["a";"b"] >>= fun novembre ->
  Assert.is_none ~msg:__LOC__ (c novembre) ;
  get ctxt ["a";"c"] >>= fun juin ->
  Assert.equal_string_option ~msg:__LOC__ (Some "Juin") (c juin) ;
  get ctxt ["a";"d"] >>= fun mars ->
  Assert.equal_string_option ~msg:__LOC__  (Some "Mars") (c mars) ;
  Lwt.return ()

let test_fork { block3b = ctxt } =
  get ctxt ["version"] >>= fun version ->
  Assert.equal_string_option ~msg:__LOC__ (Some "0.0") (c version) ;
  get ctxt ["a";"b"] >>= fun novembre ->
  Assert.equal_string_option ~msg:__LOC__ (Some "Novembre") (c novembre) ;
  get ctxt ["a";"c"] >>= fun juin ->
  Assert.is_none ~msg:__LOC__ (c juin) ;
  get ctxt ["a";"d"] >>= fun mars ->
  Assert.equal_string_option ~msg:__LOC__ (Some "Février") (c mars) ;
  Lwt.return ()

let test_replay { genesis = ctxt0 }  =
  set ctxt0 ["version"] (MBytes.of_string "0.0") >>= fun ctxt1 ->
  set ctxt1 ["a"; "b"] (MBytes.of_string "Novembre") >>= fun ctxt2 ->
  set ctxt2 ["a"; "c"] (MBytes.of_string "Juin") >>= fun ctxt3 ->
  set ctxt3 ["a"; "d"] (MBytes.of_string "July") >>= fun ctxt4a ->
  set ctxt3 ["a"; "d"] (MBytes.of_string "Juillet") >>= fun ctxt4b ->
  set ctxt4a ["a"; "b"] (MBytes.of_string "November") >>= fun ctxt5a ->
  get ctxt4a ["a";"b"] >>= fun novembre ->
  Assert.equal_string_option ~msg:__LOC__ (Some "Novembre") (c novembre) ;
  get ctxt5a ["a";"b"] >>= fun november ->
  Assert.equal_string_option ~msg:__LOC__ (Some "November") (c november) ;
  get ctxt5a ["a";"d"] >>= fun july ->
  Assert.equal_string_option ~msg:__LOC__ (Some "July") (c july) ;
  get ctxt4b ["a";"b"] >>= fun novembre ->
  Assert.equal_string_option ~msg:__LOC__ (Some "Novembre") (c novembre) ;
  get ctxt4b ["a";"d"] >>= fun juillet ->
  Assert.equal_string_option ~msg:__LOC__ (Some "Juillet") (c juillet) ;
  Lwt.return ()

let fold_keys s k ~init ~f =
  let rec loop k acc =
    fold s k ~init:acc
      ~f:(fun file acc ->
          match file with
          | `Key k -> f k acc
          | `Dir k -> loop k acc) in
  loop k init
let keys t = fold_keys t ~init:[] ~f:(fun k acc -> Lwt.return (k :: acc))

let test_fold { genesis = ctxt } =
  set ctxt ["a"; "b"] (MBytes.of_string "Novembre") >>= fun ctxt ->
  set ctxt ["a"; "c"] (MBytes.of_string "Juin") >>= fun ctxt ->
  set ctxt ["a"; "d"; "e"] (MBytes.of_string "Septembre") >>= fun ctxt ->
  set ctxt ["f";] (MBytes.of_string "Avril") >>= fun ctxt ->
  set ctxt ["g"; "h"] (MBytes.of_string "Avril") >>= fun ctxt ->
  keys ctxt [] >>= fun l ->
  Assert.equal_string_list_list ~msg:__LOC__
    [["a";"b"];
     ["a";"c"];
     ["a";"d";"e"];
     ["f"];
     ["g";"h"]] (List.sort compare l) ;
  keys ctxt ["a"] >>= fun l ->
  Assert.equal_string_list_list
    ~msg:__LOC__ [["a";"b"]; ["a";"c"]; ["a";"d";"e"]]
    (List.sort compare l) ;
  keys ctxt ["f"] >>= fun l ->
  Assert.equal_string_list_list ~msg:__LOC__ [] l ;
  keys ctxt ["g"] >>= fun l ->
  Assert.equal_string_list_list ~msg:__LOC__ [["g";"h"]] l ;
  keys ctxt ["i"] >>= fun l ->
  Assert.equal_string_list_list ~msg:__LOC__ [] l ;
  Lwt.return ()

(******************************************************************************)

let tests : (string * (t -> unit Lwt.t)) list = [
  "simple", test_simple ;
  "continuation", test_continuation ;
  "fork", test_fork ;
  "replay", test_replay ;
  "fold", test_fold ;
]

let () =
  let module Test = Tezos_test_helpers.Test.Make(Error_monad) in
  Test.run "context." (List.map (fun (s, f) -> s, wrap_context_init f) tests)
