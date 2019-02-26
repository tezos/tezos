open Irmin
open Lwt.Infix

module Store = Irmin_lmdb.Make
    (Metadata.None)
    (Contents.String)
    (Path.String_list)
    (Branch.String)
    (Hash.SHA1)

let config = Irmin_lmdb.config "/tmp/irmin-lmdb"

let date = ref 0L

let info () =
  date := Int64.add !date 1L;
  Info.v ~date:!date ~author:"test lmdb" "commit"

let fill t entries =
  Lwt_list.iter_s (fun (k, v) -> Store.set ~info t k v) entries

let store () =
  Store.Repo.v config >>= fun repo ->
  Store.Branch.remove repo Store.Branch.master  >>= fun () ->
  Store.master repo

let branches = [Store.Branch.master]

let test_basics _switch () =
  let promote_1 () =
    store () >>= fun t ->
    fill t [
      ["foo"]       , "a";
      ["bar";"toto"], "1";
      ["bar";"titi"], "2";
    ] >>= fun () ->
    Store.Head.get t >>= fun root ->
    Store.gc ~branches ~repo:(Store.repo t) ~keep:1 root >>= fun stats ->
    Alcotest.(check int) "1: promoted commits"  1 (Store.promoted_commits stats);
    Alcotest.(check int) "1: promoted nodes"    2 (Store.promoted_nodes stats);
    Alcotest.(check int) "1: promoted contents" 3 (Store.promoted_contents stats);
    Store.find t ["foo"] >>= fun v ->
    Alcotest.(check (option string)) "foo" (Some "a") v;
    Store.find t ["bar";"toto"] >>= fun v ->
    Alcotest.(check (option string)) "bar/toto" (Some "1") v;
    Store.find t ["bar";"titi"] >>= fun v ->
    Alcotest.(check (option string)) "bar/titi" (Some "2") v;
    Store.Commit.parents root >|= function
    | [] -> ()
    | _  -> Alcotest.fail "parent should not exists!"
  in
  let promote_2 () =
    store () >>= fun t ->
    fill t [
      ["foo"]       , "a";
      ["bar";"toto"], "1";
      ["bar";"titi"], "2";
      ["bar";"titi"], "3";
    ] >>= fun () ->
    Store.Head.get t >>= fun root ->
    Store.gc ~branches ~repo:(Store.repo t) ~keep:2 root >>= fun stats ->
    Alcotest.(check int) "2: promoted commits"  2 (Store.promoted_commits stats);
    Alcotest.(check int) "2: promoted nodes"    4 (Store.promoted_nodes stats);
    Alcotest.(check int) "2: promoted contents" 4 (Store.promoted_contents stats);
    Store.find t ["foo"] >>= fun v ->
    Alcotest.(check (option string)) "foo" (Some "a") v;
    Store.find t ["bar";"toto"] >>= fun v ->
    Alcotest.(check (option string)) "bar/toto" (Some "1") v;
    Store.find t ["bar";"titi"] >>= fun v ->
    Alcotest.(check (option string)) "bar/titi" (Some "3") v;
    Store.Commit.parents root >>= function
    | []  -> Alcotest.fail "parent should exist"
    | [p] ->
        (Store.Commit.parents p >|= function
          | [] -> ()
          | _  -> Alcotest.fail "grand-parents should not exist")
    | _ -> Alcotest.fail "too many parents!"
  in
  promote_1 () >>=
  promote_2

let test_basics_loop sw () =
  let rec loop = function
    | 0 -> Lwt.return ()
    | n ->
        test_basics sw () >>= fun () ->
        loop (n-1)
  in
  loop 100

let test_concurrency _ () =
  store () >>= fun t ->
  fill t [
    ["foo"]       , "a";
    ["bar";"toto"], "1";
    ["bar";"titi"], "2";
    ["bar";"titi"], "3";
    ] >>= fun () ->
  let before_pivot () =
    fill t [
      ["bar"; "yo0"], "a";
      ["bar"; "yo1"], "a";
      ["bar"; "yo1"], "b";
    ] >>= fun () ->
    (* check that we can read the new values *)
    Store.get t ["bar";"yo1"] >|= fun v ->
    Alcotest.(check string) "writing to the store still works" "b" v;
  in
  Store.Head.get t >>= fun root ->
  let wait_for_gc, gc = Lwt.task () in
  Lwt.async (fun () ->
      Store.gc ~branches ~repo:(Store.repo t) ~before_pivot ~keep:2 root >|= fun _stats ->
      Lwt.wakeup gc ()
    );
  wait_for_gc >>= fun () ->

  (* check that the new values are there *)

  Store.get t ["bar";"titi"] >>= fun v ->
  Alcotest.(check string) "bar/titi" "3" v;
  Store.get t ["bar";"yo1"] >|= fun v ->
  Alcotest.(check string) "new values are still here after a pivot" "b" v

let () =
  Alcotest.run "irmin-lmdb" [
    "GC", [
      Alcotest_lwt.test_case "basics"      `Quick test_basics;
      Alcotest_lwt.test_case "basic loop"  `Quick test_basics_loop;
      Alcotest_lwt.test_case "concurrency" `Quick test_concurrency;

    ]]
