open Kaputt.Abbreviations

let keep_dir = try ignore (Sys.getenv "KEEPDIR") ; true with _ -> false

let make_test ~title test =
  Test.add_simple_test ~title (fun () -> Lwt_main.run test)

let rec remove_dir dir =
  Array.iter (fun file ->
      let f = Filename.concat dir file in
      if Sys.is_directory f then remove_dir f
      else Sys.remove f)
    (Sys.readdir dir);
  Unix.rmdir dir

let run prefix tests =
  let dirs =
    List.fold_left (fun dirs (title, f) ->
        let base_dir = Filename.temp_file "tezos_test_" "" in
        Unix.unlink base_dir;
        Unix.mkdir base_dir 0o777;
        make_test ~title:(prefix ^ title) (f base_dir);
        base_dir :: dirs)
      [] tests in
  Test.launch_tests ();
  if not keep_dir then
    List.iter remove_dir dirs
