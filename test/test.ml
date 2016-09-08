
let (>>=) = Lwt.bind

let keep_dir = try ignore (Sys.getenv "KEEPDIR") ; true with _ -> false

let () = Printexc.record_backtrace true

(** Helpers for tests *)

let log fmt = Format.eprintf fmt
let fail fmt = Format.kasprintf failwith fmt

let run_test name f =
  let base_dir = Filename.temp_file "tezos_test_" "" in
  log "---- beginning of test %S in %s ----\n%!" name base_dir ;
  Lwt_unix.unlink base_dir >>= fun () ->
  Lwt_unix.mkdir base_dir 0o777 >>= fun () ->
  Lwt.catch
    (fun () -> f base_dir >>= fun () ->
      log "[test succeeded]\n%!" ;
      Lwt.return (Ok ()))
    (function
      | Failure msg ->
          log "[test FAILED with %s]\n%!" msg ;
          Printexc.print_backtrace stderr ;
          flush stderr ;
          Lwt.return (Error name)
      | e ->
          log "[test FAILED with exception %s]\n%!" (Printexc.to_string e) ;
          Printexc.print_backtrace stderr ;
          flush stderr ;
          Lwt.return (Error name)) >>= fun r ->
  (if not keep_dir then
     Utils.remove_dir base_dir
   else
     Lwt.return_unit) >>= fun () ->
  log "---- end of test %S ----\n%!" name ;
  Lwt.return r

let run prefix l =
  let results =
    List.map (fun (name, f) -> Lwt_main.run (run_test (prefix ^ name) f)) l in
  let failed =
    List.fold_left
      (fun acc r ->
         match r with
         | Ok () -> acc
         | Error name -> name :: acc)
      [] results in
  match failed with
  | [] ->
      Printf.printf "All tests succeeded\n%!"
  | failed ->
      Printf.printf "Some tests failed:\n";
      List.iter (Printf.printf "- %s\n") failed;
      Printf.printf "%!";
      exit 1
