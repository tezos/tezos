(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

module Test = Kaputt.Abbreviations.Test

let keep_dir = try ignore (Sys.getenv "KEEPDIR") ; true with _ -> false

let rec remove_dir dir =
  if Sys.file_exists dir then begin
    Array.iter (fun file ->
        let f = Filename.concat dir file in
        if Sys.is_directory f then remove_dir f
        else Sys.remove f)
      (Sys.readdir dir);
    Unix.rmdir dir
  end

let output name res =
  let open Kaputt in
  let open Test in
  let out = stderr in
  match res with
  | Passed ->
      Printf.fprintf out "Test '%s' ... passed\n" name
  | Failed { Assertion.expected_value = "" ; actual_value = ""  ; message } ->
      Printf.fprintf out "Test '%s' ... failed\n  %s \n" name message
  | Failed { Assertion.expected_value ; actual_value ; message = "" } ->
      if expected_value <> actual_value then
        Printf.fprintf out
          "Test '%s' ... failed\n  expected `%s` but received `%s`\n"
          name
          expected_value
          actual_value
      else
        Printf.fprintf out
          "Test '%s' ... failed\n  expected anything excluding `%s` \
           but received `%s`\n"
          name
          expected_value
          actual_value
  | Failed { Assertion.expected_value ; actual_value ; message } ->
      if expected_value <> actual_value then
        Printf.fprintf out
          "Test '%s' ... failed\n  %s (expected `%s` but received `%s`)\n"
          name
          message
          expected_value
          actual_value
      else
        Printf.fprintf out
          "Test '%s' ... failed\n  %s (expected anything excluding `%s` \
           but received `%s`)\n"
          name
          message
          expected_value
          actual_value
  | Uncaught (e, bt) ->
      Printf.fprintf out
        "Test '%s' ... raised an exception\n  %s\n%s\n"
        name (Printexc.to_string e) bt
  | Report (valid, total, uncaught, counterexamples, categories) ->
      Printf.fprintf out
        "Test '%s' ... %d/%d case%s passed%s\n"
        name
        valid
        total
        (if valid > 1 then "s" else "")
        (match uncaught with
         | 0 -> ""
         | 1 -> " (1 uncaught exception)"
         | n -> " (" ^ (string_of_int n) ^ " uncaught exceptions)");
      if counterexamples <> [] then
        Printf.fprintf out "  counterexample%s: %s\n"
          (if (List.length counterexamples) > 1 then "s" else "")
          (String.concat ", " counterexamples);
      if (List.length categories) > 1 then begin
        Printf.fprintf out "  categories:\n";
        List.iter
          (fun (c, n) ->
             Printf.fprintf out
               "    %s -> %d occurrence%s\n"
               c n (if n > 1 then "s" else ""))
          categories
      end
  | Exit_code c ->
      Printf.fprintf out "Test '%s' ... returned code %d\n" name c

let run prefix tests =
  let tests =
    List.map
      (fun (title, f) ->
         let base_dir = Filename.temp_file "tezos_test_" "" in
         Unix.unlink base_dir ;
         Unix.mkdir base_dir 0o777 ;
         let title = prefix ^ title in
         title,
         Test.make_simple_test
           ~title
           (fun () ->
              let finalise () =
                if keep_dir then
                  Format.eprintf "Kept data dir %s@." base_dir
                else
                  remove_dir base_dir
              in
              try
                match Lwt_main.run (f base_dir) with
                | Ok () -> finalise ()
                | Error err ->
                    finalise () ;
                    Format.kasprintf
                      (fun message ->
                         raise @@
                         Kaputt.Assertion.Failed
                           { expected_value = "" ;
                             actual_value = "" ;
                             message })
                      "%a" Error_monad.pp_print_error err
              with exn ->
                finalise () ;
                raise exn))
      tests in
  let passed = ref 0 in
  let failed = ref 0 in
  let uncaught = ref 0 in
  let total = ref 0 in
  List.iter
    (fun (title, test) ->
       let res = Test.exec_test test  in
       begin
         match res with
         | Passed ->
             incr passed;
             incr total
         | Failed _ ->
             incr failed;
             incr total
         | Uncaught _ ->
             incr uncaught;
             incr total
         | Report (pass, tot, unc, _, _) ->
             passed := !passed + pass;
             failed := !failed + (tot - pass -unc);
             uncaught := !uncaught + unc;
             total := !total + tot
         | Exit_code c ->
             incr (if c = 0 then passed else failed);
             incr total
       end ;
       output title res ;
       flush stderr)
    tests ;
  Format.eprintf "SUMMARY: %d/%d passed (%.2f%%) -- %d failed, \
                  %d uncaught exceptions.@."
    !passed !total (float_of_int !passed *. 100. /. float_of_int !total)
    !failed !uncaught ;
  if !total <> !passed then exit 1
