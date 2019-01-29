#!/usr/bin/env ocaml

               #load "unix.cma"

let query cmd =
  let chan = Unix.open_process_in cmd in
  try
    let out = input_line chan in
    if Unix.close_process_in chan = Unix.WEXITED 0 then out
    else "unkown"
  with End_of_file -> "unkown"

let () =
  Format.printf "@[<v>let commit_hash = \"%s\"@,"
    (query "git show -s --pretty=format:%H");
  Format.printf "let abbreviated_commit_hash = \"%s\"@,"
    (query "git show -s --pretty=format:%h");
  Format.printf "let committer_date = \"%s\"@]@."
    (query "git show -s --pretty=format:%ci")
