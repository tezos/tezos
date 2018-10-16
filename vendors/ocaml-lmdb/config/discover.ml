let () =
  let oc = open_out "c_flags.sexp" in
  let w = "-W -Wall -Wno-unused-parameter -Wbad-function-cast -Wuninitialized" in
  let thread = "-pthread" in
  let opt = "-O2 -g" in
  Printf.fprintf oc "(%s %s %s %s)" w thread opt
    (if Sys.word_size = 32 then "-DMDB_VL32" else "") ;
  close_out oc
