let () =
  let oc = open_out "c_flags.sexp" in
  output_string oc (if Sys.word_size = 32 then "(-DMDB_VL32)" else "()") ;
  close_out oc
