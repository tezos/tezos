let () =
  (* inspired by https://github.com/vbmithr/ocaml-secp256k1-internal/blob/master/config/discover.ml *)
  let ccopts = "-Ofast -mtune=generic -fwrapv -fomit-frame-pointer -funroll-loops -Wall -Werror -Wfatal-errors" in
  (* the important bit is -DKRML_NOUINT128 which enables the verified (but slow) implementation of UInt128 in FStar.c rather than the builtin __int128 which is only available on 64-bit platforms. *)
  let ccopts32 = "-Ofast -fwrapv -fomit-frame-pointer -funroll-loops -DKRML_NOUINT128 -Wall -Werror -Wfatal-errors" in

  let oc = open_out "c_flags.sexp" in
  let sixtyfour = Sys.word_size = 64 in
  let flags = if sixtyfour then ccopts else ccopts32 in
  Printf.fprintf oc "(%s)%!" flags ;
  close_out oc
