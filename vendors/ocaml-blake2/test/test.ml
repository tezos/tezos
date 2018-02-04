
open Blake2

type vector = {
  data_in : string list ;
  data_key : string option ;
  data_out : string ;
}

let vectors = [
  { data_in = [ "000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f202122232425262728292a2b2c2d2e2f303132333435363738393a3b3c3d3e3f404142434445464748494a4b4c4d4e4f505152535455565758595a5b5c5d5e5f" ] ;
    data_key = None ;
    data_out = "1c077e279de6548523502b6df800ffdab5e2c3e9442eb838f58c295f3b147cef9d701c41c321283f00c71affa0619310399126295b78dd4d1a74572ef9ed5135" ;
  } ;
  { data_in = [ "000102030405060708090a0b0c0d0e0f101112131415" ; "161718"; "191a1b1c1d1e1f202122232425262728292a2b2c2d2e2f303132333435363738393a3b3c3d3e3f404142434445464748494a4b4c4d4e4f505152535455565758595a5b5c5d5e5f" ] ;
    data_key = None ;
    data_out = "1c077e279de6548523502b6df800ffdab5e2c3e9442eb838f58c295f3b147cef9d701c41c321283f00c71affa0619310399126295b78dd4d1a74572ef9ed5135" ;
  } ;
  { data_in = [ "000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f202122232425262728292a2b2c2d2e2f303132333435363738393a3b3c3d3e3f404142434445464748494a4b4c4d4e4f505152535455565758595a5b5c5d5e5f606162636465666768696a6b6c6d6e6f707172737475767778797a7b7c7d7e7f808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9fa0a1a2a3a4a5a6a7a8a9aaabacadaeafb0b1b2b3b4b5b6b7b8b9babbbcbdbebfc0c1c2c3c4c5c6c7c8c9cacbcccdcecfd0d1d2d3d4d5d6d7d8d9dadbdcdddedfe0e1e2e3e4e5e6e7e8e9eaebecedeeeff0f1f2f3" ] ;
    data_key = Some ("000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f202122232425262728292a2b2c2d2e2f303132333435363738393a3b3c3d3e3f") ;
    data_out = "b39614268fdd8781515e2cfebf89b4d5402bab10c226e6344e6b9ae000fb0d6c79cb2f3ec80e80eaeb1980d2f8698916bd2e9f747236655116649cd3ca23a837" ;
  } ;
]

let test_update { data_in ; data_key ; data_out } =
  let key =
    match data_key with None -> None | Some s -> Some (Cstruct.of_hex s) in
  let data_out = Cstruct.of_hex data_out in
  let d = Blake2b.init ?key (Cstruct.len data_out) in
  List.iter (fun s -> Blake2b.update d (Cstruct.of_hex s)) data_in ;
  let Blake2b.Hash h = Blake2b.final d in
  assert Cstruct.(equal data_out h)

let test_direct { data_in ; data_key ; data_out } =
  let key =
    match data_key with None -> None | Some s -> Some (Cstruct.of_hex s) in
  let data_out = Cstruct.of_hex data_out in
  let Blake2b.Hash h =
    Blake2b.direct ?key
      (Cstruct.of_hex (String.concat "" data_in))
      (Cstruct.len data_out) in
  assert Cstruct.(equal data_out h)

let update_tests =
  List.mapi
    (fun i v -> string_of_int i, `Quick, fun () -> test_update v)
    vectors

let direct_tests =
  List.mapi
    (fun i v -> string_of_int i, `Quick, fun () -> test_direct v)
    vectors

let () =
  Alcotest.run "blake2b" [
    "update", update_tests ;
    "direct", direct_tests ;
  ]

