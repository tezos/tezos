open Alcotest
open Uecc

let nb_iterations = 10

let checki = check int

let test_sksize () =
  checki __LOC__ 21 (sk_size secp160r1) ;
  checki __LOC__ 24 (sk_size secp192r1) ;
  checki __LOC__ 28 (sk_size secp224r1) ;
  checki __LOC__ 32 (sk_size secp256r1) ;
  checki __LOC__ 32 (sk_size secp256k1) ;
  ()

let test_pksize () =
  checki __LOC__ 40 (pk_size secp160r1) ;
  checki __LOC__ 48 (pk_size secp192r1) ;
  checki __LOC__ 56 (pk_size secp224r1) ;
  checki __LOC__ 64 (pk_size secp256r1) ;
  checki __LOC__ 64 (pk_size secp256k1) ;
  ()

let test_export_curve curve =
  match keypair curve with
  | None -> assert false
  | Some (sk, pk) ->
      let sk_bytes = to_bytes ~compress:false sk in
      let pk_bytes = to_bytes ~compress:false pk in
      checki __LOC__ (sk_size curve) (Bigstring.length sk_bytes) ;
      checki __LOC__ (pk_size curve + 1) (Bigstring.length pk_bytes) ;
      match sk_of_bytes curve sk_bytes,
            pk_of_bytes curve pk_bytes with
      | Some (sk', pk'), Some pk'' ->
          assert (equal sk sk') ;
          assert (equal pk pk') ;
          assert (equal pk pk'') ;
          assert (equal pk' pk') ;
      | _ -> assert false

let test_export_curve curve =
  for _i = 0 to nb_iterations - 1 do
    test_export_curve curve
  done

let test_export () =
  test_export_curve secp160r1 ;
  test_export_curve secp192r1 ;
  test_export_curve secp224r1 ;
  test_export_curve secp256r1 ;
  test_export_curve secp256k1 ;
  ()

let test_export_curve_compressed curve =
  match keypair curve with
  | None -> assert false
  | Some (sk, pk) ->
      let sk_bytes = to_bytes sk in
      let pk_bytes = to_bytes pk in
      checki __LOC__ (sk_size curve) (Bigstring.length sk_bytes) ;
      checki __LOC__ (compressed_size curve) (Bigstring.length pk_bytes) ;
      match sk_of_bytes curve sk_bytes,
            pk_of_bytes curve pk_bytes with
      | Some (sk', pk'), Some pk'' ->
          assert (equal sk sk') ;
          assert (equal pk pk') ;
          assert (equal pk pk'') ;
          assert (equal pk' pk') ;
      | _ -> assert false

let test_export_curve_compressed curve =
  for _i = 0 to nb_iterations - 1 do
    test_export_curve_compressed curve
  done

let test_export_compressed () =
  test_export_curve_compressed secp160r1 ;
  test_export_curve_compressed secp192r1 ;
  test_export_curve_compressed secp224r1 ;
  test_export_curve_compressed secp256r1 ;
  test_export_curve_compressed secp256k1 ;
  ()

let test_keypair_curve curve =
  match keypair curve with
  | None -> assert false
  | Some (sk, pk) ->
      assert (equal sk sk) ;
      assert (equal pk pk) ;
      let pk' = neuterize sk in
      assert (equal pk pk')

let test_keypair_curve curve =
  for _i = 0 to nb_iterations - 1 do
    test_keypair_curve curve
  done

let test_keypair () =
  test_keypair_curve secp160r1 ;
  test_keypair_curve secp192r1 ;
  test_keypair_curve secp224r1 ;
  test_keypair_curve secp256r1 ;
  test_keypair_curve secp256k1 ;
  ()

let test_dh_curve curve =
  match keypair curve, keypair curve with
  | Some (sk, pk), Some (sk', pk') ->
      begin match dh sk pk', dh sk' pk with
        | Some secret, Some secret' ->
            assert (Bigstring.equal secret secret')
        | _ -> assert false
      end
  | _ -> assert false

let test_dh_curve curve =
  for _i = 0 to nb_iterations - 1 do
    test_dh_curve curve
  done

let test_dh () =
  test_dh_curve secp160r1 ;
  test_dh_curve secp192r1 ;
  test_dh_curve secp224r1 ;
  test_dh_curve secp256r1 ;
  test_dh_curve secp256k1 ;
  ()

let msg =
  Bigstring.of_string "Voulez-vous coucher avec moi, ce soir ?"

let test_sign_curve curve =
  match keypair curve with
  | None -> assert false
  | Some (sk, pk) ->
      let signature = Bigstring.create (pk_size curve) in
      begin match write_sign sk signature ~msg with
        | nb_written when nb_written = (pk_size curve) ->
            assert (verify pk ~msg ~signature)
        | _ -> assert false
      end ;
      match sign sk msg with
      | None -> assert false
      | Some signature ->
          assert (verify pk ~msg ~signature)

let test_sign_curve curve =
  for _i = 0 to nb_iterations - 1 do
    test_sign_curve curve
  done

let test_sign () =
  test_sign_curve secp160r1 ;
  test_sign_curve secp192r1 ;
  test_sign_curve secp224r1 ;
  test_sign_curve secp256r1 ;
  test_sign_curve secp256k1 ;
  ()

let basic = [
  "sksize", `Quick, test_sksize ;
  "pksize", `Quick, test_pksize ;
  "export", `Quick, test_export ;
  "export_compressed", `Quick, test_export_compressed ;
  "keypair", `Quick, test_keypair ;
  "dh", `Quick, test_dh ;
  (* "sign", `Quick, test_sign ; *)
]

let () =
  Alcotest.run "uecc" [
    "basic", basic ;
  ]
