(* PBKDF1 *)
let test_pbkdf1 ~hash ~password ~salt ~count ~dk_len ~dk =
  let open Nocrypto.Uncommon.Cs in
  let salt = of_hex salt
  and dk = of_hex dk
  and password = Cstruct.of_string password in
  (fun () ->
     let edk = Pbkdf.pbkdf1 ~hash ~password ~salt ~count ~dk_len in
     let sedk = Cstruct.to_string edk
     and sdk = Cstruct.to_string dk
     in
     Alcotest.check Alcotest.string "PBKDF1 test" sedk sdk)

let test_pbkdf1_invalid_arg ~hash ~password ~salt ~count ~dk_len ~msg =
  let open Nocrypto.Uncommon.Cs in
  let salt = of_hex salt
  and password = Cstruct.of_string password in
  (fun () ->
     Alcotest.check_raises
       msg
       (Invalid_argument msg)
       (fun () -> ignore (Pbkdf.pbkdf1 ~hash ~password ~salt ~count ~dk_len)))

(* Taken from http://www.di-mgt.com.au/cryptoKDFs.html *)
let pbkdf1_test1 =
  test_pbkdf1
    ~hash:`SHA1
    ~password:"password"
    ~salt:"78578e5a5d63cb06"
    ~count:1000
    ~dk_len:16
    ~dk:"dc19847e05c64d2faf10ebfb4a3d2a20"

let pbkdf1_test2 =
  test_pbkdf1_invalid_arg
    ~hash:`SHA1
    ~password:"password"
    ~salt:"78578e5a5d63cb"
    ~count:1000
    ~dk_len:16
    ~msg:"salt should be 8 bytes"

let pbkdf1_test3 =
  test_pbkdf1_invalid_arg
    ~hash:`SHA1
    ~password:"password"
    ~salt:"78578e5a5d63cb0600"
    ~count:1000
    ~dk_len:16
    ~msg:"salt should be 8 bytes"

let pbkdf1_test4 =
  test_pbkdf1_invalid_arg
    ~hash:`SHA1
    ~password:"password"
    ~salt:"78578e5a5d63cb06"
    ~count:(-1)
    ~dk_len:16
    ~msg:"count must be a positive integer"

let pbkdf1_test5 =
  test_pbkdf1_invalid_arg
    ~hash:`SHA1
    ~password:"password"
    ~salt:"78578e5a5d63cb06"
    ~count:0
    ~dk_len:16
    ~msg:"count must be a positive integer"

let pbkdf1_test6 =
  test_pbkdf1_invalid_arg
    ~hash:`SHA1
    ~password:"password"
    ~salt:"78578e5a5d63cb06"
    ~count:1000
    ~dk_len:24
    ~msg:"derived key too long"

let pbkdf1_test7 =
  test_pbkdf1_invalid_arg
    ~hash:`SHA1
    ~password:"password"
    ~salt:"78578e5a5d63cb06"
    ~count:1000
    ~dk_len:0
    ~msg:"derived key length must be a positive integer"

let pbkdf1_tests = [
  "Test Case 1", `Quick, pbkdf1_test1;
  "Test Case 2", `Quick, pbkdf1_test2;
  "Test Case 3", `Quick, pbkdf1_test3;
  "Test Case 4", `Quick, pbkdf1_test4;
  "Test Case 5", `Quick, pbkdf1_test5;
  "Test Case 6", `Quick, pbkdf1_test6;
  "Test Case 7", `Quick, pbkdf1_test7;
]


(* PBKDF2 *)
let test_pbkdf2 ~prf ~password ~salt ~count ~dk_len ~dk =
  let open Nocrypto.Uncommon.Cs in
  let salt = of_hex salt
  and dk = of_hex dk
  and password = Cstruct.of_string password
  in
  (fun () ->
     let edk = Pbkdf.pbkdf2 ~prf ~password ~salt ~count ~dk_len in
     let sedk = Cstruct.to_string edk
     and sdk = Cstruct.to_string dk
     in
     Alcotest.check Alcotest.string "PBKDF2 test" sedk sdk)

let test_pbkdf2_invalid_arg ~prf ~password ~salt ~count ~dk_len ~msg () =
  let salt = Nocrypto.Uncommon.Cs.of_hex salt
  and password = Cstruct.of_string password
  in
  Alcotest.check_raises
    msg
    (Invalid_argument msg)
    (fun () -> ignore (Pbkdf.pbkdf2 ~prf ~password ~salt ~count ~dk_len))

(* Taken from https://github.com/randombit/botan/blob/master/src/tests/data/pbkdf/pbkdf2.vec *)
let pbkdf2_test1 =
  test_pbkdf2
    ~prf:`SHA1
    ~password:""
    ~salt:"0001020304050607"
    ~count:10000
    ~dk_len:32l
    ~dk:"59b2b1143b4cb1059ec58d9722fb1c72471e0d85c6f7543ba5228526375b0127"

let pbkdf2_test2 =
  test_pbkdf2
    ~prf:`SHA1
    ~password:"jyueqgxrscgglpxdykcf"
    ~salt:"9b56e55328a4c97a250738f8dba1b992e8a1b508"
    ~count:10000
    ~dk_len:14l
    ~dk:"df6d9d72872404bf73e708cf3b7d"

let pbkdf2_test3 =
  test_pbkdf2
    ~prf:`SHA1
    ~password:"aqrqsznzvvzgtksammgo"
    ~salt:"57487813cdd2220dfc485d932a2979ee8769ea8b"
    ~count:101
    ~dk_len:40l
    ~dk:"fa13f40af1ade2a30f2fffd66fc8a659ef95e6388c1682fc0fe4d15a70109517a32942e39c371440"

let pbkdf2_test4 =
  test_pbkdf2
    ~prf:`SHA1
    ~password:"ltexmfeyylmlbrsyikaw"
    ~salt:"ed1f39a0a7f3889aaf7e60743b3bc1cc2c738e60"
    ~count:1000
    ~dk_len:10l
    ~dk:"027afadd48f4be8dcc4f"

let pbkdf2_test5 =
  test_pbkdf2
    ~prf:`SHA1
    ~password:"cxgnyrcgrvllylolsjpo"
    ~salt:"94ac88200743fb0f6ac51be62166cbef08d94c15"
    ~count:1
    ~dk_len:32l
    ~dk:"7c0d009fc91b48cb6d19bafbfccff3e2ccabfe725eaa234e56bde1d551c132f2"

let pbkdf2_test6 =
  test_pbkdf2
    ~prf:`SHA1
    ~password:"xqyfhrxehiedlhewnvbj"
    ~salt:"24a1a50b17d63ee8394b69fc70887f4f94883d68"
    ~count:5
    ~dk_len:32l
    ~dk:"4661301d3517ca4443a6a607b32b2a63f69996299df75db75f1e0b98dd0eb7d8"

let pbkdf2_test7 =
  test_pbkdf2
    ~prf:`SHA1
    ~password:"andaqkpjwabvcfnpnjkl"
    ~salt:"9316c80801623cc2734af74bec42cf4dbaa3f6d5"
    ~count:100
    ~dk_len:30l
    ~dk:"82fb44a521448d5aac94b5158ead1e4dcd7363081a747b9f7626752bda2d"

let pbkdf2_test8 =
  test_pbkdf2
    ~prf:`SHA1
    ~password:"hsavvyvocloyuztlsniu"
    ~salt:"612cc61df3cf2bdb36e10c4d8c9d73192bddee05"
    ~count:100
    ~dk_len:30l
    ~dk:"f8ec2b0ac817896ac8189d787c6424ed24a6d881436687a4629802c0ecce"

let pbkdf2_test9 =
  test_pbkdf2
    ~prf:`SHA1
    ~password:"eaimrbzpcopbusaqtkmw"
    ~salt:"45248f9d0cebcb86a18243e76c972a1f3b36772a"
    ~count:100
    ~dk_len:34l
    ~dk:"c9a0b2622f13916036e29e7462e206e8ba5b50ce9212752eb8ea2a4aa7b40a4cc1bf"

let pbkdf2_test10 =
  test_pbkdf2
    ~prf:`SHA1
    ~password:"gwrxpqxumsdsmbmhfhmfdcvlcvngzkig"
    ~salt:"a39b76c6eec8374a11493ad08c246a3e40dfae5064f4ee3489c273646178"
    ~count:1000
    ~dk_len:64l
    ~dk:"4c9db7ba24955225d5b845f65ef24ef1b0c6e86f2e39c8ddaa4b8abd26082d1f350381fadeaeb560dc447afc68a6b47e6ea1e7412f6cf7b2d82342fccd11d3b4"

let pbkdf2_test11 =
  test_pbkdf2
    ~prf:`SHA256
    ~password:"xyz"
    ~salt:"0001020304050607"
    ~count: 10000
    ~dk_len:48l
    ~dk:"defd2987fa26a4672f4d16d98398432ad95e896bf619f6a6b8d4ed1faf98e8b531b39ffb66966d0e115a6cd8e70b72d0"

let pbkdf2_test12 =
  test_pbkdf2
    ~prf:`SHA384
    ~password:"xyz"
    ~salt:"0001020304050607"
    ~count:10000
    ~dk_len:48l
    ~dk:"47a3ae920b24edaa2bb53155808554b13fab58df62b81f043d9812e9f2881164df20bbffa54e5ee2489fa183b6718a74"

let pbkdf2_test13 =
  test_pbkdf2
    ~prf:`SHA512
    ~password:"xyz"
    ~salt:"0001020304050607"
    ~count:10000
    ~dk_len:48l
    ~dk:"daf8a734327745eb63d19054dbd4018a682cef11086a1bfb63fdbc16158c2f8b0742802f36aef1b1df92accbea5d31a5"

let pbkdf2_test14 =
  test_pbkdf2_invalid_arg
    ~prf:`SHA1
    ~password:"password"
    ~salt:"0001020304050607"
    ~count:(-1)
    ~dk_len:48l
    ~msg:"count must be a positive integer"

let pbkdf2_test15 =
  test_pbkdf2_invalid_arg
    ~prf:`SHA1
    ~password:"password"
    ~salt:"0001020304050607"
    ~count:0
    ~dk_len:48l
    ~msg:"count must be a positive integer"

let pbkdf2_test16 =
  test_pbkdf2_invalid_arg
    ~prf:`SHA1
    ~password:"password"
    ~salt:"0001020304050607"
    ~count:1000
    ~dk_len:(-1l)
    ~msg:"derived key length must be a positive integer"

let pbkdf2_test17 =
  test_pbkdf2_invalid_arg
    ~prf:`SHA1
    ~password:"password"
    ~salt:"0001020304050607"
    ~count:1000
    ~dk_len:0l
    ~msg:"derived key length must be a positive integer"

let pbkdf2_tests = [
  "Test Case 1", `Quick, pbkdf2_test1;
  "Test Case 2", `Quick, pbkdf2_test2;
  "Test Case 3", `Quick, pbkdf2_test3;
  "Test Case 4", `Quick, pbkdf2_test4;
  "Test Case 5", `Quick, pbkdf2_test5;
  "Test Case 6", `Quick, pbkdf2_test6;
  "Test Case 7", `Quick, pbkdf2_test7;
  "Test Case 8", `Quick, pbkdf2_test8;
  "Test Case 9", `Quick, pbkdf2_test9;
  "Test Case 10", `Quick, pbkdf2_test10;
  "Test Case 11", `Quick, pbkdf2_test11;
  "Test Case 12", `Quick, pbkdf2_test12;
  "Test Case 13", `Quick, pbkdf2_test13;
  "Test Case 14", `Quick, pbkdf2_test14;
  "Test Case 15", `Quick, pbkdf2_test15;
  "Test Case 16", `Quick, pbkdf2_test16;
  "Test Case 17", `Quick, pbkdf2_test17;
]

let () =
  Alcotest.run "PBKDF Tests" [
    "PBKDF1 tests", pbkdf1_tests;
    "PBKDF2 tests", pbkdf2_tests;
  ]
