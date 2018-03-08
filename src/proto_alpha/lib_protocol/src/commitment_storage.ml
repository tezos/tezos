(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Commitment_repr

let test_commitments = [
  "dca88243fece75e9c22e", "4a6af2f5c466bf0a7a1001a1e9468cbfca82cef6", 1868898542104130027;
  "c736bfb7074f69bee133", "32d7b02afc218623b4e2fd85b84b06f0a1d202d2", 517697389496079974;
  "e07bb6ba71082141eae0", "17a0241048b13857abe19db7fa11ac63de3eda5e", 962290491831710023;
  "17efe5e32c28126c4e94", "7484c711d0cbd8ba6e7f9965311a4903ea17b80a", 1233665184704419921;
  "5c742b7e335b265cfa82", "3ca39ae8ddd026030af633561382d4e16c8c2cae", 131959324067470008;
  "40196c01a502608d7f22", "ebb81e0f79c568c0181e9db9cdafde7a8db65f82", 112378240876120002;
  "d7abb1cd59a66ce3fc42", "f8e91e7adba8cc32ce848a43e440de7c3e4f0866", 1060667014046690017;
  "f7a1e97970689cce7291", "4f964fbe29971a85c7152541519b1dbb8e436184", 71300478465380003;
  "f59ee7283f7eda5b3c03", "591e167977f9c4739ab17cc9d40a672737b45fa1", 283380756728119992;
  "076b7feedde492164ca3", "8a5bb1da65cbbc7f3acdc3a3dae22b43364d80a6", 1357762577679880028
]

let init_commitment ctxt (hpkh, blind, amount) =
  let half_public_key_hash = Unclaimed_public_key_hash.of_hex hpkh in
  let blinded_public_key_hash = Blinded_public_key_hash.of_hex blind in
  let amount = Tez_repr.of_mutez_exn (Int64.of_int amount) in
  Storage.Commitments.init
    ctxt half_public_key_hash
    { blinded_public_key_hash ; amount }

let init ctxt =
  fold_left_s init_commitment ctxt test_commitments >>=? fun ctxt ->
  return ctxt

let get_opt = Storage.Commitments.get_option
let delete = Storage.Commitments.delete
