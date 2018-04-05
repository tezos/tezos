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
  "tz1fkmDXEQdua3u71JBVh4eGUGqK4t1G8xhZ", "btz1T77Ly5U1bWNBR5KzDSgNFST5Bh5F1eB6g", 1868898542104130027L ;
  "tz1doNkK6RKaRswsuKJV4erT6HauYSm9fuHi", "btz1QxTPgszARgWioEor3eMehxW3osfhw3KoJ", 517697389496079974L ;
  "tz1g6zFsci4YR8p1MJrkyc6wAKizR6mqJmyQ", "btz1NUYc1tV5VBksMNuQG4AuZF9Xudh1sDJni", 962290491831710023L ;
  "tz1MpbcwGFWVBBWoxwm6iQH5Hzh9mCXbnETJ", "btz1Wwifd8vbQqnbuzSbQLvJEjQ9FUoxVJm68", 1233665184704419921L ;
  "tz1U4t2PmX5cZVUui4BNaiRVokLa6AxB5G9Z", "btz1RrFkp9GmnypoNGRSyURkBQNUs4PPYG8SR", 131959324067470008L ;
  "tz1RUxPjviua4XJM78XjSKGUCAS9R3y8Bdof", "btz1hozabzP9HdRakJddzyea7DgPHzJ5PB37N", 112378240876120002L ;
  "tz1fJPeueQKJrTj2SFV2PmyCinLYJKXCEMf4", "btz1j1k4nrZB4r8RpTYiy8zbi3Fappopkb8ZF", 1060667014046690017L ;
  "tz1iDPZLxcGf5CqCNpTuuMdtu3zKpJ6HvvFR", "btz1TaSfoSNhFoqwqbPC9iC19rN24KJtB7skD", 71300478465380003L ;
  "tz1i2kbtVu65dP739qLGRpJNujRM8pdpkH3p", "btz1USqQRuvASPXwseXkGTWeWv4dn3VwMVPEk", 283380756728119992L ;
  "tz1LKGCg6ESJLDviHkT8Jc7tUwjw4h3d9MaF", "btz1YwCKMbBLRL1qkBjAHGCwjbWDqiTAEFpbw", 1357762577679880028L ;
]

let init_commitment ctxt (hpkh, blind, amount) =
  let half_public_key_hash = Unclaimed_public_key_hash.of_b58check_exn hpkh in
  let blinded_public_key_hash = Blinded_public_key_hash.of_b58check_exn blind in
  let amount = Tez_repr.of_mutez_exn amount in
  Storage.Commitments.init
    ctxt half_public_key_hash
    { blinded_public_key_hash ; amount }

let init ctxt =
  fold_left_s init_commitment ctxt test_commitments >>=? fun ctxt ->
  return ctxt

let get_opt = Storage.Commitments.get_option
let delete = Storage.Commitments.delete
