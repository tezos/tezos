#use "topfind";;
#require "stringext";;
#require "lwt";;
#require "zarith";;
#require "nocrypto";;
#mod_use "../src/minutils/utils.ml";;
#mod_use "../src/utils/base58.ml";;
let prefix = "BLockGenesisGenesisGenesisGenesisGenesis"
let suffix = "FFFFF"
let p = Base58.raw_decode (prefix ^ suffix ^ "crcCRC")
let p = String.sub p 0 (String.length p - 4)
let p = Base58.safe_encode p
let () = print_endline p

