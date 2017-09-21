
let prefix = "BLockGenesisGenesisGenesisGenesisGenesis"
let suffix = "BBBBB"
let p = Base58.raw_decode (prefix ^ suffix ^ "crcCRC")
let p = String.sub p 0 (String.length p - 4)
let p = Base58.safe_encode p
let () = print_endline p

