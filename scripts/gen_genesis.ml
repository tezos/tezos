#use "topfind";;
#thread;;
#require "threads";;
#require "stringext";;
#require "lwt";;
#require "lwt.unix";;
#require "zarith";;
#require "re";;
#require "hacl";;
#require "calendar";;
#mod_use "../src/lib_stdlib/tzString.ml";;
#mod_use "../src/lib_stdlib/option.ml";;
#mod_use "../src/lib_stdlib/tzList.ml";;
#mod_use "../src/lib_stdlib/utils.ml";;
#mod_use "../src/lib_crypto/base58.ml";;


let prefix = "BLockGenesisGenesisGenesisGenesisGenesis"
let rec genesis () =
  let date =
    Lwt_main.run
      (Lwt_process.pread_line (Lwt_process.shell "date +%FT%TZ --utc")) in
  let suffix = String.sub Digest.(to_hex (string date)) 0 5 in
  match Base58.raw_decode (prefix ^ suffix ^ "crcCRC") with
  | None -> genesis ()
  | Some p ->
      let p = String.sub p 0 (String.length p - 4) in
      Base58.safe_encode p, date

let genesis, date = genesis ()

let () =
  Lwt_main.run (Lwt_io.lines_to_file "alphanet_version"
                  (Lwt_stream.of_list [date]))

let sed =
  Format.sprintf
    "sed -i \
     -e 's/Time.of_notation_exn \"[^\\\"]*\"/Time.of_notation_exn \"%s\"/' \
     -e 's/BLockGenesisGenesisGenesisGenesisGenesis.........../%s/' \
     ../src/bin_node/node_run_command.ml"
    date
    genesis

let _ =
  Lwt_main.run (Lwt_process.exec (Lwt_process.shell sed))

let sed =
  Format.sprintf
    "sed -i \
     -e 's/name = \"TEZOS[^\"]*\" ;/name = \"TEZOS_%s\" ;/' \
     ../src/lib_shell/distributed_db_message.ml"
    date

let _ =
  Lwt_main.run (Lwt_process.exec (Lwt_process.shell sed))
