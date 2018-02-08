(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Lwt.Infix

module Output = struct

  type t =
    | Null
    | Stdout
    | Stderr
    | File of string
    | Syslog of Lwt_log.syslog_facility

  let to_string : t -> string = function
    | Null -> "/dev/null"
    | Stdout -> "stdout"
    | Stderr -> "stderr"
    | File fp -> fp
    | Syslog `Auth -> "syslog:auth"
    | Syslog `Authpriv -> "syslog:authpriv"
    | Syslog `Cron -> "syslog:cron"
    | Syslog `Daemon -> "syslog:daemon"
    | Syslog `FTP -> "syslog:ftp"
    | Syslog `Kernel -> "syslog:kernel"
    | Syslog `Local0 -> "syslog:local0"
    | Syslog `Local1 -> "syslog:local1"
    | Syslog `Local2 -> "syslog:local2"
    | Syslog `Local3 -> "syslog:local3"
    | Syslog `Local4 -> "syslog:local4"
    | Syslog `Local5 -> "syslog:local5"
    | Syslog `Local6 -> "syslog:local6"
    | Syslog `Local7 -> "syslog:local7"
    | Syslog `LPR -> "syslog:lpr"
    | Syslog `Mail -> "syslog:mail"
    | Syslog `News -> "syslog:news"
    | Syslog `Syslog -> "syslog:syslog"
    | Syslog `User -> "syslog:user"
    | Syslog `UUCP -> "syslog:uucp"
    | Syslog `NTP -> "syslog:ntp"
    | Syslog `Security -> "syslog:security"
    | Syslog `Console -> "syslog:console"

  let of_string : string -> t = function
    | "/dev/null" | "null" -> Null
    | "stdout" -> Stdout
    | "stderr" -> Stderr
    | "syslog:auth" -> Syslog `Auth
    | "syslog:authpriv" -> Syslog `Authpriv
    | "syslog:cron" -> Syslog `Cron
    | "syslog:daemon" -> Syslog `Daemon
    | "syslog:ftp" -> Syslog `FTP
    | "syslog:kernel" -> Syslog `Kernel
    | "syslog:local0" -> Syslog `Local0
    | "syslog:local1" -> Syslog `Local1
    | "syslog:local2" -> Syslog `Local2
    | "syslog:local3" -> Syslog `Local3
    | "syslog:local4" -> Syslog `Local4
    | "syslog:local5" -> Syslog `Local5
    | "syslog:local6" -> Syslog `Local6
    | "syslog:local7" -> Syslog `Local7
    | "syslog:lpr" -> Syslog `LPR
    | "syslog:mail" -> Syslog `Mail
    | "syslog:news" -> Syslog `News
    | "syslog:syslog" -> Syslog `Syslog
    | "syslog:user" -> Syslog `User
    | "syslog:uucp" -> Syslog `UUCP
    | "syslog:ntp" -> Syslog `NTP
    | "syslog:security" -> Syslog `Security
    | "syslog:console" -> Syslog `Console
    (* | s when start_with "syslog:" FIXME error or warning. *)
    | fp ->
        (* TODO check absolute path *)
        File fp

  let encoding =
    let open Data_encoding in
    conv to_string of_string string

  let of_string str =
    try
      Some (Data_encoding.Json.destruct encoding (`String str))
    with _ -> None

  let to_string output =
    match Data_encoding.Json.construct encoding output with
    | `String res -> res
    | #Data_encoding.json -> assert false

  let pp fmt output =
    Format.fprintf fmt "%s" (to_string output)
end

let init ?(template = Logging.default_template) output =
  let open Output in
  begin
    match output with
    | Stderr ->
        Lwt.return @@
        Lwt_log.channel ~template ~close_mode:`Keep ~channel:Lwt_io.stderr ()
    | Stdout ->
        Lwt.return @@
        Lwt_log.channel ~template ~close_mode:`Keep ~channel:Lwt_io.stdout ()
    | File file_name ->
        Lwt_log.file ~file_name ~template ()
    | Null ->
        Lwt.return @@
        Lwt_log.null
    | Syslog facility ->
        Lwt.return @@
        Lwt_log.syslog ~template ~facility ()
  end >>= fun logger ->
  Lwt_log.default := logger ;
  Lwt.return_unit

let close () =
  Lwt_log.close !Lwt_log.default
