(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Lwt.Infix

module type LOG = sig

  val debug: ('a, Format.formatter, unit, unit) format4 -> 'a
  val log_info: ('a, Format.formatter, unit, unit) format4 -> 'a
  val log_notice: ('a, Format.formatter, unit, unit) format4 -> 'a
  val warn: ('a, Format.formatter, unit, unit) format4 -> 'a
  val log_error: ('a, Format.formatter, unit, unit) format4 -> 'a
  val fatal_error: ('a, Format.formatter, unit, 'b) format4 -> 'a

  val lwt_debug: ('a, Format.formatter, unit, unit Lwt.t) format4 -> 'a
  val lwt_log_info: ('a, Format.formatter, unit, unit Lwt.t) format4 -> 'a
  val lwt_log_notice: ('a, Format.formatter, unit, unit Lwt.t) format4 -> 'a
  val lwt_warn: ('a, Format.formatter, unit, unit Lwt.t) format4 -> 'a
  val lwt_log_error: ('a, Format.formatter, unit, unit Lwt.t) format4 -> 'a

end

let log_f
    ?exn ?(section = Lwt_log.Section.main) ?location ?logger ~level format =
  if level < Lwt_log.Section.level section then
    Format.ikfprintf (fun _ -> Lwt.return_unit) Format.std_formatter format
  else
    Format.kasprintf
      (fun msg -> Lwt_log.log ?exn ~section ?location ?logger ~level msg)
      format

let ign_log_f
    ?exn ?(section = Lwt_log.Section.main) ?location ?logger ~level format =
  if level < Lwt_log.Section.level section then
    Format.ikfprintf (fun _ -> ()) Format.std_formatter format
  else
    Format.kasprintf
      (fun msg -> Lwt_log.ign_log ?exn ~section ?location ?logger ~level msg)
      format

let sections = ref []

module Make(S : sig val name: string end) : LOG = struct

  let () = sections := S.name :: !sections
  let section = Lwt_log.Section.make S.name

  let debug fmt = ign_log_f ~section ~level:Lwt_log.Debug fmt
  let log_info fmt = ign_log_f ~section ~level:Lwt_log.Info fmt
  let log_notice fmt = ign_log_f ~section ~level:Lwt_log.Notice fmt
  let warn fmt = ign_log_f ~section ~level:Lwt_log.Warning fmt
  let log_error fmt = ign_log_f ~section ~level:Lwt_log.Error fmt
  let fatal_error fmt =
    Format.kasprintf
      (fun s -> Lwt_log.ign_fatal ~section s; Lwt_exit.exit 1)
      fmt

  let lwt_debug fmt = log_f ~section ~level:Lwt_log.Debug fmt
  let lwt_log_info fmt = log_f ~section ~level:Lwt_log.Info fmt
  let lwt_log_notice fmt = log_f ~section ~level:Lwt_log.Notice fmt
  let lwt_warn fmt = log_f ~section ~level:Lwt_log.Warning fmt
  let lwt_log_error fmt = log_f ~section ~level:Lwt_log.Error fmt

end

module Core = Make(struct let name = "core" end)
module Net = Make(struct let name = "net" end)
module RPC = Make(struct let name = "rpc" end)
module Db = Make(struct let name = "db" end)
module Updater = Make(struct let name = "updater" end)
module Node = struct
  module State = Make(struct let name = "node.state" end)
  module Validator = Make(struct let name = "node.validator" end)
  module Prevalidator = Make(struct let name = "node.prevalidator" end)
  module Discoverer = Make(struct let name = "node.discoverer" end)
  module Worker = Make(struct let name = "node.worker" end)
  module Main = Make(struct let name = "node.main" end)
end
module Client = struct
  module Blocks = Make(struct let name = "client.blocks" end)
  module Mining = Make(struct let name = "client.mining" end)
  module Endorsement = Make(struct let name = "client.endorsement" end)
  module Revelation = Make(struct let name = "client.revealation" end)
  module Denunciation = Make(struct let name = "client.denunciation" end)
end

type template = Lwt_log.template
let default_template = "$(date) - $(section): $(message)"

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

let init ?(template = default_template) output =
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

type level = Lwt_log_core.level =
  | Debug
      (** Debugging message. They can be automatically removed by the
          syntax extension. *)
  | Info
      (** Informational message. Suitable to be displayed when the
          program is in verbose mode. *)
  | Notice
      (** Same as {!Info}, but is displayed by default. *)
  | Warning
      (** Something strange happend *)
  | Error
      (** An error message, which should not means the end of the
          program. *)
  | Fatal

let level_encoding =
  let open Data_encoding in
  conv
    (function
      | Fatal -> "fatal"
      | Error -> "error"
      | Warning -> "warning"
      | Notice -> "notice"
      | Info -> "info"
      | Debug -> "debug")
    (function
      | "error" -> Error
      | "warn" -> Warning
      | "notice" -> Notice
      | "info" -> Info
      | "debug" -> Debug
      | "fatal" -> Fatal
      | _ -> invalid_arg "Logging.level")
    string
