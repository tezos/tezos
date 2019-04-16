(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

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

let default_template = "$(date) - $(section): $(message)"

type cfg = {
  output : Output.t ;
  default_level : Internal_event.level ;
  rules : string option ;
  template : Lwt_log_core.template ;
}

let create_cfg
    ?(output = Output.Stderr)
    ?(default_level = Internal_event.Notice)
    ?rules ?(template = default_template) () =
  { output ; default_level ; rules ; template }

let default_cfg = create_cfg ()

let cfg_encoding =
  let open Data_encoding in
  conv
    (fun {output ; default_level ; rules ; template } ->
       (output, default_level, rules, template))
    (fun (output, default_level, rules, template) ->
       { output ; default_level ; rules ; template })
    (obj4
       (dft "output"
          ~description: "Output for the logging function. Either 'stdout', \
                         'stderr' or the name of a log file ."
          Output.encoding default_cfg.output)
       (dft "level"
          ~description: "Verbosity level: one of 'fatal', 'error', 'warn',\
                         'notice', 'info', 'debug'."
          Internal_event.Level.encoding default_cfg.default_level)
       (opt "rules"
          ~description: "Fine-grained logging instructions. Same format as \
                         described in `tezos-node run --help`, DEBUG section. \
                         In the example below, sections 'p2p' and all sections \
                         starting by 'client' will have their messages logged \
                         up to the debug level, whereas the rest of log sections \
                         will be logged up to the notice level."
          string)
       (dft "template"
          ~description: "Format for the log file, see \
                         http://ocsigen.org/lwt/dev/api/Lwt_log_core#2_Logtemplates."
          string default_cfg.template))

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

let find_log_rules default =
  match Sys.(getenv_opt "TEZOS_LOG", getenv_opt "LWT_LOG") with
  | Some rules, None -> "environment variable TEZOS_LOG", Some rules
  | None, Some rules -> "environment variable LWT_LOG", Some rules
  | None, None -> "configuration file", default
  | Some rules, Some _ ->
      Format.eprintf
        "@[<v 2>@{<warning>@{<title>Warning@}@} \
         Both environment variables TEZOS_LOG and LWT_LOG \
         defined, using TEZOS_LOG.@]@\n@." ;
      "environment varible TEZOS_LOG", Some rules

let initialize ?(cfg = default_cfg) () =
  Lwt_log_core.add_rule "*"
    (Internal_event.Level.to_lwt_log cfg.default_level) ;
  let origin, rules = find_log_rules cfg.rules in
  begin match rules with
    | None -> Lwt.return_unit
    | Some rules ->
        try
          Lwt_log_core.load_rules rules ~fail_on_error:true ;
          Lwt.return_unit
        with _ ->
          Printf.ksprintf Lwt.fail_with
            "Incorrect log rules defined in %s" origin
  end >>= fun () ->
  init ~template:cfg.template cfg.output


