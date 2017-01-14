(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

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

module Make(S : sig val name: string end) : LOG = struct

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
module Webclient = Make(struct let name = "webclient" end)

let template = "$(date) $(name)[$(pid)]: $(message)"

let default_logger () =
  Lwt_log.channel ~template ~close_mode:`Keep ~channel:Lwt_io.stderr ()

type kind =
  | Null
  | Stdout
  | Stderr
  | File of string
  | Syslog
  | Manual of Lwt_log.logger

let init kind =
  let logger =
    match kind with
    | Stderr ->
        default_logger ()
    | Stdout ->
        Lwt_log.channel ~template ~close_mode:`Keep ~channel:Lwt_io.stdout ()
    | File file_name ->
        Lwt_main.run (Lwt_log.file ~file_name ~template ())
    | Null ->
        Lwt_log.null
    | Syslog ->
        Printf.eprintf "Warning: log_kind \"syslog\" not yet implemented.\n%!";
        default_logger ()
    | Manual logger -> logger in
  Lwt_log.default := logger
