
val debug: ('a, Format.formatter, unit, unit) format4 -> 'a
val log_info: ('a, Format.formatter, unit, unit) format4 -> 'a
val log_notice: ('a, Format.formatter, unit, unit) format4 -> 'a
val warn: ('a, Format.formatter, unit, unit) format4 -> 'a
val log_error: ('a, Format.formatter, unit, unit) format4 -> 'a
val fatal_error: ('a, Format.formatter, unit, unit) format4 -> 'a

val lwt_debug: ('a, Format.formatter, unit, unit Lwt.t) format4 -> 'a
val lwt_log_info: ('a, Format.formatter, unit, unit Lwt.t) format4 -> 'a
val lwt_log_notice: ('a, Format.formatter, unit, unit Lwt.t) format4 -> 'a
val lwt_warn: ('a, Format.formatter, unit, unit Lwt.t) format4 -> 'a
val lwt_log_error: ('a, Format.formatter, unit, unit Lwt.t) format4 -> 'a
