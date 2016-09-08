val log : ('a, Format.formatter, unit) format -> 'a
val fail : ('a, Format.formatter, unit, 'b) format4 -> 'a
val run : string -> (string * (string -> unit Lwt.t)) list -> unit
