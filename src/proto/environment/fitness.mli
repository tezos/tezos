
type fitness = MBytes.t list

val compare: fitness -> fitness -> int
val pp: Format.formatter -> fitness -> unit
val to_string: fitness -> string

val encoding: fitness Data_encoding.t
