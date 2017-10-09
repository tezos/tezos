
type t

val add : t -> int64 -> t
val diff : t -> t -> int64

val equal : t -> t -> bool
val compare : t -> t -> int

val (=) : t -> t -> bool
val (<>) : t -> t -> bool
val (<) : t -> t -> bool
val (<=) : t -> t -> bool
val (>=) : t -> t -> bool
val (>) : t -> t -> bool
val min : t -> t -> t
val max : t -> t -> t

val of_seconds : int64 -> t
val to_seconds : t -> int64

val of_notation : string -> t option
val of_notation_exn : string -> t
val to_notation : t -> string

val encoding : t Data_encoding.t
val rfc_encoding : t Data_encoding.t

val pp_hum : Format.formatter -> t -> unit



