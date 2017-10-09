(** Tezos Utility library - Hexadecimal encoding *)

(** Parses a sequence of hexadecimal characters pairs as bytes *)
val hex_of_bytes: MBytes.t -> string

(** Prints a sequence of bytes as hexadecimal characters pairs *)
val bytes_of_hex: string -> MBytes.t

(** Interprets a sequence of hexadecimal characters pairs representing
    bytes as the characters codes of an OCaml string. *)
val hex_decode: string -> string

(** Formats the codes of the characters of an OCaml string as a
    sequence of hexadecimal character pairs. *)
val hex_encode: string -> string
