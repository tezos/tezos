
open Error_monad

module Public_key_hash :
  S.INTERNAL_HASH with type t = Tezos_crypto.Ed25519.Public_key_hash.t

module Public_key : sig
  include (module type of  Tezos_crypto.Ed25519.Public_key)
  val encoding: t Data_encoding.t
  val param:
    ?name:string ->
    ?desc:string ->
    ('a, 'b, 'c) Cli_entries.params ->
    (t -> 'a, 'b, 'c) Cli_entries.params
  val of_b58check: string -> t tzresult
end

module Secret_key : sig
  include (module type of  Tezos_crypto.Ed25519.Secret_key)
  val encoding: t Data_encoding.t
  val param:
    ?name:string ->
    ?desc:string ->
    ('a, 'b, 'c) Cli_entries.params ->
    (t -> 'a, 'b, 'c) Cli_entries.params
  val of_b58check: string -> t tzresult
end

module Signature : sig
  include (module type of  Tezos_crypto.Ed25519.Signature)
  val encoding: t Data_encoding.t
  val param:
    ?name:string ->
    ?desc:string ->
    ('a, 'b, 'c) Cli_entries.params ->
    (t -> 'a, 'b, 'c) Cli_entries.params
  val of_b58check: string -> t tzresult
end

include (module type of Tezos_crypto.Ed25519)
  with module Public_key_hash := Public_key_hash
   and module Public_key := Public_key
   and module Secret_key := Secret_key
   and module Signature := Signature
