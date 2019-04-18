module Context : sig
  type t
  (** Opaque data structure that holds context information
      (precomputed tables etc.).

      Do not create a new context object for each operation, as
      construction is far slower than all other API calls (~100 times
      slower than an ECDSA verification).

      A constructed context can safely be used from multiple threads
      simultaneously, but API call that take a non-const pointer to a
      context need exclusive access to it. In particular this is the
      case for secp256k1_context_destroy and
      secp256k1_context_randomize.

      Regarding randomization, either do it once at creation time (in
      which case you do not need any locking for the other calls), or
      use a read-write lock. *)

  val create : ?sign:bool -> ?verify:bool -> unit -> t
  (** [create ?sign ?bool ()] is a freshly allocated [t]. *)

  val clone : t -> t
  (** [clone t] is a copy of [t]. *)

  val randomize : t -> Bigstring.t -> bool
  (** While secp256k1 code is written to be constant-time no matter
      what secret values are, it's possible that a future compiler may
      output code which isn't, and also that the CPU may not emit the
      same radio frequencies or draw the same amount power for all
      values.

      This function provides a seed which is combined into the
      blinding value: that blinding value is added before each
      multiplication (and removed afterwards) so that it does not
      affect function results, but shields against attacks which rely
      on any input-dependent behaviour.

      You should call this after secp256k1_context_create or
      secp256k1_context_clone, and may call this repeatedly
      afterwards. *)

end

module Key : sig
  type secret
  type public
  type _ t

  val buffer : _ t -> Bigstring.t
  (** [buffer k] is the underlying buffer of [k]. DO NOT MODIFY. *)

  val secret_bytes : int
  (** Length of a secret key in memory: 32 bytes *)

  val public_bytes : int
  (** Length of a public key in memory: 64 bytes *)

  val compressed_pk_bytes : int
  (** Length of the compressed serialization of a public key: 33 bytes *)

  val uncompressed_pk_bytes : int
  (** Length of the uncompressed serialization of a public key: 65 bytes *)

  val bytes : _ t -> int
  (** [bytes k] is the length of [k] in memory (the length of the
      underlying [Bigstring.t]). *)

  val serialized_bytes : ?compressed:bool -> _ t -> int
  (** [serialized_bytes ?compressed k] is the length of the
      serialization (compressed) of [k].*)

  val equal : 'a t -> 'a t -> bool
  val copy : 'a t -> 'a t

  (** {2 Aritmetic operations } *)

  val negate : Context.t -> 'a t -> 'a t
  val add_tweak : Context.t -> 'a t -> Bigstring.t -> 'a t
  val mul_tweak : Context.t -> 'a t -> Bigstring.t -> 'a t
  val neuterize : Context.t -> _ t -> public t option
  val neuterize_exn : Context.t -> _ t -> public t
  val combine : Context.t -> _ t list -> public t option
  val combine_exn : Context.t -> _ t list -> public t

  (** {2 Input/Output} *)

  val read_sk : Context.t -> Bigstring.t -> (secret t, string) result
  val read_sk_exn : Context.t -> Bigstring.t -> secret t
  val read_pk : Context.t -> Bigstring.t -> (public t, string) result
  val read_pk_exn : Context.t -> Bigstring.t -> public t
  val write : ?compress:bool -> Context.t -> ?pos:int -> Bigstring.t -> _ t -> int
  val to_bytes : ?compress:bool -> Context.t -> _ t -> Bigstring.t
end

module Sign : sig

  (** {2 Signature} *)

  type plain
  type recoverable
  type _ t

  val buffer : _ t -> Bigstring.t
  (** [buffer signature] is the underlying buffer of [signature]. DO
      NOT MODIFY. *)

  val plain_bytes : int
  (** 64 bytes *)

  val recoverable_bytes : int
  (** 65 bytes *)

  val msg_bytes : int
  (** 32 bytes *)

  val equal : 'a t -> 'a t -> bool
  val to_plain : Context.t -> _ t -> plain t

  (** {3 Input/Output} *)

  val read : Context.t -> Bigstring.t -> (plain t, string) result
  val read_exn : Context.t -> Bigstring.t -> plain t
  val read_der : Context.t -> Bigstring.t -> (plain t, string) result
  val read_der_exn : Context.t -> Bigstring.t -> plain t

  val read_recoverable :
    Context.t -> Bigstring.t -> (recoverable t, string) result
  (** [read_recoverable_exn ctx buf] reads a recoverable signature in
      [buf] if everything goes well or return an error otherwise. *)

  val read_recoverable_exn : Context.t -> Bigstring.t -> recoverable t
  (** [read_recoverable_exn ctx buf] reads a recoverable signature in
      [buf].

      @raise [Invalid_argument] if [buf] is less than 65 bytes long
      or [buf] does not contain a valid recoverable signature. *)

  val write_exn : ?der:bool -> Context.t -> Bigstring.t -> _ t -> int

  val write : ?der:bool -> Context.t -> Bigstring.t -> _ t -> (int, string) result

  val to_bytes : ?der:bool -> Context.t -> _ t -> Bigstring.t
  (** [to_bytes ?der ctx signature] writes the serialization of
      [signature] in a freshly allocated [Bigstring.t], which is then
      returned. *)

  (** {3 Sign} *)

  val normalize :
    Context.t -> plain t -> plain t option
  (** [normalize ctx sig] is the normalized lower-S form of [Some
      normalized_sig] if [sig] was not already in this form, or [None]
      otherwise. *)

  (** {4 Creation} *)

  val sign : Context.t -> sk:Key.secret Key.t -> Bigstring.t -> (plain
                                                                   t, string) result

  val sign_exn : Context.t -> sk:Key.secret Key.t -> Bigstring.t ->
    plain t

  val sign_recoverable : Context.t -> sk:Key.secret Key.t ->
    Bigstring.t -> (recoverable t, string) result

  val sign_recoverable_exn : Context.t -> sk:Key.secret Key.t ->
    Bigstring.t -> recoverable t

  (** {4 Direct write} *)

  val write_sign : Context.t -> Bigstring.t -> sk:Key.secret Key.t ->
    msg:Bigstring.t -> (int, string) result (** [write_sign ctx buf ~sk
                                                ~msg] writes signs [msg] with [sk] and writes the signature to
                                                [buf] at [?pos]. It returns the number of bytes written (64) on
                                                success, or ar error message otherwise. *)

  val write_sign_exn : Context.t -> Bigstring.t -> sk:Key.secret Key.t
    -> msg:Bigstring.t -> int (** [write_sign_exn ctx buf ~sk ~msg]
                                  writes signs [msg] with [sk] and writes the signature to [buf] at
                                  [?pos]. It returns the number of bytes written (64).

                                  @raise Invalid_argument if [buf] is not long enough to contain
                                  a signature or signing has failed. *)

  val write_sign_recoverable : Context.t -> sk:Key.secret Key.t ->
    msg:Bigstring.t -> Bigstring.t -> (int, string) result

  val write_sign_recoverable_exn : Context.t -> sk:Key.secret Key.t ->
    msg:Bigstring.t -> Bigstring.t -> int

  (** {4 Verification} *)

  val verify_exn : Context.t -> pk:Key.public Key.t -> msg:Bigstring.t
    -> signature:_ t -> bool

  val verify : Context.t -> pk:Key.public Key.t -> msg:Bigstring.t ->
    signature:_ t -> (bool, string) result

  (** {4 Recovery} *)

  val recover_exn : Context.t -> signature:recoverable t ->
    Bigstring.t -> Key.public Key.t

  val recover : Context.t -> signature:recoverable t -> Bigstring.t ->
    (Key.public Key.t, string) result end
