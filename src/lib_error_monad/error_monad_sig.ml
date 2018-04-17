(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(** Categories of error *)
type error_category =
  [ `Branch (** Errors that may not happen in another context *)
  | `Temporary (** Errors that may not happen in a later context *)
  | `Permanent (** Errors that will happen no matter the context *)
  ]

module type S = sig

  type error = ..

  (** Catch all error when 'serializing' an error. *)
  type error += private Unclassified of string
  (** Catch all error when 'deserializing' an error. *)
  type error += private Unregistred_error of Data_encoding.json

  val pp: Format.formatter -> error -> unit
  val pp_print_error: Format.formatter -> error list -> unit

  (** An error serializer *)
  val error_encoding : error Data_encoding.t
  val json_of_error : error -> Data_encoding.json
  val error_of_json : Data_encoding.json -> error

  (** {2 Error documentation} ************************************************)

  (** Error information *)
  type error_info =
    { category : error_category ;
      id : string ;
      title : string ;
      description : string ;
      schema : Data_encoding.json_schema }

  val pp_info: Format.formatter -> error_info -> unit

  (** Retrieves information of registered errors *)
  val get_registered_errors : unit -> error_info list

  (** {2 Error classification} ***********************************************)

  (** For other modules to register specialized error serializers *)
  val register_error_kind :
    error_category ->
    id:string -> title:string -> description:string ->
    ?pp:(Format.formatter -> 'err -> unit) ->
    'err Data_encoding.t ->
    (error -> 'err option) -> ('err -> error) ->
    unit

  (** Classify an error using the registered kinds *)
  val classify_errors : error list -> error_category

  (** {2 Monad definition} ***************************************************)

  (** The error monad wrapper type, the error case holds a stack of
      error, initialized by the first call to {!fail} and completed by
      each call to {!trace} as the stack is rewinded. The most general
      error is thus at the top of the error stack, going down to the
      specific error that actually caused the failure. *)
  type 'a tzresult = ('a, error list) result

  (** A serializer for result of a given type *)
  val result_encoding :
    'a Data_encoding.t ->
    'a tzresult Data_encoding.t

  (** Sucessful result *)
  val ok : 'a -> 'a tzresult

  (** Sucessful return *)
  val return : 'a -> 'a tzresult Lwt.t

  (** Erroneous result *)
  val error : error -> 'a tzresult

  (** Erroneous return *)
  val fail : error -> 'a tzresult Lwt.t

  (** Non-Lwt bind operator *)
  val (>>?) : 'a tzresult -> ('a -> 'b tzresult) -> 'b tzresult

  (** Bind operator *)
  val (>>=?) :
    'a tzresult Lwt.t -> ('a -> 'b tzresult Lwt.t) -> 'b tzresult Lwt.t

  (** Lwt's bind reexported *)
  val (>>=) : 'a Lwt.t -> ('a -> 'b Lwt.t) -> 'b Lwt.t
  val (>|=) : 'a Lwt.t -> ('a -> 'b) -> 'b Lwt.t

  (** To operator *)
  val (>>|?) : 'a tzresult Lwt.t -> ('a -> 'b) -> 'b tzresult Lwt.t

  (** Non-Lwt to operator *)
  val (>|?) : 'a tzresult -> ('a -> 'b) -> 'b tzresult

  (** Enrich an error report (or do nothing on a successful result) manually *)
  val record_trace : error -> 'a tzresult -> 'a tzresult

  (** Automatically enrich error reporting on stack rewind *)
  val trace : error -> 'b tzresult Lwt.t -> 'b tzresult Lwt.t

  (** Erroneous return on failed assertion *)
  val fail_unless : bool -> error -> unit tzresult Lwt.t
  val fail_when : bool -> error -> unit tzresult Lwt.t

  val unless : bool -> (unit -> unit tzresult Lwt.t) -> unit tzresult Lwt.t
  val _when : bool -> (unit -> unit tzresult Lwt.t) -> unit tzresult Lwt.t

  (* Usage: [_assert cond __LOC__ "<fmt>" ...] *)
  val _assert :
    bool -> string ->
    ('a, Format.formatter, unit, unit tzresult Lwt.t) format4 -> 'a

  (** {2 In-monad list iterators} ********************************************)

  (** A {!List.iter} in the monad *)
  val iter_s : ('a -> unit tzresult Lwt.t) -> 'a list -> unit tzresult Lwt.t
  val iter_p : ('a -> unit tzresult Lwt.t) -> 'a list -> unit tzresult Lwt.t
  val iter2_p : ('a -> 'b -> unit tzresult Lwt.t) -> 'a list -> 'b list -> unit tzresult Lwt.t
  val iteri2_p : (int -> 'a -> 'b -> unit tzresult Lwt.t) -> 'a list -> 'b list -> unit tzresult Lwt.t

  (** A {!List.map} in the monad *)
  val map_s : ('a -> 'b tzresult Lwt.t) -> 'a list -> 'b list tzresult Lwt.t
  val map_p : ('a -> 'b tzresult Lwt.t) -> 'a list -> 'b list tzresult Lwt.t
  val mapi_s : (int -> 'a -> 'b tzresult Lwt.t) -> 'a list -> 'b list tzresult Lwt.t
  val mapi_p : (int -> 'a -> 'b tzresult Lwt.t) -> 'a list -> 'b list tzresult Lwt.t

  (** A {!List.map2} in the monad *)
  val map2 :
    ('a -> 'b -> 'c tzresult) -> 'a list -> 'b list -> 'c list tzresult
  val map2_s :
    ('a -> 'b -> 'c tzresult Lwt.t) -> 'a list -> 'b list ->
    'c list tzresult Lwt.t
  val mapi2_s :
    (int -> 'a -> 'b -> 'c tzresult Lwt.t) -> 'a list -> 'b list ->
    'c list tzresult Lwt.t

  (** A {!List.filter_map} in the monad *)
  val filter_map_s :
    ('a -> 'b option tzresult Lwt.t) -> 'a list -> 'b list tzresult Lwt.t
  val filter_map_p :
    ('a -> 'b option tzresult Lwt.t) -> 'a list -> 'b list tzresult Lwt.t

  (** A {!List.fold_left} in the monad *)
  val fold_left_s :
    ('a -> 'b -> 'a tzresult Lwt.t) -> 'a -> 'b list -> 'a tzresult Lwt.t

  (** A {!List.fold_right} in the monad *)
  val fold_right_s :
    ('a -> 'b -> 'b tzresult Lwt.t) -> 'a list -> 'b -> 'b tzresult Lwt.t

  (** A {!Lwt.join} in the monad *)
  val join : unit tzresult Lwt.t list -> unit tzresult Lwt.t

end
