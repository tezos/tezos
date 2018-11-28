.. _error_monad:

The Error Monad
===============

This has been adapted from a blog post on *michelson-lang.com*.

If you’re not familiar with monads, go take a few minutes and read a
tutorial. I personally got a lot out of this
`paper <http://homepages.inf.ed.ac.uk/wadler/papers/marktoberdorf/baastad.pdf>`__
by Philip Wadler, but there are a ton of others available online. Find
one that works for you. The error monad isn’t terribly scary as Monads
go, so once you feel like you understand the gist, come on back and see
if you can understand what’s going on.

I’m going to omit some convenience operations that a lot of monads
provide in the examples below. If you want to add them, they’re not
difficult.

Why you want the error monad
----------------------------

In Tezos, we don’t want to have the node be crashable by an improper
input. To avoid this possibility, it was decided that the system should
not use exceptions for error handling. Instead, it uses an error monad.
This design forces errors to be handled or carried through before an
output can be used. Exceptions are still occasionally used, but this is
mostly in the client and only for internal errors.

We also mix in the Lwt library, which we use for concurrency. This is
combined with the error monad and is once again used pervasively
throughout the codebase. The Lwt monad is a lot like promises in other
languages.

Without further ado, let’s write an error monad.

A simple version of the error monad
-----------------------------------

Here’s a very simple error monad.

.. code:: ocaml

    module Error : sig
      type 'a t
      (* Create a value of type t *)
      val return : 'a -> 'a t
      (* For when a computation fails *)
      val error : 'a t
      (* Apply an operation to a value in the error monad *)
      val (>>?) : 'a t -> ('a -> 'b t) -> 'b t (* bind *)
    end = struct
      type 'a t = Ok of 'a | Error
      let return x = Ok x
      let error = Error
      let (>>?) value func =
        match value with
        | Ok x -> func x
        | Error -> Error
    end

So, is this what Tezos uses? We actually already have a lot of the
structure that we’ll use later. The basic idea is that you return a
value that’s correct and return an error if the operation failed.
Outside of the error module, you can’t actually introspect an error
value. You can only dispatch on the correctness/incorrectness of the
value using bind.

What’s wrong here?

-  We can’t report any information about an error case
-  We can’t report error traces, something that’s used to improve the
   quality of error messages throughout Tezos
-  We can’t handle some errors and continue executing

A slight improvement
--------------------

Let’s now enhance our error reporting by allowing errors to contain a
description string. Now we can report messages along with our errors. Is
this enough of an improvement? Not really. We don’t have any flexibility
about how the printing works. We still can’t create error traces and we
can’t handle errors and resume executing the program.

.. code:: ocaml

    module Error : sig
      type 'a t
      val return : 'a -> 'a t
      val error : string -> 'a t
      val (>>?) : 'a t -> ('a -> 'b t) -> 'b t (* bind *)
      val print_value : ('a -> string) -> 'a t -> unit
    end = struct
      type 'a t = Ok of 'a | Error of string
      let return x = Ok x
      let error s = Error s
      let (>>?) value func =
        match value with
        | Ok x -> func x
        | Error s -> Error s
      let print_value func = function
        | Ok x -> Printf.printf "Success: %s\n" (func x)
        | Error s -> Printf.printf "Error: %s\n" s
    end

Traces
------

Now that we have the basic structure down, we can add a mechanism to let
us include traces. As a note, the error type I had above is exactly the
``result`` type from the OCaml standard library. The traces are just
lists of error messages. If you have a call you think might fail, and
you want to provide a series of errors, you can wrap that result in the
``trace`` function. If that call fails, an additional error is added.

.. code:: ocaml

    module Error : sig
      type 'a t
      val return : 'a -> 'a t
      val error : string -> 'a t
      val (>>?) : 'a t -> ('a -> 'b t) -> 'b t (* bind *)
      val print_value : ('a -> string) -> 'a t -> unit
      val trace : string -> 'a t -> 'a t
    end = struct
      type 'a t = ('a, string list) result
      let return x = Ok x
      let error s = Error [ s ]
      let (>>?) value func =
        match value with
        | Ok x -> func x
        | Error errs -> Error errs
      let print_value func = function
        | Ok x -> Printf.printf "Success: %s\n" (func x)
        | Error [ s ] -> Printf.printf "Error: %s\n" s
        | Error errors -> Printf.printf "Errors:\t%s\n" (String.concat "\n\t" errors)
      let trace error = function
        | Ok x -> Ok x
        | Error errors -> Error (error :: errors)
    end

A more descriptive message
--------------------------

Even though traces are nice, we really want to be able to store more
interesting data in the messages. We’re going to use an extensible
variant type to do this. Extensible variants allow us to add a new case
to a variant type at the cost of exhaustivity checking. We’re going to
need two new mechanisms to make this work well. The first is an error
registration scheme. In the actual error monad, this involves the data
encoding module, which is how all data is encoded/decoded in Tezos. This
module is another decently complicated part of the codebase that should
probably the subject of a future post. Since you can declare arbitrary
new errors, we’ll have a way of adding a printer for each error.

When we add a new error handler, we’ll use the ``register_handler``
function. This function will take a function that takes an error and
returns a ``string option``. These functions will look something like
this:

.. code:: ocaml

    type error += Explosion_failure of string * int;;

    register_error
      (function
        | Explosion_failure (s, i) ->
            Some (Printf.sprintf "Everything exploded: %s at %d" s i)
        | _ -> None)

I’m also renaming the ``error`` function to ``fail``. This is the
convention used by the actual `Error_monad` module. I’m also exposing the
``'a t`` type so that you can dispatch on it if you need to. This is
used several times in the Tezos codebase.

.. code:: ocaml

    module Error : sig
      type error = ..
      type 'a t = ('a, error list) result
      val return : 'a -> 'a t
      val fail : error -> 'a t
      val (>>?) : ('a -> 'b t) -> 'a t -> 'b t (* bind *)
      val print_value : ('a -> string) -> 'a t -> unit
      val trace : error -> 'a t -> 'a t
    end = struct
      type error = ..
      type 'a t = ('a, error list) result
      let fail error = Error [ error ]
      let return x = Ok x
      let (>>?) func = function
        | Ok x -> func x
        | Error errs -> Error errs
      let registered = ref []
      let register_error handler =
        registered := (handler :: !registered)
      let default_handler error =
        "Unregistered error: " ^ Obj.(extension_name @@ extension_constructor error)
      let to_string error =
        let rec find_handler = function
          | [] -> default_handler error
          | handler :: handlers ->
              begin match handler error with
                | None -> find_handler handlers
                | Some s -> s
              end
        in find_handler !registered
      let print_value func = function
        | Ok x -> Printf.printf "Success: %s\n" (func x)
        | Error [ s ] -> Printf.printf "Error: %s\n" (to_string s)
        | Error errors -> Printf.printf "Errors:\t%s\n" (String.concat "\n\t" (List.map to_string errors))
      let trace error = function
        | Ok x -> Ok x
        | Error errors -> Error (error :: errors)
    end

Putting ``Lwt.t`` in the mix
----------------------------

Tezos uses the `Lwt library <https://ocsigen.org/lwt/3.2.1/manual/manual>`__ for threading.
The Lwt monad is mixed in with the error monad module. This requires us
to add some extra combinators and reexport some functions from Lwt.

I’m also renaming the type ``t`` to ``tzresult``, as used in the Tezos
codebase.

.. code:: ocaml

    module Error : sig
      type error = ..
      type 'a tzresult = ('a, error list) result
      val ok : 'a -> 'a tzresult
      val return : 'a -> 'a tzresult Lwt.t
      val error : error -> 'a tzresult
      val fail : error -> 'a tzresult Lwt.t
      val (>>?) : 'a tzresult -> ('a -> 'b tzresult) -> 'b tzresult (* bind *)
      val (>>=?) : 'a tzresult Lwt.t -> ('a -> 'b tzresult Lwt.t) -> 'b tzresult Lwt.t
      val (>>=) : 'a Lwt.t -> ('a -> 'b Lwt.t) -> 'b Lwt.t
      val print_value : ('a -> string) -> 'a tzresult Lwt.t -> unit Lwt.t
      val trace : error -> 'a tzresult Lwt.t -> 'a tzresult Lwt.t
    end = struct
      type error = ..
      type 'a tzresult = ('a, error list) result
      let fail error = Lwt.return (Error [ error ])
      let error error = (Error [ error ])
      let ok x = Ok x
      let return x = Lwt.return (ok x)
      let (>>?) value func =
        match value with
        | Ok x -> func x
        | Error errs -> Error errs
      let (>>=) = Lwt.bind
      let (>>=?) value func =
        value >>= function
        | Ok x -> func x
        | Error errs -> Lwt.return (Error errs)
      let registered = ref []
      let register_error handler =
        registered := (handler :: !registered)
      let default_handler error =
        "Unregistered error: " ^ Obj.(extension_name @@ extension_constructor error)
      let to_string error =
        let rec find_handler = function
          | [] -> default_handler error
          | handler :: handlers ->
              begin match handler error with
                | None -> find_handler handlers
                | Some s -> s
              end
        in find_handler !registered
      let print_value func value =
        value >>= fun value ->
        begin match value with
          | Ok x -> Printf.printf "Success: %s\n" (func x)
          | Error [ s ] -> Printf.printf "Error: %s\n" (to_string s)
          | Error errors -> Printf.printf "Errors:\t%s\n" (String.concat "\n\t" (List.map to_string errors))
        end; Lwt.return ()
      let trace error value =
        value >>= function
        | Ok x -> return x
        | Error errors -> Lwt.return (Error (error :: errors))
    end

The actual Tezos error monad
----------------------------

The actual Tezos error monad adds a few things. Firstly, there are three
categories of errors:

-  :literal:`\`Temporary` - An error resulting from an operation that
   might be valid in the future, for example, a contract’s balance being
   too low to execute the intended operation. This can be fixed by
   adding more to the contract’s balance.
-  :literal:`\`Branch` - An error that occurs in one branch of the
   chain, but may not occur in a different one. For example, receiving
   an operation for an old or future protocol version.
-  :literal:`\`Permanent` - An error that is not recoverable because the
   operation is never going to be valid. For example, an invalid ꜩ
   notation.

The registration scheme also uses data encodings. Here’s an example from
the `validator <../api/odoc/tezos-node-shell/Tezos_node_shell/Validator/index.html>`__:

.. code:: ocaml

    register_error_kind
        `Permanent
        ~id:"validator.wrong_level"
        ~title:"Wrong level"
        ~description:"The block level is not the expected one"
        ~pp:(fun ppf (e, g) ->
            Format.fprintf ppf
              "The declared level %ld is not %ld" g e)
        Data_encoding.(obj2
                         (req "expected" int32)
                         (req "provided" int32))
        (function Wrong_level (e, g)   -> Some (e, g) | _ -> None)
        (fun (e, g) -> Wrong_level (e, g))

An error takes a category, id, title, description, and encoding. You
must specify a function to take an error to an optional value of the
encoding type and a function to take a value of the encoded type and
create an error value. A pretty printer can optionally be specified, but
may also be omitted.

The actual error monad and its tracing features can be seen in this
function which parses contracts:

.. code:: ocaml

    let parse_script
      : ?type_logger: (int * (Script.expr list * Script.expr list) -> unit) ->
      context -> Script.storage -> Script.code -> ex_script tzresult Lwt.t
      = fun ?type_logger ctxt
        { storage; storage_type = init_storage_type }
        { code; arg_type; ret_type; storage_type } ->
        trace
          (Ill_formed_type (Some "parameter", arg_type))
          (Lwt.return (parse_ty arg_type)) >>=? fun (Ex_ty arg_type) ->
        trace
          (Ill_formed_type (Some "return", ret_type))
          (Lwt.return (parse_ty ret_type)) >>=? fun (Ex_ty ret_type) ->
        trace
          (Ill_formed_type (Some "initial storage", init_storage_type))
          (Lwt.return (parse_ty init_storage_type)) >>=? fun (Ex_ty init_storage_type) ->
        trace
          (Ill_formed_type (Some "storage", storage_type))
          (Lwt.return (parse_ty storage_type)) >>=? fun (Ex_ty storage_type) ->
        let arg_type_full = Pair_t (arg_type, storage_type) in
        let ret_type_full = Pair_t (ret_type, storage_type) in
        Lwt.return (ty_eq init_storage_type storage_type) >>=? fun (Eq _) ->
        trace
          (Ill_typed_data (None, storage, storage_type))
          (parse_data ?type_logger ctxt storage_type storage) >>=? fun storage ->
        trace
          (Ill_typed_contract (code, arg_type, ret_type, storage_type, []))
          (parse_returning (Toplevel { storage_type }) ctxt ?type_logger arg_type_full ret_type_full code)
        >>=? fun code ->
        return (Ex_script { code; arg_type; ret_type; storage; storage_type })

Each specific type error from the typechecking process is wrapped in a
more general error that explains which part of the program was
malformed. This improves the error reporting. You can also see the bind
operator used between functions to continue only if an error does not
occur. This function also operates in the ``Lwt`` monad, which is
largely hidden via the error monad.
