.. _data_encoding:

The ``data_encoding`` library
=============================

Throughout the Tezos protocol, data is serialized so that it can be used
via RPC, written to disk, or placed in a block. This
serialization/de-serialization is handled via the :package:`tezos-data-encoding`
library by providing a set primitive encodings and a variety of combinators.

Examples/Tutorial
-----------------

Encoding an integer
~~~~~~~~~~~~~~~~~~~

Integers are defined as other concrete data types with a generic
encoding type ``type 'a encoding``. This means that it is an encoding
to/from type ``int``. There are a variety of ways to encode an integer,
depending on what binary serialization you want to achieve:

-  ``Data_encoding.int8``
-  ``Data_encoding.uint8``
-  ``Data_encoding.int16``
-  ``Data_encoding.uint16``
-  ``Data_encoding.int31``
-  ``Data_encoding.int32``
-  ``Data_encoding.int64``

For example, an encoding that represents a 31 bit integer has type
``Data_encoding.int31 = int Data_encoding.encoding``.

.. code:: ocaml

    let int31_encoding = Data_encoding.int31

Encoding an object
~~~~~~~~~~~~~~~~~~

Encoding a single integer is fairly uninteresting. The `Data_encoding`
library provides a number of combinators that can be used to build more
complicated objects. Consider the type that represents an interval from
the first number to the second:

.. code:: ocaml

    type interval = int64 * int64

We can define an encoding for this type as:

.. code:: ocaml

    let interval_encoding =
      Data_encoding.(obj2 (req "min" int64) (req "max" int64))

In the example above we construct a new value ``interval_encoding`` by
combining two `int64` integers using the ``obj2`` (object with two fields)
constructor.

The library provides different constructors, i.e. for objects that have
no data (``Data_encoding.empty``), constructors for object up to 10
fields, constructors for tuples, list, etc.

These are serialized to binary by converting each internal object to
binary and placing them in the order of the original object and to JSON
as a JSON object with field names.

Lists, arrays, and options
~~~~~~~~~~~~~~~~~~~~~~~~~~

List, arrays and options types can by built on top of ground data types.

.. code:: ocaml

    type interval_list = interval list

    type interval_array = interval array

    type interval_option = interval option

And the encoders for these types as

.. code:: ocaml

    let interval_list_encoding = Data_encoding.list interval_encoding
    let interval_array_encoding = Data_encoding.array interval_encoding
    let interval_option_encoding = Data_encoding.option interval_encoding

Union types
~~~~~~~~~~~

The Tezos codebase makes heavy use of variant types. Consider the
following variant type:

.. code:: ocaml

    type variant = B of bool
                 | S of string

Encoding for this types can be expressed as:

.. code:: ocaml

    let variant_encoding =
      Data_encoding.(union ~tag_size:`Uint8
                       [ case
                           bool
                           (function B b -> Some b | _ -> None)
                           (fun b -> B b) ;
                         case
                           string
                           (function S s -> Some s | _ -> None)
                           (fun s -> S s) ])

This variant encoding is a bit more complicated. Let’s look at the parts
of the encoding:

-  We include an optimization hint to the binary encoding to inform it
   of the number of elements we expect in the tag. In most cases, we can
   use :literal:`\`Uint8`, which allows you to have up to 256 possible
   cases (default).
-  We provide a function to wrap the datatype. The encoding works by
   repeatedly trying to decode the datatype using these functions until
   one returns ``Some payload``. This payload is then encoded using the
   dataencoding specified.
-  We specify a function from the encoded type to the actual datatype.

Since the library does not provide an exhaustive check on these
constructors, the user must be careful when constructing union types to
avoid unfortunate runtime failures.

How the Dataencoding module works
---------------------------------

This section is 100% optional. You do not need to understand this
section to use the library.

The library uses GADTs to provide type-safe
serialization/de-serialization. From there, a runtime representation of
JSON objects is parsed into the type-safe version.

First we define an untyped JSON AST:

.. code:: ocaml

    type json =
      [ `O of (string * json) list
      | `Bool of bool
      | `Float of float
      | `A of json list
      | `Null
      | `String of string ]

This is then parsed into a typed AST (we eliminate several cases for
clarity):

.. code:: ocaml

    type 'a desc =
      | Null : unit desc
      | Empty : unit desc
      | Bool : bool desc
      | Int64 : Int64.t desc
      | Float : float desc
      | Bytes : Kind.length -> MBytes.t desc
      | String : Kind.length -> string desc
      | String_enum : Kind.length * (string * 'a) list -> 'a desc
      | Array : 'a t -> 'a array desc
      | List : 'a t -> 'a list desc
      | Obj : 'a field -> 'a desc
      | Objs : Kind.t * 'a t * 'b t -> ('a * 'b) desc
      | Tup : 'a t -> 'a desc
      | Union : Kind.t * tag_size * 'a case list -> 'a desc
      | Mu : Kind.enum * string * ('a t -> 'a t) -> 'a desc
      | Conv :
          { proj : ('a -> 'b) ;
            inj : ('b -> 'a) ;
            encoding : 'b t ;
            schema : Json_schema.schema option } -> 'a desc
      | Describe :
          { title : string option ;
            description : string option ;
            encoding : 'a t } -> 'a desc
      | Def : { name : string ;
                encoding : 'a t } -> 'a desc

-  The first few constructors define all ground types.
-  The constructors for ``Bytes``, ``String`` and ``String_enum``
   include a length field in order to provide safe binary
   serialization.
-  The constructors for ``Array`` and ``List`` are used by the
   combinators we saw earlier.
-  The ``Obj`` and ``Objs`` constructors create JSON objects. These are
   wrapped in the ``Conv`` constructor to remove nesting that results
   when these constructors are used naively.
-  The ``Mu`` constructor is used to create self-referential
   definitions.
-  The ``Conv`` constructor allows you to clean up a nested definition
   or compute another type from an existing one.
-  The ``Describe`` and ``Def`` constructors are used to add
   documentation

The library also provides various wrappers and convenience functions to
make constructing these objects easier. Reading the documentation in the
`mli file
<../api/odoc/tezos-data-encoding/Tezos_data_encoding/Data_encoding/index.html>`__
should orient you on how to use these functions.
