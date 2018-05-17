# ocplib-json-typed

This library is a collection of type-aware JSON utilities for OCaml.

  - `Json_encoding` contains an `'a encoding` type that represents
    the JSON encoding of OCaml values of type `'a`, and a collection
    of combinators to build them. These encodings can be used to
    serialize / deserialize OCaml values to / from JSON
    documents. JSON schemas can also be generated automatically to
    produce documented, interoperable JSON formats.
  - `Json_schema` contains an OCaml intermediate representation for
    the JSON schema document grammar description language, along with
    translators to / from the concrete JSON schema format.
  - `Json_query` contains various utilities to manipulate, introspect
    and update JSON data.
  - `Json_repr` defines an abstraction over JSON representations.
    This module is mainly useful when using the functorial interface of
    the library, or if you use several JSON libraries in your program
    and want to convert data from one JSON representation to another.

The type of JSON documents handled by this library is directly
compatible with `ezjsonm`, but converters are provided for `yojson`
users, and an advanced functorial interface allows you to use any JSON
representation. Two other representations are also provided.

  - `Json_repr_browser` interfaces JavaScripts objects. It is
    available only when compiling to JavaScript via
    `js_of_ocaml`.
    Provided by the extra package `ocplib-json-typed-browser`.
  - `Json_repr_bson` is an implementation of a subset of BSON.
    Provided by the extra package `ocplib-json-typed-bson`.

Thanks to polymorphic variants, this library does not depend on any
JSON library, so you are free to use whichever you want for printing
and parsing.
