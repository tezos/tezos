.. _protocol_environment:

Economic protocol sandboxing
============================

In Alpha, as in any sound future protocols, updates are approved by
voting. That way, the responsibility of switching to a new protocol code
is the responsibility of voters, and one could argue that it is up to
them to check that the code does not call, for instance, unsafe array
access functions.

Yet, we decided to introduce a minimum level of machine checks, by
compiling with a specific compiler that checks that no known-unsafe
function is used. This static form of sandboxing is performed by the
OCaml typechecker: we simply compile protocols in a restricted set of
modules with restricted interfaces that hide any unsafe, non wanted
feature.

Another goal of that specific environment is maintaining a stable OCaml
API for protocol development. Imagine that at some point, the OCaml
standard library changes (a function is added or removed, a type is
changed), then we will be able to upgrade to the new OCaml while still
remaining compatible with past protocols, by providing an adapter layer.

Here is a quick description of each file in this environment:

-  Files ``array.mli``, ``buffer.mli``, ``bytes.mli``, ``format.mli``,
   ``int32.mli``, ``int64.mli``, ``list.mli``, ``map.mli``,
   ``pervasives.mli``, ``set.mli`` and ``string.mli`` are stripped down
   interfaces to the OCaml standard library modules. The removed
   elements are: effects on toplevel references or channels, unsafe
   functions, functions that are known sources of bugs, and anything
   deprecated.
-  As we removed polymorphic comparison operators, ``compare.mli``
   implements monomorphic operators for standard OCaml and Tezos types.
   An example use is ``Compare.Int.(3 = 4)`` instead of plain OCaml
   ``(3 = 4)``.
-  Files ``lwt*`` is the stripped down interface to Lwt, of which we
   removed any non deterministic functions, since we only use Lwt for
   asynchronous access to the storage.
-  Files ``data_encoding.mli``, ``error_monad.mli``, ``mBytes.mli``,
   ``hash.mli``, ``base58.mli``, ``blake2B.mli``, ``ed25519.mli``,
   ``hex_encode.mli``, ``json.mli``, ``time.mli``, ``z.mli``,
   ``micheline.mli`` and files ``RPC_*`` are stripped down versions of
   the Tezos standard library.
-  Files ``tezos_data.mli``, ``context.mli``, ``fitness.mli`` and
   ``updater.mli`` are interfaces to the shell’s data definitions and
   storage accessors that are accessible to the protocol.
