.. _entering_alpha:

How to start reading protocol Alpha
===================================

Protocol Alpha, whose Alpha has nothing to do with the one in Alphanet,
is the name of the initial economic protocol. Alpha is a placeholder
name, while we decide on the naming convention for protocol versions.

Before reading that document, you may want to:

-  read the whitepaper,
-  read :ref:`how the economic protocol is
   sandboxed <protocol_environment>`.

As all protocols, Alpha is made of a series of OCaml interface and
implementation files, accompanied by a ``TEZOS_PROTOCOL`` file.

The ``TEZOS_PROTOCOL`` structure
--------------------------------

If you look at this file in the repository, you will see that it is
composed of the hash of the sources, and the list of its modules, in
linking order.

Protocol Alpha is structured as a tower of abstraction layers, a coding
discipline that we designed to have OCaml check as many invariants as
possible at typing time. You will also see empty lines in
``TEZOS_PROTOCOL`` that denote these layers of abstraction.

These layers follow the linking order: the first modules are the tower’s
foundation that talk to the raw key-value store, and going forward in
the module list means climbing up the abstraction tower.

The big abstraction barrier: ``Alpha_context``
----------------------------------------------

The proof-of-stake algorithm, as described in the white paper, relies on
an abstract state of the ledger, that is read and transformed during
validation of a block.

Due to the polymorphic nature of Tezos, the ledger’s state (that we call
**context** in the code), cannot be specific to protocol Alpha’s need.
The proof-of-stake is thus implemented over a generic key-value store
whose keys and associated binary data must implement the abstract
structure.

The ``Alpha_context`` module enforces the separation of concerns
between, on one hand, mapping the abstract state of the ledger to the
concrete structure of the key-value store, and, on the other hand,
implementing the proof-of-stake algorithm over this state.

In more practical terms, ``Alpha_context`` defines a type ``t`` that
represents a state of the ledger. This state is an abstracted out
version of the key-value store that can only be manipulated through the
use of the few selected manipulations reexported by ``Alpha_context``,
that always preserve the well-typed aspect and internal consistency
invariants of the state.

When validating a block, the low-level state that result from the
predecessor block is read from the disk, then abstracted out to a
``Alpha_context.t``, which is then only updated by high level operations
that preserve consistency, and finally, the low level state is extracted
to be committed on disk.

This way, we have two well separated parts in the code. The code below
``Alpha_context`` implements the ledger’s state storage, while the code
on top of it is the proof-of-stake algorithm. Thanks to this barrier,
the latter can remain nice, readable OCaml that only manipulates plain
OCaml values.

Below the ``Alpha_context``
---------------------------

For this part, in a first discovery of the source code, you can start by
relying mostly on this coarse grained description, with a little bit of
cherry-picking when you’re curious about how a specific invariant is
enforced.

The ``*_repr`` modules
~~~~~~~~~~~~~~~~~~~~~~

These modules abstract the values of the raw key-value context by using
:ref:`Data_encoding<data_encoding>`.

These modules define the data types used by the protocol that need to be
serialized (amounts, contract handles, script expressions, etc.). For
each type, it also defines its serialization format using
:ref:`Data_encoding<data_encoding>`.

Above this layer, the code should never see the byte sequences in the
database, the ones of transmitted blocks and operations, or the raw JSON
of data transmitted via RPCs. It only manipulates OCaml values.

The ``Storage`` module and storage functors
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Even with the concrete formats of values in the context abstracted out,
type (or consistency) errors can still occur if the code accesses a
value with a wrong key, or a key bound to another value. The next
abstraction barrier is a remedy to that.

The storage module is the single place in the protocol where key
literals are defined. Hence, it is the only module necessary to audit,
to know that the keys are not colliding.

It also abstracts the keys, so that each kind of key get its own
accessors. For instance, module ``Storage.Contract.Balance`` contains
accessors specific to contracts’ balances.

Moreover, the keys bear the type of the values they point to. For
instance, only values of type ``Tez_repr.t`` can by stored at keys
``Storage.Contract.Balance``. And in case a key is not a global key, but
a parametric one, this key is parameterized by an OCaml value, and not the
raw key part.

So in the end, the only way to be used when accessing a contract balance
is ``Storage.Contract.Balance.get``, which takes a ``Contract_repr.t``
and gives a ``Tez_repr.t``.

All these well-typed operations are generated by a set of functors, that
come just before ``Storage`` in ``TEZOS_CONTEXT``.

The ``*_storage`` modules
~~~~~~~~~~~~~~~~~~~~~~~~~

The two previous steps ensure that the ledger’s state is always accessed
and updated in a well-typed way.

However, it does not enforce that, for instance, when a contract is
deleted, all of the keys that store its state in the context are indeed
deleted.

This last series of modules named ``*_storage`` is there to enforce just
that kind of invariants: ensuring the internal consistency of the
context structure.

These transaction do not go as far as checking that, for instance, when
the destination of a transaction is credited, the source is also
debited, as in some cases, it might not be the case.

Above the ``Alpha_context``
---------------------------

The three next sections describe the main entrypoints to the protocol:
validation of blocks by the shell (that we often also call application),
smart contracts, and RPC services.

The ``Main`` module is the entrypoint that’s used by the shell. It
respects the module type that all protocol must follow. For that, its
code is mostly plumbing,

Starting from ``Apply``
~~~~~~~~~~~~~~~~~~~~~~~

This is were you want to start on your first read. Even if some plumbing
code is woven in, such as error cases declaration and registration, most
of the proof-of-stake code has been written in a verbose style, to be
understood with minimum OCaml knowledge.

You want to start from the shell entry points (validation of the block
header, validation of an operation, finalization of a block validation),
and follow the control flow until you hit the ``Alpha_context``
abstraction barrier. This will lead you to reading modules ``Baking``
and ``Amendment``.

Smart contracts
~~~~~~~~~~~~~~~

From ``Apply``, you will also end up in modules ``Script_ir_translator``
and ``Script_interpreter``. The former is the typechecker of Michelson
that is called when creating a new smart contract, and the latter is the
interpreter that is called when transferring tokens to a new smart
contract.

Protocol RPC API
~~~~~~~~~~~~~~~~

Finally, the RPCs specific to Alpha are also defined above the
``Alpha_context`` barrier.

Services are defined in a few modules, divided by theme. Each module
defines the RPC API: URL schemes with the types of parameters, and
input and output JSON schemas. This interface serves three
purposes. As it is thoroughly typed, it makes sure that the handlers
(that are registered in the same file) have the right input and output
types. It is also used by the client to perform RPC calls, to make
sure that the URL schemes and JSON formats and consistent between the
two parties. These two features are extremely useful when refactoring,
as the OCaml typechecker will help us track the effects of an RPC API
change on the whole codebase. The third purpose is of course, to make
automatic documentation generation possible (as in ``tezos client rpc
list/format``). Each service is also accompanied by a caller function,
that can be used from the client to perform the calls, and by the
tests to simulate calls in a fake in-memory context.

It can be useful if you are a third party developer who wants to read
the OCaml definition of the service hierarchy directly, instead of the
automatically generated JSON hierarchy.
