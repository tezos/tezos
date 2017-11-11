.. _the_big_picture:

Tezos Software Architecture
===========================

The diagram below shows a very coarse grained architecture of Tezos.
|Tezos architecture diagram|

The characteristic that makes Tezos unique is its self-amending
property. The part that amends itself is called the *economic protocol*
(the green eye of the octopus), sometimes abbreviated by protocol or
even proto in the source code. The rest of a Tezos node is what we call
the *shell* (the blue octopus).

The protocol is responsible for interpreting the transactions and other
administrative operations. It also has the responsibility to detect
erroneous blocks.

An important thing to notice is that the protocol always sees only one
block chain. In other words, a linear sequence of blocks since the
genesis. It does not know that it lives in an open network where nodes
can propose alternative heads.

Only the shell knows about the multiple heads. It is responsible for
choosing between the various chain proposals that come from the bakers
(the programs that cook new blocks) of the network. The shell has the
responsibility of selecting and downloading alternative chains, feed
them to the protocol, which in turn has the responsibility to check them
for errors, and give them an absolute score. The shell then simply
selects the valid head of highest absolute score. This part of the shell
is called the validator.

The rest of the shell includes the peer-to-peer layer, the disk storage
of blocks, the operations to allow the node to transmit the chain data
to new nodes and the versioned state of the ledger. Inbetween the
validator, the peer-to-peer layer and the storage sits a component
called the distributed database, that abstracts the fetching and
replication of new chain data to the validator.

Protocols are compiled using a tweaked OCaml compiler (green part on the
left of the picture) that does two things. First, it checks that the
protocol’s main module has the right type. A good analogy is to see
protocol as plug-ins, and in this case, it means that it respects the
common plugin interface. Then, it restricts the typing environment of
the protocol’s code so that it only calls authorized modules and
functions. Seeing protocols as plug-ins, it means that the code only
called primitives from the plug-in API. It is a form of statically
enforced sandboxing.

Finally, the RPC layer (in yellow on the right in the picture) is an
important part of the node. It is how the client, third party
applications and daemons can interact with the node and introspect its
state. This component uses the mainstream JSON format and HTTP protocol.
It uses in-house libraries ``ocplib-resto`` and ``ocplib-json-typed``
(via the module :ref:`Data_encoding <data_encoding>`). It
is fully inter-operable, and auto descriptive, using JSON schema.

.. |Tezos architecture diagram| image:: octopus.svg

