Flexible Network Sandboxes
==========================

Build
-----

Use:

::

    make build-sandbox

(or ``make build-test``).

There are testing-only ``opam`` dependencies: ``dum`` and ``genspio``
(``0.0.2``), make sure you have done ``make build-deps``.

Usage
-----

See ``./tezos-sandbox --help`` (or one can use
``_build/default/src/bin_flextesa/main.exe``).

When running (semi-)interactive tests, it is recommended to wrap the
call with ``rlwrap`` or ``ledit``.

Examples
--------

Mini-Network
~~~~~~~~~~~~

One can run a mini-network advancing fast with accusers, bakers, and
endorsers:

::

    rlwrap ./tezos-sandbox mini-network \
           --root-path /tmp/zz-mininet-test \
           --tezos-node-binary _build/default/src/bin_node/main.exe \
           --tezos-baker-alpha-binary _build/default/src/proto_alpha/bin_baker/main_baker_alpha.exe \
           --tezos-endorser-alpha-binary _build/default/src/proto_alpha/bin_endorser/main_endorser_alpha.exe \
           --tezos-accuser-alpha-binary _build/default/src/proto_alpha/bin_accuser/main_accuser_alpha.exe \
           --tezos-client-binary _build/default/src/bin_client/main_client.exe

Once the network is started this test scenario becomes interactive:

::

    Flextesa.mininet: Please enter command:

Just try ``h`` (or ``help``) to see the available commands.

Double Endorsement Accusation
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

There are 3 “accusation scenarios” so far, see
``./tezos-sandbox accusation --help``. For instance, the following command
starts a small 3-node network, forces one baker to endorse two
concurrent branches, and then makes another baker inject (and bake) the
double-endorsement-evidence operation. The option ``--base-port=20_000``
tells ``tezos-sandbox`` to start allocating P2P/RPC ports from 20 000 and
``--pause-at-end=true`` tells ``tezos-sandbox`` to enter an interactive
command prompt at the end of the test to give a chance to explore the
sandbox before killing all the nodes.

::

    rlwrap ./tezos-sandbox accusations simple-double-endorsing \
         --root $PWD/double-endorsing-test \
         --base-port=20_000 \
         --pause-at-end=true
         

This test among other ones can run
`Kiln <https://gitlab.com/obsidian.systems/tezos-bake-monitor/>`__
alongside the *Ꜩ-sandbox*, for instance:

::

    rlwrap ./tezos-sandbox accusations simple-double-endorsing --with-kiln

See also the options ``--kiln-*`` for configuration, and the option
``--starting-level`` (since Kiln assumes a long-running blockchain
adding more, e.g. 40, bakes at the beginning of the test brings us to a
more “normal” state).

Voting With a Ledger Nano S
~~~~~~~~~~~~~~~~~~~~~~~~~~~

    **Note:** this requires a ``tezos-client`` with the changes from
    https://gitlab.com/tezos/tezos/merge_requests/848.

The voting test for now goes up to the last block before the protocol is
supposed to change to the election winner (see also
``./tezos-sandbox voting --help``).

The test can use a Ledger Nano S as one of the voters (the test
automatically becomes **interactive** then because the user has to press
buttons on the device).

Get an URI for your ledger (the test requires both the Wallet and Baking
apps):

::

    tezos-client list connected ledgers

And use the URI (no need to import it) for the ``--with-ledger`` option:

::

    rlwrap ./tezos-sandbox voting ./src/bin_client/test/demo/ \
         --with-ledger "ledger://crouching-tiger-hidden-dragon/ed25519/0'/0'" \
         --serialize-proposals \
         --root $PWD/voting-test \
         --base-port=20_000 \
         --tezos-client-binary ../mr848/tezos-client \
         --pause-on-error=true

The test becomes interactive and guides you through the interactions
with the ledger, e.g.:

::

    Flextesa.voting:
      Ledger-prompt 
        
          Setting up "ledger://crouching-tiger-hidden-dragon/ed25519/0'/0'" for
          baking. The ledger should be showing the setup parameters (Address,
          Main chain, HWMs).
        
         Please hit “✔” on the ledger. 

Implementation Considerations
-----------------------------

``Running_processes`` is very high-level (actually agnostic to Tezos).
Most processes are actually calls to ``sh -c <script>`` where
``<script>`` is the result of a ``Genspio`` compilation, this leaves the
option to later easily run some processes over SSH (without OCaml
dependencies on the destination host) or in special containers (e.g.
``docker run --cpu-shares ...``).

The prompt commands for interactive use use ``Base.Sexp.t`` parsers
(because already a dependency, and we need a good string literal parser
so we cannot use ``Clic`` nor ``Cmdliner``).

Special Coding Style
--------------------

A fresh “just for testing project” is a good occasion to experiment a
bit …

See ``./src/lib_network_sandbox/internal_pervasives.ml``:

-  ``EF``: we try to use combinators on top of
   `Easy-format <https://mjambon.github.io/mjambon2016/easy-format.html>`__
   for most pretty-printing (it is still compatible with ``Format`` but
   it is much more functional/composable and does not rely on
   ``@[<2,3>@{crazy}@ @<acronym>EDSLs@n@]``).
-  Many standard modules are taken from Jane St Base (already a
   dependency of Tezos): List, String, Option, Int, Float.
-  Error monad uses *more typed* errors (polymorphic variants), cf.
   module ``Asynchronous_result`` (and note that ``bind`` also calls
   ``Lwt_unix.auto_yield 0.005 ()``).
-  All state is kept in a (*non-global*) value passed as argument
   everywhere needed. To simplify the dependency management the state
   variables are objects (cf. ``Base_state``, then ``Paths``,
   ``Console``, etc).

Also, everything uses OCamlFormat instead of ``ocp-indent`` (see
``./.ocamlformat``).
