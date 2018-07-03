.. _howto:

How to build and run
====================

Get the sources
---------------

Tezos *git* repository is hosted at `GitLab
<https://gitlab.com/tezos/tezos/>`_. All development happens here. Do
**not** use our `GitHub mirror <https://github.com/tezos/tezos>`_
which we don't use anymore and only mirrors what happens at GitLab.

You also need to **choose a branch**:

- The *master* branch is where code is merged, but there is no test
  network using the *master* branch directly.
- The *alphanet* and *alphanet-lmdb* is what you want to use if you want
  to connect to Tezos' test network, the *Alphanet*. The
  *-lmdb* version uses LMDB instead of LevelDB.

**TL;DR**: Typically you want to do:

::

   git clone https://gitlab.com/tezos/tezos.git
   git checkout alphanet

Install OPAM
------------

To compile Tezos, you need an OCaml compiler (version 4.06.1) and all
the libraries listed in the various ``tezos-*.opam`` files.

The simplest way to install all dependencies is by using `OPAM
<https://opam.ocaml.org/>`__, the OCaml package manager.


**IMPORTANT**: Please use `version 2
<https://opam.ocaml.org/blog/opam-2-0-0-rc3/>`_ of OPAM. That
is what the Tezos Core team uses. Most distribution probably ship
**version 1** of OPAM out of the box, but installing version 2 is
preferable for many reasons.


Install Tezos dependencies with OPAM
------------------------------------

Install the OCaml compiler and the libraries which Tezos depends on:

::

    make build-deps

While building the dependencies, ``opam`` is able to handle correctly
the OCaml libraries but it is not always able to handle all external C
libraries we depend on. On most system, it is able to suggest a call to
the system package manager but it currently does not handle version
check.

At last, compile the project:

::

    make

This should produce three binaries:

-  ``tezos-node``: the tezos daemon itself;
-  ``tezos-client``: a command-line client;
-  ``tezos-admin-client``: a command-line administration tool for the node;
-  ``tezos-alpha-baker``: a client and daemon to bake on the Tezos network;
-  ``tezos-protocol-compiler``: a protocol compiler used for developing
   new version of the economic protocol.

Currently Tezos is being developed for Linux only. It should work on
macOS, but it has not been tested recently. A Windows port is feasible
and might be developed in the future.

Note that, when executing ``make build-deps``, OPAM will detect if
required system dependencies are installed. However, it is not able to
detect which versions you actually have.


Join the Alphanet!
------------------

If you succesfully built Tezos on the *alphanet* or *alphanet-lmdb*
branch, then your node is elligible to join Tezos'
:ref:`Alphanet<alphanet>`.

Command-line basics
~~~~~~~~~~~~~~~~~~~

The `tezos-node` executable uses subcommands. You can obtain help on a
subcommand by using `./tezos-node <subcommand> --help`. There are
three subcommands:

::

   ./tezos-node identity --help
   ./tezos-node config --help
   ./tezos-node run --help


The `identity` and `config` serve the purpose of managing
configuration files for the node, we will describe them below. The
`run` command is for running the node.

Pretty much all configuration parameters can be overriden by a
command-line argument. Check out `./tezos-node run --help` to discover
them.

Configure your node
~~~~~~~~~~~~~~~~~~~

The following steps are required to connect to Alphanet.

::

    ./tezos-node identity generate

This will generate a new node identity and compute the associated
stamp of proof-of-work. The identity comprises a pair of cryptographic
keys that nodes use to encrypt messages sent to each other, and an
antispam-PoW stamp proving that enough computing power has been
dedicated to creating this identity.

The identity will be stored in `$HOME/.tezos-node/identity.json`.

::

   ./tezos-node config init

This will initialize an configuration file for the node in
`$HOME/.tezos-node/config.json`, using default values. It only
specifies that the node will listen to incoming connections on socket
address ``[::]:9732``.

The easiest way to amend this default configuration is to use

::

   # Update the config file
   ./tezos-node config update <…>
   # Start from an empty cfg file
   ./tezos-node config reset <…>


All blockchain data is stored under ``$HOME/.tezos-node/``.  You can
change this by doing `./tezos-node config update --data-dir
</somewhere/in/your/disk>`.

To run multiple nodes on the same machine, you can duplicate and edit
``$HOME/.tezos-node/config.json`` while making sure they don't share
the same ``data-dir``. Then run your node with `./tezos-node
run --config-file=</path/to/alternate_cfg>`.

Lastly, you want to enable RPC communication with clients. Use:

::

   ./tezos-node config update --rpc-addr=127.0.0.1:8732

This is the default socket address that the client will try, so
`./tezos-client` will work out-of-the-box that way.

Run your node
~~~~~~~~~~~~~

You are all set! Now you just need to do:

::

   ./tezos-node run

To interact with your node, read the doc of clients:

::

   ./tezos-client man
   ./tezos-admin-client man
   ./tezos-alpha-baker man

And read :ref:`this section<faucet>` to learn how to get alphanet tezzies.

Use sandboxed mode
------------------

To run a ‘localhost-only’ instance of a Tezos network, we provide two
helper scripts:

-  ``./src/bin_node/tezos-sandboxed-node.sh``
-  ``./src/bin_client/tezos-init-sandboxed-client.sh``

Run a sandboxed node
~~~~~~~~~~~~~~~~~~~~

For instance, if you want to run local network with two nodes, in a
first terminal, the following command will initialize a node listening
for peers on port ``19731`` and listening for RPC on port ``18731``.

::

    ./src/bin_node/tezos-sandboxed-node.sh 1

This node will store its data in a temporary directory which will be
removed when the node is killed.

To launch the second node, just run the following command, it will
listen on port ``19739`` and ``18739``:

::

    ./src/bin_node/tezos-sandboxed-node.sh 9

You might replace ``1`` or ``9`` by any number in between if you want to
run more than two nodes. But, if you intend to run a single node
network, you might remove the spurious “Too few connections” warnings by
lowering the number of expected connection, by running the following
command instead:

::

    ./src/bin_node/tezos-sandboxed-node.sh 1 --connections 0

Use the sandboxed client
~~~~~~~~~~~~~~~~~~~~~~~~

Once your node(s) is/are running, open a new terminal and initialize the
“sandboxed” client data:

::

    eval `./src/bin_client/tezos-init-sandboxed-client.sh 1`

It will initialize the client data in a temporary directory. It will
also defines in the current shell session an alias ``tezos-client``
preconfigured for communicating the same-numbered node. For instance:

::

    $ tezos-client rpc get /chains/main/blocks/head/hash
    { "hash": "BLockGenesisGenesisGenesisGenesisGenesisGeneskvg68z" }

When you bootstrap a new network, the network is initialized with a
dummy economic protocol, called “genesis”. If you want to run the same
protocol than the alphanet, ``init-sandboxed-client`` also defines an
alias ``tezos-activate-alpha``, that you need to execute once for
activating the whole network. For instance:

::

    $ tezos-client rpc get /chains/main/blocks/head/metadata/next_protocol_hash
    { "protocol": "Ps9mPmXaRzmzk35gbAYNCAw6UXdE2qoABTHbN2oEEc1qM7CwT9P" }
    $ tezos-activate-alpha
    Injected BMBcK869jaHQDc
    $ tezos-client rpc get /chains/main/blocks/head/metadata/next_protocol_hash
    { "protocol": "PtCJ7pwoxe8JasnHY8YonnLYjcVHmhiARPJvqcC6VfHT5s8k8sY" }

Tune protocol alpha parameters
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The ``tezos-active-alpha`` alias use parameters from
``scripts/protocol_parameters.json`` to activate protocol alpha. It can
be useful to tune these parameters when you need to debug something,
for example, change the number of blocks per cycle, the time between
blocks, etc.

Configuration options
---------------------

Here is an example configuration file with all parameters specified.
Most of the time it uses default values, except for cases where the
default is not explanatory enough (i.e. “bootstrap-peers” is an empty
list by default). Comments are not allowed in JSON, so this
configuration file would not parse. They are just provided here to help
writing your own configuration file if needed.

::

    {

      /* Location of the data dir on disk. */

      "data-dir": "/home/tezos/my_data_dir"

      /* Configuration of net parameters */

      "net": {

        /* Floating point number between 0 and 256 that represents a
        difficulty, 24 signifies for example that at least 24 leading
        zeroes are expected in the hash. */

        "expected-proof-of-work": 24.5,

        /* List of hosts. Tezos can connect to both IPv6 and IPv4
        hosts. If the port is not specified, default port 9732 will be
        assumed. */

        "bootstrap-peers": ["::1:10732", "::ffff:192.168.1.3:9733", "mynode.tezos.com"],

        /* Specify if the node is in private mode or not. A node in
        private mode only opens outgoing connections to peers whose
        addresses are in [trusted_peers] and only accepts incoming
        connections from trusted peers. In addition, it informs these
        peers that the identity of the node should not be revealed to
        the rest of the network. */

        "private-mode": false,

        /* Network limits */

        "limits": {

          /* Delay granted to a peer to perform authentication, in
          seconds. */

          "authentication-timeout": 5,

          /* Strict minimum number of connections (triggers an urgent
          maintenance). */

          "min-connections": 50,

          /* Targeted number of connections to reach when bootstrapping /
          maintaining. */

          "expected-connections": 100,

          /* Maximum number of connections (exceeding peers are
          disconnected). */

          "max-connections": 200,

          /* Number above which pending incoming connections are
          immediately rejected. */

          "backlog": 20,

          /* Maximum allowed number of incoming connections that are
          pending authentication. */

          "max-incoming-connections": 20,

          /* Max download and upload speeds in KiB/s. */

          "max-download-speed": 1024,
          "max-upload-speed": 1024,

          /* Size of the buffer passed to read(2). */

          "read-buffer-size": 16384,
        }
      },

      /* Configuration of rpc parameters */

      "rpc": {

        /* Host to listen to. If the port is not specified, the default
        port 8732 will be assumed. */

        "listen-addr": "localhost:8733",

        /* Cross Origin Resource Sharing parameters, see
        https://en.wikipedia.org/wiki/Cross-origin_resource_sharing. */

        "cors-origin": [],
        "cors-headers": [],

        /* Certificate and key files (necessary when TLS is used). */

        "crt": "tezos-node.crt",
        "key": "tezos-node.key"
      },

      /* Configuration of log parameters */

      "log": {

        /* Output for the logging function. Either "stdout", "stderr" or
        the name of a log file . */

        "output": "tezos-node.log",

        /* Verbosity level: one of 'fatal', 'error', 'warn', 'notice',
        'info', 'debug'. */

        "level": "info",

        /* Fine-grained logging instructions. Same format as described in
        `tezos-node run --help`, DEBUG section. In the example below,
        sections "net" and all sections starting by "client" will have
        their messages logged up to the debug level, whereas the rest of
        log sections will be logged up to the notice level. */

        "rules": "client* -> debug, net -> debug, * -> notice",

        /* Format for the log file, see
        http://ocsigen.org/lwt/dev/api/Lwt_log_core#2_Logtemplates. */

        "template": "$(date) - $(section): $(message)"
      },

      /* Configuration for the validator and mempool parameters */

      "shell": {

         /* The number of peers to synchronize with
            before declaring the node 'bootstrapped'. */

         "bootstrap_threshold": 4

      }
    }

Debugging
---------

It is possible to set independent log levels for different logging
sections in Tezos, as well as specifying an output file for logging. See
the description of log parameters above as well as documentation under
the DEBUG section displayed by `tezos-node run –-help`.

JSON/RPC interface
------------------

The Tezos node provides a JSON/RPC interface. Note that it is an RPC,
and it is JSON based, but it does not follow the “JSON-RPC” protocol. It
is not active by default and it must be explicitly activated with the
``--rpc-addr`` option. Typically, if you are not trying to run a local
network and just want to explore the RPC, you would run:

::

    ./tezos-node run --rpc-addr localhost

The RPC interface is self-documented and the ``tezos-client`` executable
is able to pretty-print the RPC API. For instance, to see the API
provided by the Tezos Shell:

::

    ./tezos-client rpc list

To get API attached to the “genesis” block, including the remote
procedures provided by the associated economic protocol version:

::

    ./tezos-client rpc list /blocks/genesis/

You might also want the JSON schema describing the expected input and
output of a RPC. For instance:

::

    ./tezos-client rpc schema /blocks/genesis/hash

Note: you can get the same information, but as a raw JSON object, with a
simple HTTP request:

::

    wget --post-data '{ "recursive": true }' -O - http://localhost:8732/describe
    wget --post-data '{ "recursive": true }' -O - http://localhost:8732/describe/blocks/genesis
    wget -O - http://localhost:8732/describe/blocks/genesis/hash

The minimal CLI client
----------------------

Tezos is distributed with two command line tools: a minimal command
line wallet ``tezos-client``, and an administration tool
``tezos-admin-client``.

Their command line interfaces are described
:ref:`here<tezos_client_commands>` and
:ref:`here<tezos_admin_client_commands>`.
