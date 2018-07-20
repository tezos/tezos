Various
=======

.. _tezos-admin-client:

Admin Client
------------

The admin client gives access to more commands to interact with the
peer-to-peer layer in order to:

- check the status of the connections
- force connections to known peers
- ban/unban peers

A useful command to debug a node that is not syncing is:

::

   tezos-admin-client p2p stat


.. _ledger:

Ledger support
--------------

**Disclaimer:** Ledger support is still in development, **the current app
doesn't show all the needed information** for signing securely.
Check frequently for updates.

It is possible and advised to use a hardware wallet to manage your
keys, Tezos' client supports the Ledger Nano S provided that you have
the Tezos app installed.
The app is developed by Obsidian Systems and they provide a comprehensive
`tutorial on how to install it.
<https://github.com/obsidiansystems/ledger-app-tezos>`_

Ledger Manager
~~~~~~~~~~~~~~

The preferred way to set up your Ledger is to install the `Ledger
Manager extension
<https://chrome.google.com/webstore/detail/ledger-manager/beimhnaefocolcplfimocfiaiefpkgbf>`_
on a Chrome browser.
On Linux makes sure you correctly set up your `udev` rules as explained
`here <https://github.com/obsidiansystems/ledger-app-tezos#udev-rules-linux-only>`_.
Connect your ledger, unlock it and go the dashboard.
In the Ledger Manager enable `developer items` in the bottom right,
install `Tezos Wallet` from the applications list and open it on the
device.


Tezos Wallet app
~~~~~~~~~~~~~~~~

Now on the client we can import the keys (make sure the device is
in the Tezos Wallet app):

::

   ./tezos-client list connected ledgers

You can follow the instructions to import the ledger private key and
you can choose between the root or a derived address.
We can confirm the addition by listing known addresses.

::

   ./tezos-client import secret key my_ledger ledger://tz1XXXXXXXXXX
   ./tezos-client list known addresses

Optional: we can check that our ledger signs correctly using the
following command and confirming on the device:

::

   tezos-client show ledger path ledger://tz1XXXXXXXXXX

The address can now be used as any other with the exception that
during an operation the device will prompt you to confirm when it's
time to sign an operation.


Tezos Baking app
~~~~~~~~~~~~~~~~

In Ledger Manager there is also a `Tezos Baking` app which allows a
delegate to sign non-interactively e.g. there is no need to
manually sign every block or endorsement.
The application however is restricted to sign exclusively blocks and
endorsement operations; it is not possible to sign for example a
transfer.
Furthermore the application keeps track of the last level baked and allows
only to bake for increasing levels.
This prevents signing blocks at levels below the latest
block signed.

If you have tried the app on Alphanet or Zeronet and want to change
network you might need to reset this level with the command:

::

   tezos-client set ledger high watermark for ledger://tz1XXXXXXXXXX to 0


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

    ./src/bin_node/tezos-sandboxed-node.sh 1 --connections 1

This node will store its data in a temporary directory
``/tmp/tezos-node.xxxxxxxx`` which will be removed when the node is
stopped.
The option ``--connections`` is just to remove the spurious “Too few
connections” warnings by lowering the number of expected connection.

To launch the second node, just run the following command, it will
listen on port ``19739`` and ``18739``:

::

    ./src/bin_node/tezos-sandboxed-node.sh 9 --connections 1

You might replace ``1`` or ``9`` by any number in between if you want to
run more than two nodes.


Use the sandboxed client
~~~~~~~~~~~~~~~~~~~~~~~~

Once your node is running, open a new terminal and initialize the
“sandboxed” client data in a temporary directory:

::

    eval `./src/bin_client/tezos-init-sandboxed-client.sh 1`

It also define in the current shell session an alias ``tezos-client``
preconfigured for communicating with the same-numbered node.

When you bootstrap a new network, the network is initialized with a
dummy economic protocol, called `genesis`. If you want to run the same
protocol than the alphanet, ``init-sandboxed-client`` also defines an
alias ``tezos-activate-alpha``, that you need to execute once for
activating the whole network.
For instance:

::

    $ tezos-client rpc get /chains/main/blocks/head/metadata
      "next_protocol": "Ps9mPmXaRzmzk35gbAYNCAw6UXdE2qoABTHbN2oEEc1qM7CwT9P"
    $ tezos-activate-alpha
      Injected BMV9KnSPE1yw
    $ tezos-client rpc get /chains/main/blocks/head/metadata/next_protocol_hash
      "protocol": "Ps9mPmXaRzmzk35gbAYNCAw6UXdE2qoABTHbN2oEEc1qM7CwT9P"

Tune protocol alpha parameters
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The ``tezos-active-alpha`` alias use parameters from
``scripts/protocol_parameters.json`` to activate protocol alpha. It can
be useful to tune these parameters when you need to debug something,
for example, change the number of blocks per cycle, the time between
blocks, etc.


.. _node-conf:

Configuration options for the node
----------------------------------

::

   ./tezos-node config init

This will initialize a configuration file for the node in
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
