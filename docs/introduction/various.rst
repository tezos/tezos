Various
=======

.. _activate_fundraiser_account:

Activate fundraiser account - Mainnet
-------------------------------------

If you took part in the fundraiser you can activate your account for
the Mainnet on https://check.tezos.com/.
This feature is also included in some wallets.
If you have any questions or issues, refer to that page or to the `Tezos
foundation <https://tezos.foundation/>`_ for support.

You may also use ``tezos-client`` to activate your account, **be
warned that you should have a very good understanding of key
management in Tezos and be familiar with the command-line.**
The first step is to recover your private key using the following
command which will ask for:

- the email address used during the fundraiser
- the 14 words mnemonic of your paper wallet
- the password used to protect the paper wallet

::

   tezos-client import fundraiser key alice

Once you insert all the required information, the client computes
your secret key and it asks to create a new password to store your
secret key on disk encrypted.

If you haven't already activated your account on the website, you can
use this command with the activation code obtained from the Tezos
foundation.

::

   tezos-client activate fundraiser account alice with <code>

Like explained above, your keys are stored under ``~/.tezos-client``.
We strongly advice you to first **make a backup** and then
transfer your tokens to a new pair of keys imported from a ledger (see
:ref:`ledger`).

Check the balance with:

::

    tezos-client get balance for alice


.. _tezos-admin-client:

Admin Client
------------

The admin client enables you to interact with the peer-to-peer layer in order
to:

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

The preferred way to set up your Ledger is to install `Ledger
Live
<https://shop.ledger.com/pages/ledger-live>`_.
On Linux make sure you correctly set up your `udev` rules as explained
`here <https://github.com/obsidiansystems/ledger-app-tezos#udev-rules-linux-only>`_.
Connect your ledger, unlock it and go the dashboard.
In Ledger Live `install Tezos Wallet` from the applications list and open it on the
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

In Ledger Live there is also a `Tezos Baking` app which allows a
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


.. _private-mode:

Private node
------------

The node can be set in private mode with the option ``--private-mode``
so that:

- it doesn't connects to any peer other than those provided with
  ``--peer`` or in bootstrap-peers
- the peers connected to a private node don't include it in the list
  of peers sent to their neighborhood

This feature is especially useful to hide a sensitive node that signs
operations.

For example we could have a set up with two nodes, a private one
connected uniquely with a public one.
The public node runs on a VPS, connects normally to the network and
keeps an up to date state of the network while the private node runs at
your home and is in charge of injecting and signing operations with a
hardware wallet.

::

   tezos-node run --rpc-addr [::] --private-mode \
                                  --no-bootstrap-peers \
                                  --bootstrap-threshold=1 \
                                  --connections 1 \
                                  --peer <public-node-ip>


.. _signer:

Signer
------

Another solution to decouple the node from the signing process is to
use the *remote signer*.
Among the signing scheme supported by the client, that we can list
with ``tezos-client list signing schemes``, there are ``unix``,
``tcp``, ``http`` and ``https``.
These schemes send signing requests over their respective
communication channel towards the ``tezos-signer``, which can run on a
different machine that stores the secret key.

In our home server we can generate a new key pair (or import one from a
:ref:`Ledger<ledger>`) and launch a signer that signs operations using these
keys.
The new keys are store in ``$HOME/.tezos-signer`` in the same format
as ``tezos-client``.
On our internet facing vps we can then import a key with the address
of the signer.

::

   home~$ tezos-signer gen keys alice
   home~$ cat ~/.tezos-signer/public_key_hashs
   [ { "name": "alice", "value": "tz1abc..." } ]
   home~$ tezos-signer launch socket signer -a home-ip

   vps~$ tezos-client import secret key alice tcp://home-ip:7732/tz1abc...

Every time the client on *vps* needs to sign an operation for
*alice*, it sends a signature request to the remote signer on
*home*.
Note that this setup alone is not secure, **the signer accepts
requests from anybody and happily signs any transaction!**

Secure the connection
~~~~~~~~~~~~~~~~~~~~~

Improving the security of the communication channel can be done at the
system level, setting up a tunnel with ``ssh`` or ``wireguard``
between *home* and *vps*, otherwise the signer already provides an
additional protection.

With the option ``--require-authentication`` the signer requires the
client to authenticate before signing any operation.
First we create a new key on the *vps* and then import it as an
authorized key on *home* where it is stored under
``.tezos-signer/authorized_keys`` (similarly to ``ssh``).
Note that this key is only used to authenticate the client to the
signer and it is not used as a Tezos account.

::

   vps~$ tezos-client gen keys vps
   vps~$ cat ~/.tezos-client/public_keys
   [ { "name": "vps",
       "value":
          "unencrypted:edpk123456789" } ]

   home~$ tezos-signer add authorized key edpk123456789 --name vps
   home~$ tezos-signer --require-authentication launch socket signer -a home-ip

All request are now signed with the *vps* key thus you are
guaranteed authenticity and integrity.
This set up **does not guarantee confidentiality**, an evesdropper can
see the transactions that you sign but on a public blockchain this is
less of a concern.
You can still use the ``https`` scheme or the tunnel to encrypt your
traffic.


.. _sandboxed-mode:

Use sandboxed mode
------------------

To run a ‘localhost-only’ instance of a Tezos network, we provide two
helper scripts:

-  ``./src/bin_node/tezos-sandboxed-node.sh``
-  ``./src/bin_client/tezos-init-sandboxed-client.sh``


Run a sandboxed node
~~~~~~~~~~~~~~~~~~~~

For instance, if you want to run a local network with two nodes, in the
first terminal, the following command will initialize a node listening
for peers on port ``19731`` and listening for RPC on port ``18731``.

::

    ./src/bin_node/tezos-sandboxed-node.sh 1 --connections 1

This node will store its data in a temporary directory
``/tmp/tezos-node.xxxxxxxx`` which will be removed when the node is
stopped.
The option ``--connections`` is just to remove the spurious “Too few
connections” warnings by lowering the number of expected connection.

To launch the second node, run the following command in another terminal, and
it will listen on port ``19739`` and ``18739``:

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

It will also define in the current shell session an alias ``tezos-client``
preconfigured for communicating with the same-numbered node.

When you bootstrap a new network, the network is initialized with a
dummy economic protocol, called `genesis`. If you want to run the same
protocol as the alphanet, ``init-sandboxed-client`` also defines an
alias ``tezos-activate-alpha``, that you need to execute once for
activating the whole network.
For instance:

::

    $ tezos-client rpc get /chains/main/blocks/head/metadata
      "next_protocol": "Ps9mPmXaRzmzk35gbAYNCAw6UXdE2qoABTHbN2oEEc1qM7CwT9P"
    $ tezos-activate-alpha
      Injected BMV9KnSPE1yw
    $ tezos-client rpc get /chains/main/blocks/head/metadata
      "protocol": "Ps9mPmXaRzmzk35gbAYNCAw6UXdE2qoABTHbN2oEEc1qM7CwT9P"

We now have the possibility to send transactions to the sandboxed network.
As the genesis block used to initialize the sandboxed network differs from the
one used in Alphanet and Zeronet, it is not possible to activate
accounts obtained from the faucet. However, we can use the
preconfigured accounts which can be listed with:

::

   $ tezos-client list known addresses

     activator: tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV (unencrypted sk known)
     bootstrap5: tz1ddb9NMYHZi5UzPdzTZMYQQZoMub195zgv (unencrypted sk known)
     bootstrap4: tz1b7tUupMgCNw2cCLpKTkSD1NZzB5TkP2sv (unencrypted sk known)
     bootstrap3: tz1faswCTDciRzE4oJ9jn2Vm2dvjeyA9fUzU (unencrypted sk known)
     bootstrap2: tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN (unencrypted sk known)
     bootstrap1: tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx (unencrypted sk known)

We can run the following command to transfer some Tez from one account to
another:

::

   $ tezos-client transfer 42 from bootstrap1 to bootstrap2 &
   ...
   Waiting for the operation to be included...

You will notice that this command doesn't terminate (hence the ``&``),
as usual it is waiting for the network to include the transaction in a
block.
Given that we are in a sandbox we need to bake a block ourselves and
we can do so with the following command:

::

   $ tezos-client bake for bootstrap1

If the previous transaction is valid, the operation is included in the
chain and the transfer terminates returning the usual ticket.
Note that the ``bake for`` command of the client is exclusively for
testing purposes, all baking should be done using the ``tezos-baker``
binary.


Tune protocol alpha parameters
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The ``tezos-active-alpha`` alias uses parameters from
``scripts/protocol_parameters.json`` to activate protocol alpha. It can
be useful to tune these parameters when you need to debug something,
for example, change the number of blocks per cycle, the time between
blocks, etc.


Preserve data
~~~~~~~~~~~~~

If you want to preserve data and configuration files at the end of your run, you
can use the `DATA_DIR` environment variable.

::

    mkdir /tmp/tz-data
    DATA_DIR='/tmp/tz-data' ./src/bin_node/tezos-sandboxed-node.sh 1 --connections 1

You can even provide a custom `identity.json` and `config.json` to the
sandboxed node by placing them in the data directory.


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


Environment for writing Michelson contracts
-------------------------------------------

Here is how to setup a practical environment for
writing, editing and debugging Michelson programs.

Install `Emacs <https://www.gnu.org/software/emacs/>`_ with
the `deferred <https://github.com/kiwanami/emacs-deferred>`_ and
`exec-path-from-shell
<https://github.com/purcell/exec-path-from-shell>`_ packages.
The packages can be installed from within Emacs with
``M-x package-install``.
The last package imports the shell path in Emacs and it is needed
because we will run a sandboxed node.

Set up the `Michelson mode
<https://gitlab.com/tezos/tezos/tree/master/emacs>`_ by adding in
your ``.emacs`` :

::

   (load "~/tezos/tezos/emacs/michelson-mode.el" nil t)
   (setq michelson-client-command "tezos-client")
   (setq michelson-alphanet nil)

Note that the Michelson mode will be chosen automatically by Emacs for
files with a ``.tz`` or ``.tez`` extension.

Run a :ref:`sandboxed node<sandboxed-mode>` (and activate the alphanet
protocol with ``tezos-activate-alpha``) so that useful information
about the program can be displayed.
We can now open our favourite contract ``emacs
./src/bin_client/test/contracts/id.tz`` and, when moving the cursor on
a Michelson instruction, in the bottom of the windows Emacs should
display the state of the stack before (left) and after (right) the
application of the instruction.
The Emacs mode automatically type-checks your program and reports
errors; once you are happy with the result you can ask the client to
run it locally:

::

   tezos-client run script ./src/bin_client/test/contracts/id.tz \
                on storage '"hello"' and input '"world"'


Debugging
---------

It is possible to set independent log levels for different logging
sections in Tezos, as well as specifying an output file for logging. See
the description of log parameters above as well as documentation under
the DEBUG section displayed by `tezos-node run –-help`.
