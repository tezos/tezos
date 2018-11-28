.. _rpc:


JSON/RPC interface
==================

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

    ./tezos-client rpc list /chains/main/blocks/genesis

You might also want the JSON schema describing the expected input and
output of a RPC. For instance:

::

    ./tezos-client rpc schema get /chains/main/blocks/genesis/hash

Note: you can get the same information, but as a raw JSON object, with a
simple HTTP request:

::

   curl -s localhost:8732/chains/main/blocks/head~10
   wget -O - http://localhost:8732/describe?recurse=true
   wget -O - http://localhost:8732/describe/chains/main/blocks/genesis?recurse=true
   wget -O - http://localhost:8732/describe/chains/main/blocks/genesis/hash


An online :ref:`index <rpc_index>` of RPC calls is also available.

The general call of an RPC from the client is ``tezos-admin-client rpc
(get|post) <url>``.
For instance, if you wish to request the current balance of a given
block and contract, you can call the associated RPC via the command :
``$ tezos-admin-client rpc get
/blocks/<block_id>/proto/context/contracts/<contract_id>/balance``.

An RPC may take an *input* and generate an *output* both in JSON
format. For example, the previous RPC call, that does not require an
input, would display on the standard output : ``{ "balance":
"4000000000000" }``. When calling a RPC that requires an input
through command-line, you will be prompted to provide the JSON input
in your default configured text editor. Alternatively, you can provide
the JSON input using command
``$ tezos-admin-client rpc post <url> with <JSON>``. Don't forget to quote
the JSON according to your shell rules.

If you want to learn more about the exchange of RPCs between node and
client you can pass the option `-l` and the client will print all the
calls with their input/output.

A useful util to manipulate JSON is `jq <https://stedolan.github.io/jq/>`_.
