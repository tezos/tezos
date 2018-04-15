This document contains the list of RPC services provided by the Tezos
node. It is generated from the OCaml source code (master branch).

Usage
*****

In order to interact with a Tezos node, you may use RPC calls through the
client using this command ``tezos-admin-client rpc (get|post) <url>``.

For instance, if you wish to request the current balance of a given
block and contract, you can call the associated RPC via the command :
``$ tezos-admin-client rpc post
/blocks/<block_id>/proto/context/contracts/<contract_id>/balance``.

A RPC may takes an *input* and generates an *output* both in JSON
format. For example, the previous RPC call, that does not require an
input, would display on the standard output : ``{ "balance":
"4000000000000" }``. When calling a RPC that requires an input
through command-line, you will be prompted to provide the JSON input
in your default configured text editor. Alternatively, you can provide
the JSON input using command
``$ tezos-admin-client rpc post <url> with <JSON>``. Don't forget to quote
the JSON according to your shell rules.

You can also obtain the list of RPCs on the command line with
``tezos-admin-client rpc list /``, and the description of each service
using ``tezos-admin-client rpc format <url>``.

Of course, you can use your standard HTTP tool or library as well to
perform all these tasks.
