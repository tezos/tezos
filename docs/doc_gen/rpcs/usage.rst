Usage
*****

In order to interact with a Tezos node, you may use RPC calls through the
client using this command ``tezos-client rpc call <url>``.

For instance, if you wish to request the current balance of a given
block and contract, you can call the associated RPC via the command :
``$ tezos-client rpc call
/blocks/<block_id>/proto/context/contracts/<contract_id>/balance``.

A RPC may takes an *input* and generates an *output* both in JSON
format. For example, the previous RPC call, that does not require an
input, would display on the standard output : ``{ "balance":
"4000000000000" }``. When calling a RPC that requires an input
through command-line, you will be prompted to provide the JSON input
in your default configured text editor.

In the following sections, you may find the complete list of RPCs
available for the client and for the protocol Alpha. You may also
find, for each RPC, its input, output and errors in JSON schema
format. Dynamic parameters, such as ``<block-id>``, are described in
`Dynamic parameters description`_
