.. _mainnet-april:

Mainnet April
=============

As announced on Nomadic Labs' blog, the April release contains many
improvements and fixes for the Tezos shell and its baking daemons.
The release is available as usual in the Git branch or in the Docker
tag `mainnet`.

Enable the testchain
--------------------

When the node is started with the new option ``--enable-testchain``,
it follows both the mainchain and the testchain, once the latter is
started.
Once a node knows about the testchain it will also accept rpc calls
starting with ``/chains/test/`` and as a consequence `tezos-client`
can be used with the option ``--chain test``.

Peers randomization
-------------------

This release includes also improvements for the randomization of peers
on the gossip network.
If you are using a front-end node facing the public internet to hide a node in
private mode, it is important for the ip of the private node to be present in
the front-end's list of trusted peers.
It suffice to pass the private node ip to the ``--peer`` option of the
front-end node.

A lighter setup using snapshots
-------------------------------

In order to minimize the impact of the testchain on a baker's existing
infrastructure, instead of enabling the testchain on the node that is
baking on the mainchain, we advice running a second node.
This second node will enable the testchain and it will be used by the
baking daemons of the testchain.
Deploying a second node can be done without duplicating the existing
chain context, but by starting it using a snapshot from your
first node.

For this purpose the `mainnet` node has a new command ``snapshot
export`` that allows to export a snapshot from a existing whole
context, in history mode full or rolling.
Note that the `mainnet` node can only export snapshots and not import
them.

A node from the Git branch (or Docker tag) `mainnet-snapshots` is
capable of importing a snapshot and starting from it.
This allows for a smaller disk footprint and faster node execution
because of better locality on disk.
Furthermore a `mainnet-snapshots` node will start the testchain by
default.

Note that the branch `mainnet-snapshots` produces and requires a
`tezos-node` directory with version 0.0.2 while a node from the branch
`mainnet` requires version 0.0.1.
The two are incompatible and a node will refuse to start if given the
wrong one.

Single machine setup
--------------------

The suggested setup is to run on the same machine the two nodes,
`mainnet` with standard ports and `mainnet-snapshots` on different
ports.
For the `mainnet` node, the updated 003 daemons can be started as usual.
For the `mainnet-snapshots` node, the 004 daemons should be started
with right port and the option ``--chain test``.
Note that both sets of daemons need access to the same `tezos-client`
directory and it is important that this directory is not duplicated.
There are several precautions in place to avoid double baking and
replaying of operations between networks but all daemons should use
the same `tezos-client` to work properly.
An example of this setup is described later.

Two machine setup
-----------------

If running the `mainnet` node and the `mainnet-snapshots` node on two
separate machines is preferable, it is important that the two sets
of daemons for the main and test chain access the same keys from the
same source.
Duplicating keys in never advisable.
This setup can be chosen if you use the Ledge baking app from
Obsidian Systems or a remote signer, such as `tezos-remote-signer`.
They both support the testchain and implement mechanisms to prevent
double baking.

Example setup
-------------

Here's a example of the procedure to set up two nodes on the same
machine, running on different ports, built from sources.

1. Compile the binaries for the two branches `mainnet` and `mainnet-snapshots`
   and copy them into two directories `~/bin-main` and `~/bin-test`.
   For example for `mainnet`::

     git fetch --all
     git checkout mainnet
     git reset --hard origin/mainnet
     make
     mkdir ~/bin-main
     cp tezos-* ~/bin-main/
     git checkout mainnet-snapshots
     git reset --hard origin/mainnet-snapshots
     make
     mkdir ~/bin-test
     cp tezos-* ~/bin-test/


2. Gracefully stop your old node and using the new node from `mainnet`, export a
   snapshot in the history mode you prefer, default is `full`::

     ~/bin-main/tezos-node snapshot export mainnet-$(date +%F).full --data-dir ~/.tezos-node


3. Restart the node and daemons for the main chain with default ports::

     ~/bin-main/tezos-node run --net-addr [::]:9732 --rpc-addr localhost:8732 --data-dir ~/.tezos-node
     ~/bin-main/tezos-baker-003-PsddFKi3    -P 8732 -d ~/.tezos-client run with local node ~/.tezos-node <account>
     ~/bin-main/tezos-endorser-003-PsddFKi3 -P 8732 -d ~/.tezos-client run <account>
     ~/bin-main/tezos-accuser-003-PsddFKi3  -P 8732 -d ~/.tezos-client run


4. Import the snapshot with the node from `mainnet-snapshots` to populate the
   new directory `~/tezos-node-testchain`::

     ~/bin-test/tezos-node snapshot import mainnet-$(date +%F).full --data-dir ~/tezos-node-testchain


5. Generate a fresh network identity::

     ~/bin-test/tezos-node identity generate --data-dir ~/tezos-node-testchain


6. Restart the node and daemons for the test chain on different ports::

     ~/bin-test/tezos-node run --net-addr [::]:9733 --rpc-addr localhost:8733 --data-dir ~/tezos-node-testchain
     ~/bin-test/tezos-baker-004-Pt24m4xi    --chain test -P 8733 -d ~/.tezos-client run with local node ~/tezos-node-testchain <account>
     ~/bin-test/tezos-endorser-004-Pt24m4xi --chain test -P 8733 -d ~/.tezos-client run <account>
     ~/bin-test/tezos-accuser-004-Pt24m4xi  --chain test -P 8733 -d ~/.tezos-client run


Once the testchain starts the 004 daemons will automatically wake up
and the chain will progress.
