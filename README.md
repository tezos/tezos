TEZOS
=====

See https://www.tezos.com/ for more information about the project.


Build instructions
------------------

To compile Tezos, you need an OCaml compiler (version 4.03.0) and all the
libraries listed in `src/tezos-deps.opam`. To install all the required
dependencies, we strongly recommand to install
[OPAM](https://opam.ocaml.org/), the OCaml package manager, and then to
run the following command:

```
make build-deps
```

Then, to compile the project, simply run:

```
make
```

This should produce three binaries:

* `tezos-node`: the tezos daemon itself;
* `tezos-client`: a minimal command-line client;
* `tezos-protocol-compiler`: a protocol compiler used for developing new version of the economical protocol.

Currently Tezos is developped/tested under Linux only.
It should work on mac OS, but this has not been tested recently.
The Windows port is in progress.



Running the node in a sandbox
-----------------------------

To run a single instance of a Tezos node in a sandbox mode:

```
./tezos-node -sandbox /path/to/a/custom/data/dir -rpc-port 8732
```

This "sandboxed" node will not participate to the P2P network, but
it accepts RPC from the local host on port 8732. See below from more
details on the RPC interface.


Running the node
----------------

There is not an official running Tezos network yet. But you might run
a local test network. To run a node that accept incoming connections,
simply run:

```
./tezos-node
```

It will listen to incoming connection on `0.0.0.0:9732` (and `[::]:9732`).
All useful data are stored into `${HOME}/.tezos-node/`, e.g. a default
configuration file was generated: `${HOME}/.tezos-node/config`.

To create multiple nodes on the same machine, it is possible to
duplicate and edit `${HOME}/.tezos-node/config` while taking care not to
share paths for accessing the database or any other data file
(cf. options `db.store` ; `db.context` ; `net.peers` and `protocol.dir`).

Another possibility is to let Tezos generate a config file by specifying
options on the command line. For instance, if `${DIR}/config` does not
exist, the following command will generate it and replace the default values
with the values from the command line arguments:

```
./tezos-node -base-dir ${DIR} -net-port 9733 -net-addr 127.0.0.1
```

The Tezos server has a built-in mechanism to discover peers on the local net
(using UDP packets broadcasted on port 7732).

If this mechanism is not sufficient, one can provide Tezos with a list of
initial peers, either by editing the option `net.bootstrap.peers` in the
`config` file, or by specifying a command line parameter:

```
./tezos-node -base-dir ${DIR} -net-port 2023 -net-addr 127.0.0.1 \
             -net-bootstrap-peers '[("127.0.0.1", 2021);("127.0.0.1", 2022)]'
```

If `${DIR}/config` exists, the command line options override those read
in the config file. Tezos never modifies the content of an existing
`${DIR}/config` file.

```
./tezos-node -config-file ${DIR}/config
```


JSON/RPC interface
------------------

The tezos node provides a JSON/RPC interface. It is not active by
default and it should be explicitely activated with the `rpc-port`
option. This RPC interface is self-documented and the `tezos-client`
is able to pretty-print the list of available RPCs. For instance, for
the list of RPC provided by the Shell:

```
./tezos-client rpc list
```

And, for the list of RPC attached to the "genesis" bloc,
including the RPC provided by the associated economical-protocol version:

```
./tezos-client rpc list /blocks/genesis/
```

You might also get the JSON schema describing the expected input and output
of a RPC. For instance:

```
./tezos-client rpc schema /block/genesis/hash
```

Note: you might get the same information, but as a raw JSON object,
with a simple HTTP request:

```
wget --post-data '{ "recursive": true }' -O - http://127.0.0.1:8732/describe
wget --post-data '{ "recursive": true }' -O - http://127.0.0.1:8732/describe/blocks/genesis
wget -O - http://127.0.0.1:8732/describe/blocks/genesis/hash
```



The minimal CLI client
----------------------

Work in progress.

See `./tezos-client -help` for the current list of available commands.