TEZOS
=====

Tezos is a distributed consensus platform with meta-consensus capability. Tezos
not only comes to consensus about state, like BTC or ETH. It also comes to
consensus about how the protocol and the nodes should adapt and upgrade.

See https://www.tezos.com/ for more information about the project.


Build instructions
------------------

To compile Tezos, you need an OCaml compiler (version 4.03.0) and all the
libraries listed in `src/tezos-deps.opam`.

The best way to install all dependencies is by first installing
[OPAM](https://opam.ocaml.org/), the OCaml package manager.

Create a new switch alias for Tezos. A switch is your own version of the OPAM
configuration, including the OCaml compiler, all packages, and package manager
configuration related to your project. This is necessary so that the project
doesn't conflict with other OCaml projects or other versions of Tezos.

```
opam switch tezos --alias-of 4.03.0
```

Activate the new switch:

```
eval `opam config env`
```

Install Tezos dependencies:

```
make build-deps
```

Compile the project:

```
make
```

This should produce three binaries:

* `tezos-node`: the tezos daemon itself;
* `tezos-client`: a minimal command-line client;
* `tezos-protocol-compiler`: a protocol compiler used for developing new version of the economical protocol.

Currently Tezos is being developed for Linux only. It should work on mac OS,
but it has not been tested recently. A Windows port is in progress.



Running the node in a sandbox
-----------------------------

To run a single instance of a Tezos node in sandbox mode:

```
./tezos-node -sandbox /path/to/a/custom/data/dir -rpc-port 8732
```

This "sandboxed" node will not participate in the P2P network, but will accept
RPC from localhost on port 8732. See below from more details on the RPC
interface.


Running the node
----------------

So far there is no official Tezos network being run, but you might run a local
test network. Use the following command to run a node that will accept incoming
connections:

```
./tezos-node
```

The node will listen to connections coming in on `0.0.0.0:9732` (and
`[::]:9732`). All used data is stored at `${HOME}/.tezos-node/`. For example,
the default configuration file is at `${HOME}/.tezos-node/config`.

To run multiple nodes on the same machine, you can duplicate and edit
`${HOME}/.tezos-node/config` while making sure they don't share paths to the
database or any other data file (cf. options `db.store` ; `db.context` ;
`net.peers` and `protocol.dir`).

You could also let Tezos generate a config file by specifying options on the
command line. For instance, if `$dir/config` does not exist, the following
command will generate it and replace the default values with the values from
the command line:

```
./tezos-node -base-dir "$dir" -net-port 9733 -net-addr 127.0.0.1
```

The Tezos server has a built-in mechanism to discover peers on the local
network (using UDP packets broadcasted on port 7732).

If this mechanism is not sufficient, one can provide Tezos with a list of
initial peers, either by editing the option `net.bootstrap.peers` in the
`config` file, or by specifying a command line parameter:

```
./tezos-node -base-dir "$dir" -net-port 2023 -net-addr 127.0.0.1 \
             -net-bootstrap-peers '[("127.0.0.1", 2021);("127.0.0.1", 2022)]'
```

If `"$dir"/config` exists, the command line options override those read in the
config file. Tezos won't modify the content of an existing `"$dir"/config`
file.

```
./tezos-node -config-file "$dir"/config
```


JSON/RPC interface
------------------

The Tezos node provides a JSON/RPC interface. Note that it is an RPC, and it is
JSON based, but it does not follow the "JSON-RPC" protocol. It is not active by
default and it must be explicitely activated with the `-rpc-port` option.
Typically, if you are not trying to run a local network and just want to
explore the RPC, you would run:

```
./tezos-node -sandbox /path/to/a/custom/data/dir -rpc-port 8732
```

The RPC interface is self-documented and the `tezos-client` executable is able
to pretty-print the RPC API. For instance, to see the API provided by the Tezos
Shell:

```
./tezos-client rpc list
```

To get API attached to the "genesis" block, including the remote procedures
provided by the associated economical protocol version:

```
./tezos-client rpc list /blocks/genesis/
```

You might also want the JSON schema describing the expected input and output of
a RPC. For instance:

```
./tezos-client rpc schema /block/genesis/hash
```

Note: you can get the same information, but as a raw JSON object, with a simple
HTTP request:

```
wget --post-data '{ "recursive": true }' -O - http://127.0.0.1:8732/describe
wget --post-data '{ "recursive": true }' -O - http://127.0.0.1:8732/describe/blocks/genesis
wget -O - http://127.0.0.1:8732/describe/blocks/genesis/hash
```



The minimal CLI client
----------------------

Work in progress.

See `./tezos-client -help` for available commands.