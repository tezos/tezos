TEZOS
=====

Tezos is a distributed consensus platform with meta-consensus capability. Tezos
not only comes to consensus about state, like BTC or ETH. It also comes to
consensus about how the protocol and the nodes should adapt and upgrade.

See https://www.tezos.com/ for more information about the project.


Build instructions
------------------

To compile Tezos, you need an OCaml compiler (version 4.04.0) and all the
libraries listed in `src/tezos-deps.opam`.

The best way to install all dependencies is by first installing
[OPAM](https://opam.ocaml.org/), the OCaml package manager.

Create a new switch alias for Tezos. A switch is your own version of the OPAM
configuration, including the OCaml compiler, all packages, and package manager
configuration related to your project. This is necessary so that the project
doesn't conflict with other OCaml projects or other versions of Tezos.

```shell
opam switch tezos --alias-of 4.04.2
```

Activate the new switch:

```shell
eval `opam config env`
```

Install Tezos dependencies:

```shell
make build-deps
```

Compile the project:

```shell
make
```

This should produce three binaries:

* `tezos-node`: the tezos daemon itself;
* `tezos-client`: a minimal command-line client;
* `tezos-protocol-compiler`: a protocol compiler used for developing new version of the economic protocol.

Currently Tezos is being developed for Linux only. It should work on mac OS,
but it has not been tested recently. A Windows port is in progress.


Note that, when executing `make build-deps`, OPAM will detect if
required system dependencies are installed. However, it is not able to
detect which versions you actually have. Typically, `make` will
probably fail if you have an libsodium < 1.0.11. In this case, make
sure you have a recent version of libsodium and libsodium-dev, or
download and install them from, eg,
https://pkgs.org/download/libsodium18 and
https://pkgs.org/download/libsodium-dev

Running the node in a sandbox
-----------------------------

To run a single instance of a Tezos node in sandbox mode:

```shell
./tezos-node run --sandbox --rpc-addr localhost:8732
```

This "sandboxed" node will not participate in the P2P network, but will accept
RPC from localhost on port 8732. See below from more details on the RPC
interface.


Running the node
----------------

So far there is no official Tezos network being run, but you might run a local
test network (the development team is running its own testnet, if you're interested
in joining this network, please make a request on our slack channel. We have
limited support abilities at the moment but we'll try to help you best we can).

Use the following command to run a node that will accept incoming
connections:

```shell
./tezos-node identity generate 24.
```

This will first generate a new node identity and compute the
associated stamp of proof-of-work. Then, the node will listen to
connections coming in on `[::]:9732`. All used data is stored at
`$HOME/.tezos-node/`. For example, the default configuration file is
at `$HOME/.tezos-node/config.json`.

To run multiple nodes on the same machine, you can duplicate and edit
`$HOME/.tezos-node/config.json` while making sure they don't share paths to the
database or any other data file (cf. options `db.store` ; `db.context` ;
`db.protocol`, `net.peers-metadata` and `net.identity`).

You could also let Tezos generate a config file by specifying options on the
command line. For instance, if `$dir/config.json` does not exist, the following
command will generate it and replace the default values with the values from
the command line:

```shell
./tezos-node run --data-dir "$dir" --net-addr localhost:9733
```

The Tezos server has a built-in mechanism to discover peers on the local
network (using UDP packets broadcasted on port 7732).

If this mechanism is not sufficient, one can provide Tezos with a list of
initial peers, either by editing the option `net.bootstrap-peers` in the
`config.json` file, or by specifying a command line parameter:

```shell
./tezos-node run \
             --data-dir "$dir" --net-addr localhost:2023 \
             --peer localhost:2021 --peer localhost:2022
```

If `"$dir"/config.json` exists, the command line options override those
read in the config file. By default, Tezos won't modify the content of an
existing `"$dir"/config.json` file. But, you may explicit ask the node
to reset or to update the file according to the command line parameters
with the following commands line:

```shell
./tezos-node config reset --data-dir "$dir" --net-addr localhost:9733
./tezos-node config update --data-dir "$dir" --net-addr localhost:9734
```

Configuration options
---------------------

Here is an example configuration file with all parameters
specified. Most of the time it uses default values, except for cases
where the default is not explanatory enough (i.e. "bootstrap-peers" is
an empty list by default). Comments are not allowed in JSON, so this
configuration file would not parse. They are just provided here to
help writing your own configuration file if needed.


```javascript
{

  /* Location of the data dir on disk. */

  "data-dir": "/home/tezos/my_data_dir",

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

    /* Specify if the network is closed or not. A closed network
    allows only peers listed in "bootstrap-peers". */

    "closed": false,

    /* Network limits */

    "limits": {

      /* Delay granted to a peer to perform authentication, in
      seconds. */

      "authentication-timeout": 5,

      /* Strict minimum number of connections (triggers an urgent
      maintenance). */

      "min-connections": 50,

      /* Targeted number of connections to reach when bootstraping /
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
  }
}
```

Debugging
---------

It is possible to set independant log levels for different logging
sections in Tezos, as well as specifying an output file for
logging. See the description of log parameters above as well as
documentation under the DEBUG section diplayed by `tezos-node run
--help'.


JSON/RPC interface
------------------

The Tezos node provides a JSON/RPC interface. Note that it is an RPC, and it is
JSON based, but it does not follow the "JSON-RPC" protocol. It is not active by
default and it must be explicitely activated with the `--rpc-addr` option.
Typically, if you are not trying to run a local network and just want to
explore the RPC, you would run:

```shell
./tezos-node run --sandbox --rpc-addr localhost
```

The RPC interface is self-documented and the `tezos-client` executable is able
to pretty-print the RPC API. For instance, to see the API provided by the Tezos
Shell:

```shell
./tezos-client rpc list
```

To get API attached to the "genesis" block, including the remote procedures
provided by the associated economic protocol version:

```shell
./tezos-client rpc list /blocks/genesis/
```

You might also want the JSON schema describing the expected input and output of
a RPC. For instance:

```shell
./tezos-client rpc schema /blocks/genesis/hash
```

Note: you can get the same information, but as a raw JSON object, with a simple
HTTP request:

```shell
wget --post-data '{ "recursive": true }' -O - http://localhost:8732/describe
wget --post-data '{ "recursive": true }' -O - http://localhost:8732/describe/blocks/genesis
wget -O - http://localhost:8732/describe/blocks/genesis/hash
```



The minimal CLI client
----------------------

Work in progress.

See `./tezos-client -help` for available commands.
