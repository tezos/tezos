
TEZOS
=====

To compile:

```
make build_deps
make
```

=========

To run a single instance :

```
./tezos-node
```

All useful data are stored in `${HOME}/.tezos-node`.

To run a test instance, without connecting to the gossup network :

```
./tezos-node -sandbox /path/to/a/custom/data/dir
```

Useful data will be stored in the directory `/path/to/a/custom/data/dir`
instead of `${HOME}/.tezos-node`.

=========

To create other instances on the same machine, it is possible to
duplicate and edit `${HOME}/.tezos/config` while taking care not to
share paths for accessing the database or any other data file
(cf. options `db.store` ; `db.context` ; `net.peers` and `protocol.dir`).

Another possibility is to let Tezos generate a config file by specifying
options on the command line. For instance, if `${DIR}/config` does not
exist, the following command will generate it and replace the default values
with the values from the command line arguments:

```
./tezos-node -base-dir ${DIR} -net-port 2023 -net-addr 127.0.0.1
```

The Tezos server has a built-in mechanism to discover peers on the local net
(using UDP packets broadcasted on port 7732)

If this mechanism isn't sufficient, one can provide Tezos with a list of
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

