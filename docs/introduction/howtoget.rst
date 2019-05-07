.. _howtoget:

How to get Tezos
================

In this How To we explain how to get up-to-date binaries to run Tezos
for each network.
You can either use the docker images, which is easier, or build from
sources.


Docker images
-------------

The recommended way for running an up-to-date Tezos node is to use the
docker images that are automatically generated from the GitLab
repository and published on `DockerHub
<https://hub.docker.com/r/tezos/tezos/>`_.
The script ``alphanet.sh`` is provided to help download the right
image for each network and run a simple node.
Its only requirement is a working installation of `Docker
<https://www.docker.com/>`__ and docker compose on a machine with
architecture **x86_64**.
Although we only officially support Linux, the script has been tested
with success in the past on windows/mac/linux.

The same script can be used to run Mainnet, Alphanet or Zeronet, it
suffices to rename it as it downloads a different image based on its
name.
For example, to run Alphanet:

::

    wget https://gitlab.com/tezos/tezos/raw/master/scripts/alphanet.sh
    chmod +x alphanet.sh

Alternatively, to run Mainnet:

::

    wget -O mainnet.sh https://gitlab.com/tezos/tezos/raw/master/scripts/alphanet.sh
    chmod +x mainnet.sh

In the following we assume you are running Alphanet.
You are now one step away from a working node:

::

    ./alphanet.sh start

This will download the right docker image for your chosen network, launch 3
docker containers running the node, the baker and the endorser. Keep in mind
that when a tezos node is launched, it needs to connect to new peers and
synchronize the chain. This can be *lengthy* on the first launch
considering the chain takes up several gigabytes of data. See
:ref:`how to use Tezos<howtouse>` for more details.

Every call to ``alphanet.sh`` will check for updates of the node and
will fail if your node is not up-to-date. For updating the node, simply
run:

::

    ./alphanet.sh restart

If you prefer to temporarily disable automatic updates, you just have to
set an environment variable:

::

    export TEZOS_ALPHANET_DO_NOT_PULL=yes

See ``./alphanet.sh --help`` for more informations about the
script. In particular see ``./alphanet.sh client --help`` or the
:ref:`online manual<client_manual>` for more information about
the client. Every command to the ``tezos-client`` can be equivalently
executed using ``./alphanet.sh client``. Similary, ``tezos-admin-client``
can be executed using ``./alphanet.sh admin-client``.


Build from sources
------------------

**TL;DR**: Typically you want to do:

::

   sudo apt install -y rsync git m4 build-essential patch unzip bubblewrap wget libev-dev libgmp-dev pkg-config libhidapi-dev which
   wget https://github.com/ocaml/opam/releases/download/2.0.1/opam-2.0.1-x86_64-linux
   sudo cp opam-2.0.1-x86_64-linux /usr/local/bin/opam
   sudo chmod a+x /usr/local/bin/opam
   git clone https://gitlab.com/tezos/tezos.git
   cd tezos
   git checkout alphanet
   opam init --bare
   make build-deps
   eval $(opam env)
   make
   export PATH=~/tezos:$PATH
   source ./src/bin_client/bash-completion.sh
   export TEZOS_CLIENT_UNSAFE_DISABLE_DISCLAIMER=Y

For development, numerous shell scripts also rely on jq:

::

   sudo apt install -y jq


Environment
~~~~~~~~~~~

Currently Tezos is being developed for Linux x86_64, mostly for
Debian/Ubuntu and Archlinux.

The following OSes are reported to work:

- macOS/x86_64
- Linux/armv7h (32 bits) (Raspberry Pi3, etc.)
- Linux/aarch64 (64 bits) (Raspberry Pi3, etc.)

A Windows port is feasible and might be developed in the future.

If ``bubblewrap`` is not available in your distribution you can also
skip it and init opam with ``--disable-sandbox``.

Get the sources
~~~~~~~~~~~~~~~

Tezos *git* repository is hosted at `GitLab
<https://gitlab.com/tezos/tezos/>`_. All development happens here. Do
**not** use our `GitHub mirror <https://github.com/tezos/tezos>`_
which we don't use anymore and only mirrors what happens on GitLab.

You also need to **choose the branch** of the network you want to connect
to: *alphanet*, *zeronet* or *mainnet*.

The *master* branch is where code is merged, but there is no test
network using the master branch directly.


Install OPAM
~~~~~~~~~~~~

To compile Tezos, you need the `OPAM <https://opam.ocaml.org/>`__
package manager, version *2.0*. The build script will take
care of setting-up OPAM, download the right version of the OCaml
compiler, and so on.

Use ``opam init --bare`` to avoid compiling the OCaml compiler now: it
will be done in the next step.


Install Tezos dependencies with OPAM
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Install the OCaml compiler and the libraries which Tezos depends on:

::

   make build-deps

Alternatively, if you want to be able to install extra packages
(development packages such as `merlin`), you may use the following
command instead:

::

   make build-dev-deps

This command creates a local opam switch ``_opam`` where the right
version of OCaml is compiled and installed (this takes a while but
it's only done once).

After OCaml it will start with Tezos dependencies, OPAM is able to
handle correctly the OCaml libraries but it is not always able to
handle all external C libraries we depend on. On most system, it is
able to suggest a call to the system package manager but it currently
does not handle version check.

Once the dependencies are done we can update opam's environment to
refer to the new switch and compile the project:

::

   eval $(opam env)
   make

Lastly you can also add Tezos binaries to your ``PATH`` variable,
activate bash autocompletion and after reading the Disclaimer a few
hundred times you are allowed to disable it with
``TEZOS_CLIENT_UNSAFE_DISABLE_DISCLAIMER=Y``.
