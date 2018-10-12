.. Tezos documentation master file, created by
   sphinx-quickstart on Sat Nov 11 11:08:48 2017.
   You can adapt this file completely to your liking, but it should at least
   contain the root `toctree` directive.


Welcome to the Tezos Developer Documentation!
=============================================

The Project
-----------

Tezos is a distributed consensus platform with meta-consensus
capability. Tezos not only comes to consensus about the state of its ledger,
like Bitcoin or Ethereum. It also attempts to come to consensus about how the
protocol and the nodes should adapt and upgrade.

 - Developer documentation is available online at
   https://tezos.gitlab.io/master . The documentation is automatically
   generated for the master branch, the `main network (mainnet)
   <https://tezos.gitlab.io/mainnet>`_ and the `test network (alphanet)
   <https://tezos.gitlab.io/alphanet>`_.
   Make sure you are consulting the right version.
 - The website https://tezos.com/ contains more information about the project.
 - All development happens on GitLab at https://gitlab.com/tezos/tezos

The source code of Tezos is placed under the MIT Open Source License.


The Community
-------------

- The website of the `Tezos Foundation <https://tezos.foundation/>`_.
- `Tezos sub-reddit <https://www.reddit.com/r/tezos/>`_ is an
  important meeting point of the community.
- Several community-built block explorers are available:

    - https://tzscan.io

- A few community-run websites collect useful Tezos links:

    - https://www.tezos.help
    - https://tezos.rocks

- More resources can be found in the :ref:`support` page.


The Networks
------------

Mainnet
~~~~~~~

The Tezos network is the current incarnation of the Tezos blockchain.
It runs with real tez that have been allocated to the
donors of July 2017 ICO (see :ref:`activate_fundraiser_account`).

The Tezos network has been live and open since June 30th 2018.

All the instructions in this documentation are valid for Mainnet
however we **strongly** encourage users to first try all the
introduction tutorials on Alphanet to familiarize themselves without
risks.

Alphanet
~~~~~~~~

Tezos Alphanet is a test network for the Tezos blockchain with a
faucet to obtain free tez (see :ref:`faucet`).
It is updated and rebooted rarely and it is running the same code as
the Mainnet.
It is the reference network for developers wanting to test their
software before going to beta and for users who want to familiarize
themselves with Tezos before using their real tez.

We offer support for Alphanet on IRC.

The Tezos Alpha (test) network has been live and open since February 2017.

Zeronet
~~~~~~~

Zeronet is the most cutting-edge development network of Tezos. It is
restarted without notice, possibly several times a day.
This network is mostly used internally by the Tezos developers and may
have *different constants* from Alphanet or Mainnet, for example it
has shorter cycles and a shorter interval between blocks.
We offer no support for the Zeronet.


Getting started
---------------

The best place to start exploring the project is following the How Tos
in the :ref:`introduction <howtoget>`.


.. toctree::
   :maxdepth: 2
   :caption: Introduction:

   introduction/howtoget
   introduction/howtouse
   introduction/howtorun
   introduction/various
   introduction/support
   introduction/contributing
   introduction/glossary

.. toctree::
   :maxdepth: 2
   :caption: White doc:

   whitedoc/the_big_picture
   whitedoc/p2p
   whitedoc/validation
   whitedoc/michelson
   whitedoc/proof_of_stake
   whitedoc/voting

.. toctree::
   :maxdepth: 2
   :caption: Protocols:

   protocols/003_PsddFKi3

.. toctree::
   :maxdepth: 2
   :caption: Releases:

   releases/april

.. toctree::
   :maxdepth: 2
   :caption: Developer Tutorials:

   tutorials/rpc
   tutorials/data_encoding
   tutorials/error_monad
   tutorials/michelson_anti_patterns
   tutorials/entering_alpha
   tutorials/protocol_environment
   tutorials/profiling

.. toctree::
   :maxdepth: 2
   :caption: APIs:

   README
   api/api-inline
   api/cli-commands
   api/rpc
   api/errors
   api/p2p


Indices and tables
==================

* :ref:`genindex`
* :ref:`modindex`
* :ref:`search`
