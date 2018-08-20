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

 - Developer documentation is available online at https://tezos.gitlab.io/master
   The documentation is automatically generated for the master branch and the
   three official network branches `betanet <https://tezos.gitlab.io/betanet>`_,
   `alphanet <https://tezos.gitlab.io/alphanet>`_,
   `zeronet <https://tezos.gitlab.io/zeronet>`_. Make sure you are
   consulting the right API version.
 - The website https://tezos.com/ contains more information about the project.
 - All development happens on GitLab at https://gitlab.com/tezos/tezos

The source code of Tezos is placed under the MIT Open Source License.

The Community
-------------

 - Several community built block explorers are available:

    - http://tzscan.io
    - https://tezos.id
    - https://tezex.info

- A few community run websites collect useful Tezos links:

    - https://www.tezos.help
    - https://tezos.rocks

 - There is a matrix channel *Tezos* that you can join `here <https://riot.im/app/#/room/#tezos:matrix.org>`_.
 - There is a sub-reddit at https://www.reddit.com/r/tezos/
 - There is also a community FAQ at https://github.com/tezoscommunity/faq/wiki/Tezos-Technical-FAQ
 - There is a *#tezos* IRC channel on *freenode* that is reserved for technical discussions


The Networks
------------

.. _betanet:

Betanet
~~~~~~~

The Tezos Beta (experimental) network is the current incarnation of
the Tezos blockchain.
It runs with real tezzies that have been allocated to the
donors of July 2017 ICO (see :ref:`activate_fundraiser_account`).
It is the step before the full Tezos mainnet, with a `few caveats
<https://tezosfoundation.ch/news/tezos-betanet-expectations>`_.

The Tezos Beta (experimental) network has been live and open since
`June 30th 2018 <https://tezosfoundation.ch/news/tezos-betanet-launch>`_.

All the instructions in this documentation are valid for Betanet
however we **strongly** encourage users to first try all the
introduction tutorials on Alphanet to familiarize themselves without
risks.

.. _alphanet:

Alphanet
~~~~~~~~

Tezos Alphanet is a test network for the Tezos blockchain with a
faucet to obtain free tezzies (see :ref:`faucet`).
It is updated and rebooted rarely and it is running the same code as
the Betanet.
It is the reference network for developers wanting to test their
software before going to beta and for users who want to familiarize
themselves with Tezos before using their real tezzies.

We offer support for Alphanet on IRC.

The Tezos Alpha (test) network has been live and open since February 2017.


.. _zeronet:

Zeronet
~~~~~~~

Zeronet is the most cutting-edge development network of Tezos. It is
restarted without notice, possibly several times a day.
This network is mostly used internally by the Tezos developers and may
have *different constants* that Alphanet or Betanet, for example it
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
   introduction/contributing

.. toctree::
   :maxdepth: 2
   :caption: White doc:

   whitedoc/the_big_picture
   whitedoc/p2p
   whitedoc/validation
   whitedoc/michelson
   whitedoc/proof_of_stake

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

Indices and tables
==================

* :ref:`genindex`
* :ref:`modindex`
* :ref:`search`
