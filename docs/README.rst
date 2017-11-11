******************************
Building documentation locally
******************************

Building instructions
---------------------

To build the documentaion, you can use the main Makefile target ``doc-html``

.. code:: bash

    make doc-html

The documentation is built by Sphinx, and uses the Read The Docs theme.

On a debian system, you can install the needed dependencies with:

.. code:: bash

    sudo apt install \
      python3-recommonmark \
      python3-sphinx \
      python3-sphinx-rtd-theme

OCaml documentation
-------------------

Odoc is used for OCaml API generation, that you can install with:

.. code:: bash

    opam install odoc

Tezos generates the API documentation for all libraries in HTML format.  The
generated HTML pages in ``_build/<context>/_doc``. It creates one sub-directory
per public library and generates an ``index.html`` file in each sub-directory.

The documentation is not installed on the system by Tezos. It is meant to be
read locally while developing and then published on the www when releasing
packages.
