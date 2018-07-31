How to contribute
=================

Introduction
------------

The purpose of this document is to help contributors get started with
the Tezos OCaml codebase.


Reporting issues
----------------

The simplest way to contribute to Tezos is to report issues that you may
find with the software on `gitlab <https://gitlab.com/tezos/tezos/issues>`__.
If you are unsure about an issue ask on IRC first and always make sure
to search the existing issues before reporting a new one.
Some info that are probably important to include in the description:
the architecture (e.g. *ARM64*), the operating system (e.g. *Debian
Stretch*), the network you are connected to (e.g. *Alphanet*), the
binary or component (e.g. *tezos-node crashes* or *rpc X returns Y
while Z was expected*).


First steps
-----------

First, make sure that you are proficient enough in OCaml. The community
Website http://www.ocaml.org below gives a few pointer for that. In
particular, we use a lot of functors, and a few GADTs in the codebase,
so you may want to make sure that you master these advanced concepts.

Then, if you don’t know well about the Lwt library, that’s what you want
to learn. This library is used extensively throughout the code base, as
that’s the one we use to handle concurrency, and Tezos is a very
concurrent system. You can use the `online documentation <https://ocsigen.org/lwt/3.2.1/manual/manual>`__. The chapter on concurrency of `Real World
OCaml <https://github.com/dkim/rwo-lwt>`__ has also been ported to Lwt.

After that, it is a good idea to read the tutorials for
:ref:`error_monad<error_monad>` and
:ref:`data_encoding <data_encoding>`, two homegrown
libraries that we use pervasively.

Where to start
--------------

While you familiarize yourself with the basics as suggested above, you
can have a look at the :ref:`software architecture
<software_architecture>` of Tezos. It will
give you the main components and their interactions, and links to the
documentations for the various parts.

Our git workflow
----------------

First, the repository is https://gitlab.com/tezos/tezos, the github one
is just a clone that exists for historical reasons. So if you want to
contribute, simply create an account there.

Then, there are many ways to use Git, here is ours.

We use almost only merge requests to push into master. Meaning, nobody
should push directly into master. Once a merge request is ready, it is
reviewed and approved, we merge it using the ``--fast-forward`` option
of Git, in order to maintain a linear history without merge patches.

For that to work, it means that merge requests must be direct suffixes
of the master branch. So whenever ``origin/master`` changes, you have to
rebase your branch on it, so that you patches always sit on top of
master. When that happens, you may have to edit your patches during the
rebase, and then use ``push -f`` in your branch to rewrite the history.

We also enforce a few hygiene rules, so make sure your MR respects them:

-  Prefer small atomic commits over a large one that do many things.
-  Don’t mix refactoring and new features.
-  Never mix reindentation, whitespace deletion, or other style changes
   with actual code changes.
-  Try as much as possible to make every patch compile, not only the
   last.
-  If you add new functions into a documented interface, don’t forget to
   extend the documentation for your addition.
-  For parts whose specification is in the repository (e.g. Michelson),
   make sure to keep it in sync with the implementation.
-  Try and mimic the style of commit messages, and for non trivial
   commits, add an extended commit message.

As per the hygiene of MRs themselves:

-  Give appropriate titles to the MRs, and when non trivial add a
   detailed motivated explanation.
-  Give meaningful and consistent names to branches.
-  Don’t forget to put a ``WIP:`` flag when it is a work in progress

Some extra CI tests are only done on demand for branches other that
master. You can (should) activate these tests by including keywords in
the branch name.

-  If your MR impacts OPAM packaging, use ``opam`` in the branch name.
-  If your MR updates documentation, use ``doc`` in the branch name.
