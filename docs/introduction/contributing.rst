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

Then, if you don’t know much about the Lwt library, that’s what you want
to learn next. This library is used extensively throughout the code base, as
that’s the one we use to handle concurrency; and Tezos is a very
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

We mostly use merge requests for master, meaning that nobody should be pushing
into master directly. Once a merge request is ready, it is reviewed and
approved, then merged using the ``--fast-forward`` option of Git in order to
maintain a linear history without merge patches.

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

Code Review
-----------

At tezos all the code is peer reviewed before getting committed in the
master branch. Briefly, a code review is a discussion between two or
more developers about changes to the code to address an issue.

Author Perspective
~~~~~~~~~~~~~~~~~~

Code review is a tool among others to enhance the quality of the code and to
reduce the likelihood of introducing new bugs in the code base. It is a
technical discussion, it is not an exam, but it is a common effort to learn from
each other.

These are a few common suggestions we often give while reviewing new code.
Addressing these points beforehand makes the reviewing process easier and less
painful for everybody. The reviewer is your ally, not your enemy.

- Commented code: Did I remove any commented out lines?
  Did I leave a ``TODO`` or an old comment?

- Docstrings: Did I export a new function? Each exported
  function should be documented in the corresponding ``mli`` .

- Readability: Is the code easy to understand? Is it worth adding
  a comment to the code to explain a particular operation and its
  repercussion on the rest of the code?

- Variable and function names: These should be meaningful and in line
  with the convention adopted in the code base.

- Testing: Are the tests thoughtful? Do they cover the failure conditions? Are
  they easy to read? How fragile are they? How big are the tests? Are they slow?

- Are your Commit messages meaningful? (i.e., https://chris.beams.io/posts/git-commit/ )

Review your own code before calling for a peer review from a college.

Reviewer Perspective
~~~~~~~~~~~~~~~~~~~~

Code review can be challenging at times. These are suggestions and common
pitfalls a code reviewer should avoid.

- Ask questions: How does this function work? If this requirement changes,
  what else would have to change? How could we make this more maintainable?

- Discuss in person for more detailed points: Online comments are useful for
  focused technical questions. In many occasions it is more productive to
  discuss it in person rather than in the comments. Similarly, if discussion
  about a point goes back and forth, It will be often more productive to pick
  it up in person and finish out the discussion.

- Explain reasoning: Sometimes it is best to both ask if there is a better
  alternative and at the same time justify why a problem in the code is worth
  fixing. Sometimes it can feel like the changes suggested are nit-picky
  without context or explanation.

- Make it about the code: It is easy to take notes from code reviews
  personally, especially if we take pride in our work. It is best to make
  discussions about the code than about the developer. It lowers resistance and
  it is not about the developer anyway, it is about improving the quality of
  the code.

- Suggest importance of fixes: While offering many suggestions at once, it is
  important to also clarify that not all of them need to be acted upon and some
  are more important than others. It gives an important guidance to the developer
  to improve their work incrementally.

- Take the developer's opinion into consideration: Imposing a particular design
  choice out of personal preferences and without a real explanation will
  incentivize the developer to be a passive executor instead of a creative agent.

- Do not re-write, remove or re-do all the work: Sometimes it is easier to
  re-do the work yourself discarding the work of the developer. This can give
  the impression that the work of the developer is worthless and adds
  additional work for the reviewer that effectively takes responsibility for
  the code.

- Consider the person you are reviewing: Each developer is a person. If you
  know the person, consider their personality and experience while reviewing their
  code. Sometime it is possible with somebody to be more direct and terse, while
  other people require a more thorough explaination.

- Avoid confrontational and authoritative language: The way we communicate has
  an impact on the receiver. If communicating a problem in the code or a
  suggestion is the goal, making an effort to remove all possible noise from
  the message is important. Consider these two statements to communicate about
  a problem in the code : "This operation is wrong. Please fix it." and
  "Doing this operation might result in an error, can you please
  review it?". The first one implies you made an error (confrontational), and
  you should fix it (authority). The second suggest to review the code because
  there might be a mistake. Despite the message being the same, the recipient might
  have a different reactions to it and impact on the quality of this work. This
  general remark is valid for any comment.

Practicalities : how to ask for a code review.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Our code review process uses GitLab. First a developer creates a new
branch (it is often useful to prefix the name of the branch with the name of
the developer to make it clear at glance who is working on what : e.g.
``john@new-feature``). This is a private new branch, the developer is free to
rebase, squash commits, rewrite history (``git push --force``), etc. at will.

Once the code is ready to be shared with the rest of the team, the developer
opens a Merge Request. It is useful to add a precise description of the code
changes while opening the MR and check if those are in line with the initial
requirements.

If the code is still not ready to be peer reviewed, but it is merely a
RFC, we prefix the MR with ``WIP:`` (work in progress). This will tell everybody
they can look at the code, comment, but there is still work to be done and the
branch can change and history be rewritten.

Finally, when the code is ready to be audited, we remove the WIP status of the
MR and we freeze the branch. From this moment on, the developer will refrain to
rewrite history (but he/she can add new commits) and to rebase the branch
without notice. At this point the developer waits for the reviewer to add his
comments and suggestions.

Gitlab allows to comment both on the code and to add general comments on the
MR.  Each comment should be addressed by the developer. He/she can add
additional commits to address each comment. This incremental approach will make
it easier for the reviewer to keep interacting till each discussion is
resolved. When the reviewer is satisfied, he/she will mark the discussion resolved.

When all discussions are resolved, the reviewer will rebase the branch,
possibly squash commits and merge the MR in the master branch.
