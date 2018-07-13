Profiling the Tezos node
========================

Memory profiling the OCaml heap
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

- Install an OCaml switch with the `statmemprof` patch:

  ``4.04.2+statistical-memprof`` or ``4.06.0+statistical-memprof``

- Install ``statmemprof-emacs``.

- Enable loading `statmemprof` into the node.

  Add the ``statmemprof-emacs`` package as a dependency to the main package, and add
  ``let () = Statmemprof_emacs.start 1E-4 30 5`` to the ``node_main.ml`` file.

  Arguments:

  - ``sampling_rate`` is the sampling rate of the profiler. Good value: ``1e-4``.
  - ``callstack_size`` is the size of the fragment of the call stack which is captured for each sampled allocation.
  - ``min_sample_print`` is the minimum number of samples under which the location of an allocation is not displayed.

- Load sturgeon into emacs, by adding this to your ``.emacs``:

::

    (let ((opam-share (ignore-errors (car (process-lines "opam" "config" "var" "share")))))
     (when (and opam-share (file-directory-p opam-share))
      (add-to-list 'load-path (expand-file-name "emacs/site-lisp" opam-share))))

    (require 'sturgeon)

- Launch the node then connect to it with sturgeon.

  If the process is launched with pid ``1234`` then

::

    M-x sturgeon-connect
    tezos-nodememprof.1234.sturgeon

  (tab-completion works for finding the socket name)

Memory profiling the C heap
~~~~~~~~~~~~~~~~~~~~~~~~~~~

- Install ``valgrind`` and ``massif-visualizer``

::

    valgrind --tool=massif tezos-node run ...

- Stop with `Ctrl-C` then display with

::

    massif-visualizer massif.out.pid


Performance profiling
~~~~~~~~~~~~~~~~~~~~~

- Install `perf` (the ``linux-perf`` package for debian).

  If the package does not exist for your current kernel, a previous
  version can be used. Substitute the ``perf`` command to ``perf_4.9``
  if your kernel is 4.9).

- Run the node, find the pid.

- Attach `perf` with ``perf record -p pid --call-stack dwarf``.

  Then stop capturing with ``Ctrl-C``. This can represent a lot of
  data. Don't do that for too long. If this is too much you can remove
  the ``--call-stack dwarf`` to get something more manageable, but
  interpreting the information can be harder.

- display the result with ``perf report``
