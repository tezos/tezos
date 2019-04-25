# System testing environment for Tezos

This repository contains:
- A Python API to run and interact with Tezos clients and servers,
- A system testing environment based on the
 [`pytest`](https://docs.pytest.org/en/latest/) package.

It contains the following python packages.
- `daemons` defines classes to run Tezos node and daemons,
- `client` mainly defines the `Client` class, that provides a programmatic
   interface to a client,
- `launcher` defines classes used to launch a nodes and daemons with specific
   settings,
- `tools` contains utility functions and constants shared by the tests,
- `examples` contains example of tests and scripts that run scenarios of
   interactions between tezos nodes and clients,
- `tests` contains `pytest` tests,
- `scripts` contains utility scripts.

They are organized in three layers.
1. `daemons` and `client`,
2. `launchers`,
3. `tests`, `examples`, `tools`.

## Installation

You need:
- A working environment (see [documentation](http://tezos.gitlab.io/mainnet/index.html))
 with the binaries compiled,
- `python` (version >= 3.6),
- A local copy of the tezos [repository](https://gitlab.com/tezos/tezos),
- the `pip` package manager.

On some systems, several versions of `python` coexist. You need to explicitly
use `python3` and `pip3` (instead of `python` and `pip`).

Install a few python packages
```
cd PATH_TO_YOUR_TEZOS_DIR/tests_python
pip3 install -r requirements.txt
```

Then run some tests
```
pytest examples/test_example.py  # simple test example
pytest -m "not slow"  # run all tests not marked as slow
pytest -s tests/test_injection.py  # run a specific test with traces
pytest  # run all tests
```

### On `tezdev` (test server at nomadic)

Same as above, but export local python binaries path.
```
export PATH=$PATH:~/.local/bin/
```

### On debian systems (without pip)

```
sudo apt install python3-pytest python3-pytest-timeout
```
Notice that the command `pytest` on debian is `pytest-3`.
Other option, use `pip` with `python3-pip`.

## A simple sandbox scenario

The following example runs a couple of nodes and performs
a transfer operation.

```
import time
from tools import constants, paths, utils
from launchers.sandbox import Sandbox


def scenario():
    """ a private tezos network, initialized with network parameters
        and some accounts. """
    with Sandbox(paths.TEZOS_HOME,
                 constants.IDENTITIES,
                 constants.GENESIS_PK) as sandbox:
        # Launch node running protocol alpha
        sandbox.add_node(0)
        utils.activate_alpha(sandbox.client(0))
        # Launch a second node on the same private tezos network
        sandbox.add_node(1)
        # Launch a baker associated to node 0, baking on behalf of delegate
        # bootstrap5
        sandbox.add_baker(0, 'bootstrap5', proto=constants.ALPHA_DEAMON)
        # first client tells node 0 to transfer money for an account to another
        # receipt is an object representing the client answer
        receipt = sandbox.client(0).transfer(500, 'bootstrap1', 'bootstrap3')
        transfer_hash = receipt.operation_hash
        # Wait for second node to update its protocol to alpha, if not
        # it may not know yet the `wait_for_inclusion` operation which is
        # protocol specific
        time.sleep(5)
        # second client waits for inclusion of operation by the second node
        sandbox.client(1).wait_for_inclusion(transfer_hash)


if __name__ == "__main__":
    scenario()
```

This can be run with `python3 examples/example.py`. It should display all the
clients commands and their results.

The `sandbox` object allows the user to add nodes, bakers or endorsers
running in tezos sandboxed mode. Whenever a node has been added, one can
access it using a client object.

The client object is a wrapper on the `tezos-client` command. It runs
`tezos-client` with "administrative" parameters, plus the parameters determined
by the  method called by the user.

For instance
```
receipt = client.transfer(500, 'bootstrap1', 'bootstrap3')
```
will run something like
```
tezos-client -base-dir /tmp/tezos-client.be22ya16 -addr 127.0.0.1 -port 18730 transfer 500 from bootstrap1 to bootstrap3
```
`receipt` is an object of type `client_ouput.TransferResult` which gives
access to some data of the `tezos-client` output.

Alternatively, one can always construct the command manually:

```
client_output =  client.run(['transfer', '500', 'from', 'bootstrap1', 'bootstrap3'])
```

In that case, `client_output` is the string returned by the client, such as
```
Node is bootstrapped, ready for injecting operations.
Estimated gas: 10100 units (will add 100 for safety)
Estimated storage: no bytes added
Operation successfully injected in the node.
Operation hash is 'op9K2VJjKJLaFnfQKzsoz9rzr5v1PrLjpefiPtVhuiiXYgkZes1'
...
```

The first method is more convenient and less error prone. But the second
method is more generic and sometimes the only option if the specialized method
doesn't exist yet.

## Test suite and `pytest`

Tests are located in the `tests` directory and rely on the `pytest` library.

Tests are divided into modules, and are furthermore subdivided into classes.
A class defines a full testing scenario. A typical scenario is a sequence of
client commands and assertions, operating on a set of Tezos nodes running in
a private network (a.k.a *sandbox* mode).

### Running tests

#### Useful options

`pytest` has a lot of launching options, see the doc for the full list.

Useful options:
- `-v` display test names,
- `-x` stop at first failure,
- `-s` display output, including commands launched and stdout from client
  (by default, pytest captures all *passing* test output and show failed tests
  output),
- `--tb=short`, `--tb=long`, `--tb=no`, set size of python trace back in case
 of failure. Default is `long` and is too verbose in most case. The
python trace back is useful to detect bugs in the python scripts,
- `--log-dir=<dir>` saves all servers log in the given dir (CREATE `<DIR>` FIRST).
- `-x --pdb`, start
python debugger at first failure, this allows interacting with the node in
the same context of the test,
- `-m TAGS_EXPR`, run all tests containing some combination of tags.

`-v` and `--tb=short` are set by default in `pytest` initialization files.

#### Tags

Tests are classified using tags. They are added with the annotation
```
@pytest.mark.TAG
```
Currently, `TAGS` can be `vote`, `multinode`, `baker`, `endorser`, `contract`,
 `slow`, `multibranch`. The configuration file `pytest.ini` defines the list
 of allowed tags.

#### Examples

There are usually two ways of using `pytest`:
- run a subset of the tests (batch mode),
- or run a specific test.

In batch mode, we usually don't care about traces. No particular option is
needed, but sometimes we want to stop at first failure using `-x`, and some
tests require the server logs to be saved (`--log-dir=tmp/`).

To run a specifc test, we usually want client and server traces
(`-s --log-dir=tmp/`).

```
# Launch a simple test without capturing stdout
> pytest -s examples/test_example.py
# run all tests about vote
> pytest -m "vote"
# run all vote and non-slow tests
> pytest -m "vote and not slow"
# run module test_voting.py, display all output, save server logs in tmp
> pytest -s tests/test_voting.py --log-dir=tmp
# run all tests using a deamon
> pytest -m "endorser or baker"
# run everything
> pytest
```

### Anatomy of a test

A typical testing scenario consists in:
 1. initializing the context (starting servers, setting up clients)
 2. running a sequence of commands and assertions
 3. releasing resources, terminating servers

This is done by grouping tests in a class, and managing the context in
a *fixture*.

The following `test_example.py` is the pytest counterpart of the first example.

```
import pytest
from tools import constants, paths, utils
from launchers.sandbox import Sandbox


@pytest.fixture(scope="class")
def sandbox():
    """Example of sandbox fixture."""
    with Sandbox(paths.TEZOS_HOME,
                 constants.IDENTITIES,
                 constants.GENESIS_PK) as sandbox:
        sandbox.add_node(0)
        utils.activate_alpha(sandbox.client(0))
        sandbox.add_node(1)
        sandbox.add_baker(0, 'bootstrap5', proto=constants.ALPHA_DEAMON)
        yield sandbox
        assert sandbox.are_daemons_alive()


@pytest.fixture(scope="class")
def session():
    """Example of dictionary fixture. Used for keeping data between tests."""
    yield {}


@pytest.mark.incremental
class TestExample:

    def test_wait_sync_proto(self, sandbox):
        clients = sandbox.all_clients()
        for client in clients:
            proto = "ProtoALphaALphaALphaALphaALphaALphaALphaALphaDdp3zK"
            assert utils.check_protocol(client, proto)

    def test_transfer(self, sandbox, session):
        receipt = sandbox.client(0).transfer(500, 'bootstrap1', 'bootstrap3')
        session['operation_hash'] = receipt.operation_hash

    def test_inclusion(self, sandbox, session):
        operation_hash = session['operation_hash']
        sandbox.client(0).wait_for_inclusion(operation_hash)
```

In this example, we defined the fixtures in the same module, but they are
generally shared between tests and put in `conftest.py`.

Currently, all tests scenarios in the test suite are defined as classes,
consisting of a sequence of methods that are run incrementally (as
specified with the annotation `@pytest.mark.incremental`). Classes are
used to define the scope of a fixture, and a unit of incremental
testing sequence. We don't directly instanciate them, or use `self`.

Data between methods are shared using a dictionary `session`. For instance,
we save the result of the `transfer` operation, and retrieve it in the next
method.

### Fixtures

The list of fixtures available is given by
```
pytest --fixtures
```
The most general fixture is `sandbox`. It allows to instanciate an arbitrary
number of nodes and deamons. Other fixtures (such as `client` in
`conftest.py`) are specialized versions (slightly more convenient than using
`sandbox` directly). Fixtures can be defined directly in a module defining a
test, or they can be shared (most fixtures are defined in `conftest.py`).

### Skipping tests

Sometimes, a test can't be run. For instance, it is known to fail, or it
relies on some resources that may not be available. In that case, the test
can be skipped (instead of failing).

For instance, if no log dir has been specified, the check_logs tests are
skipped using `pytest.skip()`.

```
def test_check_logs(self, sandbox):
        if not sandbox.log_dir:
            pytest.skip()
```

Alternatively, one can use the `skip` annotation:
```
@pytest.mark.skip(reason="Not yet implemented")
```

### Adding a test

* By imitation, choose an existing test that looks similar,
* Use the proper tags,
* Say briefly what the test is supposed to test in the class docstring,
* *Run the linters* and typechecker `make lint_all`,
* If you modify the launchers or the deamons, make sure they don't
  depend on testing constants (`tools/constant.py` or `tools/paths.py`)
  in order to keep the tests separated from the infrastructure that runs
  them.

### Testing on `zeronet`, `alphanet`,...

On `master`, protocol alpha is named `ProtoALphaALphaALphaALphaALphaALphaALphaALphaDdp3zK`, and deamons binary
name are suffixed with `alpha` (`tezos-baker-alpha`,
`tezos-endorser-alpha`...). However, on *production* branches, an actual hash
of the protocol is used, and a shortened string is used to specify deamons.

For instance, on revision `816625bed0983f7201e4c369440a910f006beb1a` of
zeronet, protocal alpha is named `PsddFKi32cMJ2qPjf43Qv5GDWLDPZb3T3bF6fLKiF5HtvHNU7aP`
and deamons are suffixed by `003-PsddFKi3` (`tezos-baker-003-PsddFKi3`).

To reduce coupling between tests and the actual branch to be tested, tests
refer to protocol alpha using `constants.ALPHA` and `constants.ALPHA_DEAMON`
rather than by hard-coded identifiers.

### Tests based on fixed revisions (multibranch)

It is useful to test interactions between different server versions. There
are currently two ways of doing this.

1. The `Sandbox` launcher can use binaries built from different revisions.
Methods `add_node`, `add_baker` and `add_endorser` have an optional parameter
`branch` that points to a subdirectory where binaries are to be looked for.

2. The `SandboxMultibranch` launcher is instanciated by map from ids to
branches. Then everytime we launch a node or a deamon the actual binary will be
selected according to the map.

Tests using specific revisions are in `tests/multibranch` and aren't run
by default. They are not regression tests and are usually launched
separately from the rest of the tests. To run these tests, you need
to set up the `TEZOS_BINARIES` environment variable to a directory
that contains the binaries for all revisions needed by test (see below).
The tests will be skipped if this variable isn't set, and fail
if the binaries aren't available.

#### Building binaries for several revisions

Before running the script, the user has to build the binaries and
copy them to the right location. This can be done by the `scripts/build_branches.py` script.

For instance, suppose we want to build binaries for two different
 revisions of zeronet:
```
A = b8de4297db6a681eb13343d2773c6840969a5537
B = 816625bed0983f7201e4c369440a910f006beb1a
```

```
TEZOS_HOME=~/tezos  # TEZOS repo, read-only access from the script
TEZOS_BINARIES=~/tezos-binaries  # where the binaries will be stored
TEZOS_BUILD=~/tmp/tezos_tmp  # where the binaries will be built
```

The following command will generate binaries for each of the specified
branches in `TEZOS_BINARIES`.

```
scripts/build_branches.py --clone $TEZOS_HOME --build-dir $TEZOS_BUILD \
                          --bin-dir $TEZOS_BINARIES \
                          b8de4297db6a681eb13343d2773c6840969a5537 \
                          816625bed0983f7201e4c369440a910f006beb1a
```

```
> ls $TEZOS_BINARIES *
816625bed0983f7201e4c369440a910f006beb1a:
tezos-accuser-003-PsddFKi3  tezos-baker-004-Pt24m4xi    tezos-node
tezos-accuser-004-Pt24m4xi  tezos-client                tezos-protocol-compiler
tezos-admin-client          tezos-endorser-003-PsddFKi3 tezos-signer
tezos-baker-003-PsddFKi3    tezos-endorser-004-Pt24m4xi

b8de4297db6a681eb13343d2773c6840969a5537:
tezos-accuser-003-PsddFKi3  tezos-baker-004-Pt24m4xi    tezos-node
tezos-accuser-004-Pt24m4xi  tezos-client                tezos-protocol-compiler
tezos-admin-client          tezos-endorser-003-PsddFKi3 tezos-signer
tezos-baker-003-PsddFKi3    tezos-endorser-004-Pt24m4xi
```

Note: One can specify a branch instead of a revision but this is error-prone.
For instance, protocols may have different hashes on different revisions
on the same branch, and these hashes are typically hard-coded in the tests to
activate the protocols.

#### Example 1: `test_baker_endorser_mb.py`

The test `test_baker_endorser_mb.py` uses two different revisions.

the `sandbox_multibranch` fixtures (which uses the `SandboxMultibranch` launcher)
 parameterized by a map that alternates between the two revisions.

The executables will be selected from revisions A and B as specified by:
```
A = "d272059bf474018d0c39f5a6e60634a95f0c44aa" # MAINNET
B = "6718e80254d4cb8d7ad86bce8cf3cb692550c6e7"  # MAINNET SNAPSHOT
MAP = {i:A if i % 2 == 0 else B  for i in range(20)}
@pytest.mark.parametrize('sandbox_multibranch', [MAP], indirect=True)
```

Run the test with
```
# mkdir tmp
pytest tests/multibranch/test_baker_endorser_mb.py --log-dir=tmp
```

#### Example 2: A full voting scenario `test_voting_full.py`

This tests uses binaries from revision
 `b8de4297db6a681eb13343d2773c6840969a5537` and implements a full
 voting scenario (voting, launching a test chain and a test chain baker,
  upgrading to a new protocol, performing operations on the new protocol).

It uses two protocols implemented by this specific revisions,
```
ALPHA = 'PsddFKi32cMJ2qPjf43Qv5GDWLDPZb3T3bF6fLKiF5HtvHNU7aP'
NEW_PROTO = 'Pt24m4xiPbLDhVgVfABUjirbmda3yohdN82Sp9FeuAXJ4eV9otd'
```
as well the corresponding bakers 'tezos-baker-003-PsddFKi3' 'tezos-baker-004-Pt24m4xi'.

```
scripts/build_branches.py --clone $TEZOS_HOME --build-dir $TEZOS_BUILD \
       --bin-dir $TEZOS_BINARIES \ b8de4297db6a681eb13343d2773c6840969a5537
```
It can be run with
```
pytest tests/multibranch/test_baker_endorser_mb.py`
```

Note: this test uses only one revision but it can't run
on branch `master` as we need an extra protocol with bakers.

## TODO

Mostly little improvements

* Many `client` methods and `client_output` classes haven't been implemented yet,
* Before using the CI, make sure tests are reproductible and aren't sensitive
  on timing assumption
    * more consistent use of retries
    * deduce time-constant from block baking period
    * beware of timeout (see known issues)
* More launchers (i.e. zeronet, history mode)
* Use parametric fixtures more consistently: one can relaunch the same tests
  with different parameters such as the number of peers.
* Finish porting of bash scripts (if we intend to replace them)
* there maybe a simpler way of implementing `incremental` tests, without
relying on classes to group tests (it adds unnecessary overhead since we have
mostly one class per module). Currently, we use
what is recommended in pytest
 [documentation](https://docs.pytest.org/en/latest/example/simple.html#incremental-testing-test-steps)
but there may be a better way.
* improve typing annotations and `mypy` (typechecker) usage. Currently, only
 part of the code is type-checked.
* make deamons classes more generic. In the first implementations,
  servers were launched only in sandbox mode so default parameters still
  reflect that. It would be better to have server/client generic and fill the
  right parameters in the launcher. This will be necessary to define other
  launchers.

## Known issues

* There are some tests that fail spuriously, in particular
. `test_baker_endorser.py::TestAllDeamonsWithOperations::test_check_operations`
. `test_contract_baker.py::TestOriginationCall::test_call`
. `test_voting_full.py::TestMultiNode::test_contains_transfer`
The cause remains to be determined.

* On rare occasions, some servers may not be properly killed upon test
termination. This can lead to a message `Fitness too low` from the client.

* One some occasions, the `timeout` marker doesn't play well with
  blocking client commmands. for instance, this may not stop the test if
 `wait_for_inclusion` is stuck.
```
@pytest.mark.timeout(5)
def test_inclusion(self, sandbox, session):
    operation_hash = session['operation_hash']
    sandbox.client(0).wait_for_inclusion(operation_hash)
```

The `thread` methods terminates the test but the resources aren't properly
cleaned up.
```
@pytest.mark.timeout(5, method='thread')
```
See discussion (here)[https://pypi.org/project/pytest-timeout/].

To avoid this issue, one can use polling functions
 such as `utils.check_contains_operations(client, [op_hash])`
  instead of using blocking commands.

* Problems of modules imports such as

```
ImportError while loading conftest '/home/philippe/tezos/tests_python/tests/conftest.py'.
tests/conftest.py:10: in <module>
    from launchers.sandbox import Sandbox, SandboxMultiBranch
E   ModuleNotFoundError: No module named 'launchers'
```
can be solved by setting `PYTHONPATH`
```
export PYTHONPATH=`YOUR_TEST_DIR/tests_python:$PYHONPATH
```
