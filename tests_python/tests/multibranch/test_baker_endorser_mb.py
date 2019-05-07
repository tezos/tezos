r"""
Before running this test, make sure that the environment variable
`TEZOS_BINARIES` is defined, and that it points to a properly
set up directory. The script `scripts/build_branches.py` can be used
to build the binaries.

Example:

  TEZOS_HOME=~/tezos
  TEZOS_BINARIES=~/tmp/tezos_binaries
  TEZOS_BUILD=~/tmp/tezos_build
  scripts/build_branches.py --clone $TEZOS_HOME --build-dir $TEZOS_BUILD \
            --bin-dir $TEZOS_BINARIES \
            d272059bf474018d0c39f5a6e60634a95f0c44aa \
            6718e80254d4cb8d7ad86bce8cf3cb692550c6e7

Note: the test relies (only) on `TEZOS_BINARIES`. The test will be be skipped
if the environment variable isn't defined.

TODO this test is a copy and paste from tests/test_baker_endorser.py
     we could try to somehow re-use the same code with a different fixture.
"""


import itertools
import random
import time
import subprocess
import pytest
from tools import utils, constants

random.seed(42)
KEYS = [f'bootstrap{i}' for i in range(1, 6)]
NEXT_KEY = itertools.cycle(KEYS)
NUM_NODES = 5
NEW_NODES = 5
REPLACE = False
NUM_CYCLES = 60
TIME_BETWEEN_CYCLE = 1
assert NEW_NODES <= NUM_CYCLES


def random_op(client):
    sender = next(NEXT_KEY)
    dest = random.choice([key for key in KEYS if key != sender])
    amount = random.randrange(10000)
    return client.transfer(amount, sender, dest)


# The executables will be selected from revisions A and B
A = "d272059bf474018d0c39f5a6e60634a95f0c44aa"  # MAINNET
B = "6718e80254d4cb8d7ad86bce8cf3cb692550c6e7"  # MAINNET SNAPSHOT
# The MAP parameter specifies where the binaries should be looked up
MAP = {i: A if i % 2 == 0 else B for i in range(20)}


# A and B use the same protocol but it may not be the case
ALPHA = 'PsddFKi32cMJ2qPjf43Qv5GDWLDPZb3T3bF6fLKiF5HtvHNU7aP'
ALPHA_DEAMON = '003-PsddFKi3'


def params(i):
    params = ['--connections', '500']
    extra = ['--history-mode', 'full'] if i % 2 == 1 else []
    return params + extra


@pytest.mark.parametrize('sandbox_multibranch', [MAP], indirect=True)
@pytest.mark.baker
@pytest.mark.endorser
@pytest.mark.multinode
@pytest.mark.slow
@pytest.mark.multibranch
@pytest.mark.incremental
class TestAllDeamonsWithOperations:
    '''Runs two baker and two endorsers, generates random op, and
       add (or replace) new nodes dynamically. After a little while,
       we kill the bakers and check everyone synchronize to the same head. '''

    def test_setup_network(self, sandbox_multibranch):
        # Set appropriate time to avoid double-baking
        for i in range(NUM_NODES):
            sandbox_multibranch.add_node(i, params=params(i))
        parameters = dict(constants.PARAMETERS)
        parameters["time_between_blocks"] = ["10", "0"]
        sandbox_multibranch.client(0).activate_protocol_json(ALPHA, parameters)
        sandbox_multibranch.add_baker(0, 'bootstrap5',
                                      proto=ALPHA_DEAMON)
        sandbox_multibranch.add_baker(1, 'bootstrap4',
                                      proto=ALPHA_DEAMON)
        sandbox_multibranch.add_endorser(0, account='bootstrap1',
                                         endorsement_delay=1,
                                         proto=ALPHA_DEAMON)
        sandbox_multibranch.add_endorser(1, account='bootstrap2',
                                         endorsement_delay=1,
                                         proto=ALPHA_DEAMON)

    def test_wait_for_alpha(self, sandbox_multibranch):
        clients = sandbox_multibranch.all_clients()
        for client in clients:
            proto = ALPHA
            assert utils.check_protocol(client, proto)

    def test_network_gen_operations_and_add_nodes(self, sandbox_multibranch,
                                                  session):
        node_add_period = NUM_CYCLES // NEW_NODES
        for cycle in range(NUM_CYCLES):
            i = random.randrange(NUM_NODES)
            client = sandbox_multibranch.client(i)
            try:
                transfer = random_op(client)
                session[f'op{cycle}'] = transfer.operation_hash
            except subprocess.CalledProcessError:
                # some operations may be invalid, e.g. the client sends
                # several operation with the same counter
                print('# IGNORED INVALID OPERATION')

            if cycle % node_add_period == 0:
                # add node
                running_nodes = list(sandbox_multibranch.nodes.keys())
                new_node = max(running_nodes) + 1
                if REPLACE:
                    running_nodes.remove(0)
                    running_nodes.remove(1)
                    sandbox_multibranch.rm_node(random.choice(running_nodes))
                sandbox_multibranch.add_node(
                    new_node,
                    params=params(new_node))
                proto = ALPHA
                assert utils.check_protocol(
                    sandbox_multibranch.client(new_node),
                    proto)
            time.sleep(TIME_BETWEEN_CYCLE)

    def test_kill_baker(self, sandbox_multibranch):
        sandbox_multibranch.rm_baker(0, proto=ALPHA_DEAMON)
        sandbox_multibranch.rm_baker(1, proto=ALPHA_DEAMON)

    def test_synchronize(self, sandbox_multibranch):
        utils.synchronize(sandbox_multibranch.all_clients())

    def test_check_operations(self, sandbox_multibranch):
        min_level = min([client.get_level()
                         for client in sandbox_multibranch.all_clients()])
        heads_hash = set()
        # check there is exactly one head
        for client in sandbox_multibranch.all_clients():
            block_hash = utils.get_block_hash(client, min_level)
            heads_hash.add(block_hash)
        assert len(heads_hash) == 1
        # TODO check for operations inclusion

    def test_check_logs(self, sandbox_multibranch):
        if not sandbox_multibranch.log_dir:
            pytest.skip()
        assert sandbox_multibranch.logs
        # TODO check more things in the log! endorsement, baking...
        error_pattern = r"Uncaught|registered"
        assert utils.check_logs(sandbox_multibranch.logs, error_pattern)
