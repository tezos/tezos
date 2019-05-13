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


@pytest.mark.baker
@pytest.mark.endorser
@pytest.mark.multinode
@pytest.mark.slow
@pytest.mark.incremental
class TestAllDeamonsWithOperations:
    '''Runs two baker and two endorsers, generates random op, and
       add (or replace) new nodes dynamically. After a little while,
       we kill the bakers and check everyone synchronize to the same head. '''

    def test_setup_network(self, sandbox):
        # Set appropriate time to avoid double-baking
        parameters = dict(constants.PARAMETERS)
        parameters["time_between_blocks"] = ["15", "0"]
        for i in range(NUM_NODES):
            sandbox.add_node(i, params=['--connections', '500'])
        utils.activate_alpha(sandbox.client(0), parameters)
        sandbox.add_baker(0, 'bootstrap5', proto=constants.ALPHA_DEAMON)
        sandbox.add_baker(1, 'bootstrap4', proto=constants.ALPHA_DEAMON)
        sandbox.add_endorser(0, account='bootstrap1', endorsement_delay=1,
                             proto=constants.ALPHA_DEAMON)
        sandbox.add_endorser(1, account='bootstrap2', endorsement_delay=1,
                             proto=constants.ALPHA_DEAMON)

    def test_wait_for_alpha(self, sandbox):
        clients = sandbox.all_clients()
        for client in clients:
            proto = constants.ALPHA
            assert utils.check_protocol(client, proto)

    def test_network_gen_operations_and_add_nodes(self, sandbox, session):
        node_add_period = NUM_CYCLES // NEW_NODES
        for cycle in range(NUM_CYCLES):
            i = random.randrange(NUM_NODES)
            client = sandbox.client(i)
            try:
                transfer = random_op(client)
                session[f'op{cycle}'] = transfer.operation_hash
            except subprocess.CalledProcessError:
                # some operations may be invalid, e.g. the client sends
                # several operation with the same counter
                print('# IGNORED INVALID OPERATION')

            if cycle % node_add_period == 0:
                # add node
                running_nodes = list(sandbox.nodes.keys())
                new_node = max(running_nodes) + 1
                if REPLACE:
                    running_nodes.remove(0)
                    running_nodes.remove(1)
                    sandbox.rm_node(random.choice(running_nodes))
                sandbox.add_node(new_node,
                                 params=['--connections', '500'])
                proto = constants.ALPHA
                assert utils.check_protocol(sandbox.client(new_node), proto)
            time.sleep(TIME_BETWEEN_CYCLE)

    def test_kill_baker(self, sandbox):
        sandbox.rm_baker(0, proto=constants.ALPHA_DEAMON)
        sandbox.rm_baker(1, proto=constants.ALPHA_DEAMON)

    def test_synchronize(self, sandbox):
        utils.synchronize(sandbox.all_clients())

    def test_check_operations(self, sandbox):
        min_level = min([client.get_level()
                         for client in sandbox.all_clients()])
        heads_hash = set()
        # check there is exactly one head
        for client in sandbox.all_clients():
            block_hash = utils.get_block_hash(client, min_level)
            heads_hash.add(block_hash)
        assert len(heads_hash) == 1
        # TODO check for operations inclusion

    def test_check_logs(self, sandbox):
        if not sandbox.log_dir:
            pytest.skip()
        assert sandbox.logs
        # TODO check more things in the log! endorsement, baking...
        error_pattern = r"Uncaught|registered"
        assert utils.check_logs(sandbox.logs, error_pattern)
