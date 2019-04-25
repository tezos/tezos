import random
import time
import pytest
from tools import utils, constants

NUM_NODES = 2
NEW_NODES = 2
REPLACE = False
ERROR_PATTERN = r"Uncaught|registered"
PARAMS = ['--connections', '500']


@pytest.mark.baker
@pytest.mark.multinode
@pytest.mark.slow
@pytest.mark.incremental
class TestManyNodesBootstrap:
    """Run many nodes, wait a while, run more nodes, check logs"""

    def test_init(self, sandbox):
        sandbox.add_node(0, params=PARAMS)
        parameters = dict(constants.PARAMETERS)
        parameters["time_between_blocks"] = ["1", "0"]
        utils.activate_alpha(sandbox.client(0), parameters)
        sandbox.add_baker(0, 'bootstrap1', proto=constants.ALPHA_DEAMON)
        sandbox.add_node(1, params=PARAMS)
        sandbox.add_baker(1, 'bootstrap2', proto=constants.ALPHA_DEAMON)

    def test_add_nodes(self, sandbox):
        for i in range(2, NUM_NODES):
            sandbox.add_node(i, params=PARAMS)

    def test_sleep_10s(self):
        time.sleep(10)

    def test_add_more_nodes(self, sandbox):
        new_node = NUM_NODES
        for i in range(NEW_NODES):
            if REPLACE:
                running_nodes = list(sandbox.nodes.keys())
                running_nodes.remove(0)
                running_nodes.remove(1)
                sandbox.rm_node(random.choice(running_nodes))
            sandbox.add_node(new_node + i, params=PARAMS)

    def test_kill_baker(self, sandbox):
        assert utils.check_logs(sandbox.logs, ERROR_PATTERN)
        sandbox.rm_baker(0, proto=constants.ALPHA_DEAMON)
        sandbox.rm_baker(1, proto=constants.ALPHA_DEAMON)

    def test_synchronize(self, sandbox):
        utils.synchronize(sandbox.all_clients())

    def test_check_logs(self, sandbox):
        if not sandbox.log_dir:
            pytest.skip()
        assert sandbox.logs
        assert utils.check_logs(sandbox.logs, ERROR_PATTERN)
