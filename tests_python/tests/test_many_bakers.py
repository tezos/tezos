import time
import pytest
from tools import utils, constants


PARAMS = ['--connections', '500']

# TODO parameterize test


@pytest.mark.baker
@pytest.mark.multinode
@pytest.mark.slow
@pytest.mark.incremental
class TestManyBakers:
    """Run 5 bakers and num nodes, wait and check logs"""

    def test_init(self, sandbox):
        for i in range(10):
            sandbox.add_node(i, params=PARAMS)
        utils.activate_alpha(sandbox.client(0))
        for i in range(5):
            sandbox.add_baker(i, f'bootstrap{i + 1}',
                              proto=constants.ALPHA_DEAMON)

    def test_wait(self):
        time.sleep(5)

    def test_check_logs(self, sandbox):
        if not sandbox.log_dir:
            pytest.skip()
        assert sandbox.logs
        error_pattern = r"canceled|crashed"
        assert utils.check_logs(sandbox.logs, error_pattern)
