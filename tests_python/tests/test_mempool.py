import time
import pytest
from tools import utils

BAKE_ARGS = ['--max-priority', '512', '--minimal-timestamp']


@pytest.mark.mempool
@pytest.mark.multinode
@pytest.mark.slow
@pytest.mark.incremental
class TestMempool:
    " Tests mempool"

    def test_init(self, sandbox):
        sandbox.add_node(1)
        sandbox.add_node(2)
        sandbox.add_node(3, params=['--disable-mempool'])
        utils.activate_alpha(sandbox.client(1))

    def test_running_prevalidators(self, sandbox):
        assert sandbox.client(1).get_prevalidator()
        assert sandbox.client(2).get_prevalidator()
        assert not sandbox.client(3).get_prevalidator()

    def test_mempool_empty(self, sandbox):
        for i in range(1, 4):
            assert sandbox.client(i).mempool_is_empty()

    def test_transfer(self, sandbox):
        sandbox.client(1).transfer(1.000, 'bootstrap1', 'bootstrap2')

    def test_sleep_3s(self):
        time.sleep(3)

    def test_mempool_include_transfer(self, sandbox):
        assert not sandbox.client(1).mempool_is_empty()
        assert not sandbox.client(2).mempool_is_empty()
        assert sandbox.client(3).mempool_is_empty()

    def test_bake_for(self, sandbox):
        sandbox.client(1).bake('bootstrap1')

    def test_sleep_2s(self):
        time.sleep(2)

    def test_mempools_are_empty(self, sandbox):
        for i in range(1, 4):
            assert sandbox.client(i).mempool_is_empty()

    def test_injection_fails_on_mempool_disabled_node(self, sandbox):
        with pytest.raises(Exception):
            sandbox.client(3).transfer(2.000, 'bootstrap2', 'bootstrap3')
