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
            proto = constants.ALPHA
            assert utils.check_protocol(client, proto)

    def test_transfer(self, sandbox, session):
        receipt = sandbox.client(0).transfer(500, 'bootstrap1', 'bootstrap3')
        session['operation_hash'] = receipt.operation_hash

    # @pytest.mark.timeout(5, method='thread')
    def test_inclusion(self, sandbox, session):
        operation_hash = session['operation_hash']
        sandbox.client(0).wait_for_inclusion(operation_hash)
