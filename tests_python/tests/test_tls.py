import pytest
from tools import constants


@pytest.fixture(scope="class")
def client(sandbox):
    sandbox.add_node(0, use_tls=(constants.TEZOS_CRT, constants.TEZOS_KEY))
    yield sandbox.client(0)


@pytest.mark.vote
@pytest.mark.incremental
class TestTLS:
    """Test voting protocol with manual baking, 4 blocks per voting period."""

    def test_bootstrapped(self, client):
        assert client.bootstrapped()
