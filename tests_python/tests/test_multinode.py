import pytest
from tools import utils


BAKE_ARGS = ['--max-priority', '512', '--minimal-timestamp']


@pytest.mark.multinode
@pytest.mark.parametrize("clients", [3], indirect=True)
@pytest.mark.incremental
class TestManualBaking:
    """
    For n nodes in sandboxed mode, tests:
    . injection of protocol alpha
    . check inclusion of transfer and endorsement operations
    """

    def test_level(self, clients):
        level = 1
        for client in clients:
            assert utils.check_level(client, level)

    def test_bake_and_check_level(self, clients):
        level = 2
        for i in range(1, 6):
            account = f'bootstrap{i}'
            client_i = level % len(clients)
            clients[client_i].bake(account, BAKE_ARGS)
            for client in clients:
                assert utils.check_level(client, level)
            level += 1

    def test_endorse(self, clients, session):
        endorse = clients[2 % len(clients)].endorse('bootstrap3')
        session["endorse_hash"] = endorse.operation_hash

    def test_transfer(self, clients, session):
        client_id = 3 % len(clients)
        transfer = clients[client_id].transfer(500, 'bootstrap1', 'bootstrap3')
        session["transfer_hash"] = transfer.operation_hash

    def test_bake(self, clients):
        clients[3 % len(clients)].bake('bootstrap4', BAKE_ARGS)

    def test_contains_endorse_and_transfer(self, clients, session):
        endorse_hash = session["endorse_hash"]
        transfer_hash = session["transfer_hash"]
        operation_hashes = [endorse_hash, transfer_hash]
        for client in clients:
            assert utils.check_contains_operations(client, operation_hashes)

    def test_balance(self, clients):
        bal = clients[0].get_balance('tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx')
        assert bal == 3998987.998727
