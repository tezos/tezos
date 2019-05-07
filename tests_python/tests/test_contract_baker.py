import pytest
from tools import paths, utils

BAKE_ARGS = ['--minimal-timestamp']


@pytest.fixture(scope="class")
def client(sandbox):
    """One node running protocol alpha and a baker."""
    sandbox.add_node(0)
    utils.activate_alpha(sandbox.client(0))
    yield sandbox.client(0)


@pytest.mark.contract
@pytest.mark.baker
@pytest.mark.incremental
class TestOriginationCall:
    """Test a simple contract origination and call"""

    def test_originate(self, client, session):
        initial_storage = 'Unit'
        path = f'{paths.TEZOS_HOME}/src/bin_client/test/contracts/opcodes'
        contract = f'{path}/transfer_tokens.tz'
        args = ['--init', initial_storage, '--burn-cap', '0.400']
        origination = client.originate('foobar', 'bootstrap1', 1000,
                                       'bootstrap1', contract, args)
        session['contract'] = origination.contract
        client.bake('bootstrap5', BAKE_ARGS)

        # Unsolved mistery:
        #    client.wait_for_inclusion(origination.operation_hash)
        # fails sometimes with tezos-client crashing. Maybe caused with
        # subprocess captured of forked process output?
        #
        # Safer to poll with `check_contain_operations`
        assert utils.check_contains_operations(client,
                                               [origination.operation_hash])

    def test_call(self, client, session):
        contract = session['contract']
        bootstrap3 = '"tz1faswCTDciRzE4oJ9jn2Vm2dvjeyA9fUzU"'
        transfer = client.transfer(0, 'bootstrap2', contract,
                                   ['--arg', bootstrap3])
        client.bake('bootstrap5', BAKE_ARGS)
        assert utils.check_contains_operations(client,
                                               [transfer.operation_hash])

    def test_balance(self, client):
        assert client.get_balance("bootstrap3") == 4000100

    def test_query_storage(self, client, session):
        contract = session['contract']
        url = f'/chains/main/blocks/head/context/contracts/{contract}/storage'
        res = client.rpc('get', url)
        assert res['prim'] == 'Unit'
