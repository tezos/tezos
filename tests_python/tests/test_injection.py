import os
import subprocess
import time
import pytest
from tools import utils, paths


@pytest.fixture(scope="class")
def clients(sandbox):
    """Launches 2 nodes in sandbox mode (genesis, doesn't activate alpha)."""
    num_nodes = 2
    for i in range(num_nodes):
        sandbox.add_node(i, params=['--connections', '30'])
    yield sandbox.all_clients()


PROTO = f'{paths.TEZOS_HOME}/src/bin_client/test/proto_test_injection'
COMPILER = (f'{paths.TEZOS_HOME}/_build/default/src/lib_protocol_compiler/'
            'main_native.exe')


@pytest.mark.incremental
class TestInjectionAndActivation:
    """Protocol injection and activation"""

    def test_check_resources(self):
        assert os.path.isfile(COMPILER)
        assert os.path.isdir(PROTO)

    def test_compute_hash(self, session):
        cmd = [COMPILER, '-hash-only', PROTO]
        res = subprocess.run(cmd, universal_newlines=True, check=True,
                             stdout=subprocess.PIPE)
        proto_hash = res.stdout[:-1]
        assert len(proto_hash) == 51
        session['proto_hash'] = proto_hash

    def test_injection(self, clients):
        clients[0].inject_protocol(PROTO)

    def test_check_injected(self, clients, session):
        proto = session['proto_hash']
        protos = clients[0].list_protocols()
        assert proto in protos

    def test_activation(self, clients, session):
        proto = session['proto_hash']
        parameters = {}
        res = clients[0].activate_protocol_json(proto, parameters,
                                                key='activator', fitness='1')
        assert res.block_hash

    def test_check_protocol(self, clients, session):
        proto = session['proto_hash']
        params = ['-p', 'ProtoGenesisGenesisGenesisGenesisGenesisGenesk612im']
        for client in clients:
            assert utils.check_protocol(client, proto, params=params)


@pytest.fixture(scope="class")
def client(sandbox):
    """One node in sandbox mode (genesis, doesn't activate alpha)."""
    sandbox.add_node(0)
    client = sandbox.client(0)
    yield client


@pytest.mark.incremental
class TestActivation:
    """ Protocol activation (protocol already linked to the node) """

    def test_proto_known(self, client):
        res = client.list_protocols()
        assert 'ProtoDemoDemoDemoDemoDemoDemoDemoDemoDemoDemoD3c8k9' in res

    def test_first_protocol(self, client):
        proto = 'PrihK96nBAFSxVL1GLJTVhu9YnzkMFiBeuJRPA8NwuZVZCE1L6i'
        assert client.get_protocol() == proto

    def test_activate_demo(self, client):
        proto = 'ProtoDemoDemoDemoDemoDemoDemoDemoDemoDemoDemoD3c8k9'
        parameters = {}
        res = client.activate_protocol_json(proto, parameters, key='activator',
                                            fitness='1')
        assert res.block_hash

    def test_level1(self, client):
        assert client.get_level() == 1

    def test_protocol_genesis(self, client):
        proto = 'ProtoGenesisGenesisGenesisGenesisGenesisGenesk612im'
        assert client.get_protocol() == proto

    def test_bake(self, client):
        time.sleep(0.5)
        # this a command of proto demo lib_client
        res = client.run(['bake'])
        assert res.startswith('Injected')

    def test_level2(self, client):
        assert client.get_level() == 2
