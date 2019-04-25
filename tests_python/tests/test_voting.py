import shutil
import pytest
from tools import paths, constants, utils

BAKE_ARGS = ['--minimal-fees', '0', '--minimal-nanotez-per-byte', '0',
             '--minimal-nanotez-per-gas-unit', '0', '--max-priority', '512',
             '--minimal-timestamp']


@pytest.fixture(scope="class")
def client(sandbox):
    """One snode, 4 blocks per voting period."""
    parameters = dict(constants.PARAMETERS)
    parameters["time_between_blocks"] = ["1", "0"]
    parameters["blocks_per_voting_period"] = 4
    sandbox.add_node(0)
    utils.activate_alpha(sandbox.client(0), parameters)
    yield sandbox.client(0)


@pytest.mark.vote
@pytest.mark.incremental
class TestManualBaking:
    """Test voting protocol with manual baking, 4 blocks per voting period."""

    def test_period_position(self, client):
        assert client.get_period_position() == 1

    def test_listings(self, client):
        assert client.get_listings() == []

    def test_bake_one_block(self, client):
        client.bake('bootstrap1', BAKE_ARGS)

    def test_period_position2(self, client):
        assert client.get_period_position() == 2

    def test_bake_two_blocks(self, client):
        client.bake('bootstrap1', BAKE_ARGS)
        client.bake('bootstrap1', BAKE_ARGS)

    def test_period_position3(self, client):
        assert client.get_period_position() == 0

    def test_listings2(self, client):
        assert client.get_listings() != []

    def test_inject_proto1(self, client, tmpdir):
        proto_fp = (f'{paths.TEZOS_HOME}/src/'
                    f'bin_client/test/proto_test_injection')
        for i in range(1, 4):
            proto = f'{tmpdir}/proto{i}'
            shutil.copytree(proto_fp, proto)
            main = f'{proto}/main.ml'
            print(main)
            with open(main, "a") as file:
                file.write(f'(* {i} *)')
            client.inject_protocol(proto)

    def test_number_proto(self, client, session):
        protos = client.list_protocols()
        assert len(protos) >= 4
        session['protos'] = protos[:4]

    def test_proposal(self, client):
        assert client.get_proposals() == []

    def test_show_voting_period2(self, client):
        client.show_voting_period()

    def test_submit_proposals(self, client, session):
        protos = session['protos']
        client.submit_proposals('bootstrap1', [protos[0]])
        client.submit_proposals('bootstrap2', [protos[0], protos[1]])
        client.submit_proposals('bootstrap3', [protos[1]])
        client.submit_proposals('bootstrap4', [protos[2]])

    def test_bake_one_block2(self, client):
        client.bake('bootstrap1', BAKE_ARGS)

    def test_proposal2(self, client):
        assert client.get_proposals() != []

    def test_bake_one_block3(self, client):
        client.bake('bootstrap1', BAKE_ARGS)

    def test_breaking_tie(self, client, session):
        protos = session['protos']
        client.submit_proposals('bootstrap4', [protos[1]])

    def test_show_voting_period3(self, client):
        client.show_voting_period()

    def test_bake_two_blocks2(self, client):
        client.bake('bootstrap1', BAKE_ARGS)
        client.bake('bootstrap1', BAKE_ARGS)

    def test_period_position4(self, client):
        client.show_voting_period()
        assert client.get_period_position() == 0

    def test_current_period_kind(self, client):
        assert client.get_current_period_kind() == 'testing_vote'

    def test_listings3(self, client):
        assert client.get_listings() != []

    def test_current_proposal(self, client, session):
        expected = session['protos'][1]
        assert expected == client.get_current_proposal()

    def test_submit_ballot(self, client, session):
        proto = session['protos'][1]
        for i in range(1, 4):
            client.submit_ballot(f'bootstrap{i}', proto, 'yay')
        client.submit_ballot(f'bootstrap{4}', proto, 'nay')

    def test_bake_four_blocks(self, client):
        client.bake('bootstrap1', BAKE_ARGS)
        client.bake('bootstrap1', BAKE_ARGS)
        client.bake('bootstrap1', BAKE_ARGS)
        client.bake('bootstrap1', BAKE_ARGS)

    def test_new_period(self, client):
        assert client.get_period_position() == 0
        assert client.get_current_period_kind() == "proposal"
        assert client.get_listings() != '[]'
        # strange behavior here, RPC returns 'null' on stderr
        assert client.get_current_proposal() is None
        assert client.get_ballot_list() == []
