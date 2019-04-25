"""
This test relies on a specific revision of ZERONET. The required
binaries need to be built with

  scripts/build_branches.py --clone $TEZOS_HOME --build-dir $TEZOS_BUILD \
                            --bin-dir $TEZOS_BINARIES \
                            b8de4297db6a681eb13343d2773c6840969a5537

The test will be be skipped `TEZOS_BINARIES` is not set.

This revision of zeronet implements two versions of protocol alpha,

  ALPHA = 'PsddFKi32cMJ2qPjf43Qv5GDWLDPZb3T3bF6fLKiF5HtvHNU7aP'
  NEW_PROTO = 'Pt24m4xiPbLDhVgVfABUjirbmda3yohdN82Sp9FeuAXJ4eV9otd'

This test tests the voting for NEW_PROTO (and subsequent activation)
from ALPHA.
"""
import time
import pytest
from tools import utils, constants
BAKE_ARGS = ['--minimal-fees', '0', '--minimal-nanotez-per-byte', '0',
             '--minimal-nanotez-per-gas-unit', '0', '--max-priority', '512',
             '--minimal-timestamp']
ERROR_PATTERN = r"Uncaught|registered|error"
NODE_PARAMS = ['--connections', '500', '--enable-testchain']
PARAMETERS = dict(constants.PARAMETERS)
PARAMETERS["time_between_blocks"] = ["1", "0"]
PARAMETERS["blocks_per_voting_period"] = 20

# All executables will be selected from revision ZERONET
ZERONET = "b8de4297db6a681eb13343d2773c6840969a5537"
MAP = {i: ZERONET for i in range(20)}
ALPHA = 'PsddFKi32cMJ2qPjf43Qv5GDWLDPZb3T3bF6fLKiF5HtvHNU7aP'
ALPHA_DEAMON = '003-PsddFKi3'
NEW_PROTO = 'Pt24m4xiPbLDhVgVfABUjirbmda3yohdN82Sp9FeuAXJ4eV9otd'
NEW_PROTO_BAKER = "004-Pt24m4xi"


@pytest.mark.parametrize('sandbox_multibranch', [MAP], indirect=True)
@pytest.mark.multibranch
@pytest.mark.vote
@pytest.mark.slow
@pytest.mark.baker
@pytest.mark.incremental
class TestMultiNode:

    def test_start_nodes_and_baker(self, sandbox_multibranch):
        sandbox_multibranch.add_node(0, params=NODE_PARAMS)
        sandbox_multibranch.client(0).activate_protocol_json(ALPHA, PARAMETERS)
        sandbox_multibranch.add_node(1, params=NODE_PARAMS)
        sandbox_multibranch.add_node(2, params=NODE_PARAMS)
        sandbox_multibranch.add_node(3, params=NODE_PARAMS)
        sandbox_multibranch.add_baker(0, 'bootstrap5', proto=ALPHA_DEAMON)

    def test_wait_for_alpha(self, sandbox_multibranch):
        clients = sandbox_multibranch.all_clients()
        for client in clients:
            assert utils.check_protocol(client, ALPHA)

    @pytest.mark.timeout(60)
    def test_wait_second_proposal_period(self, sandbox_multibranch):
        """make sure we skip the first proposal period, there's a bug
           that prevents to make proposals there"""
        client = sandbox_multibranch.client(0)
        block_voting_period = PARAMETERS['blocks_per_voting_period']
        while client.get_level() <= block_voting_period:
            time.sleep(5)

    # protocol already linked to the node, no need for injection
    # def test_inject_proto(self, sandbox_multibranch):
    #     NEW_PROTO_SRC = "004_Pt24m4xi"
    #     client = sandbox_multibranch.client(0)
    #     assert client.get_current_period_kind() == 'proposal'
    #     proto = f'{paths.TEZOS_HOME}/src/proto_{NEW_PROTO_SRC}/lib_protocol/'
    #     assert os.path.isdir(proto), f"can't find {proto}"
    #     Protocol already injected
    #     client.inject_protocol(proto)

    def test_retrieve_protos(self, sandbox_multibranch):
        client = sandbox_multibranch.client(0)
        protos = client.list_protocols()
        assert NEW_PROTO in protos

    def test_submit_proposals(self, sandbox_multibranch, session):
        client = sandbox_multibranch.client(0)
        assert client.get_current_period_kind() == 'proposal'
        proposals = client.submit_proposals('bootstrap1', [NEW_PROTO])
        session['prop_hash'] = proposals.operation_hash

    def test_wait_for_protocol_inclusion(self, sandbox_multibranch, session):
        client = sandbox_multibranch.client(0)
        client.wait_for_inclusion(session['prop_hash'])

    def test_check_proposals(self, sandbox_multibranch):
        client = sandbox_multibranch.client(0)
        proposals = client.get_proposals()
        assert NEW_PROTO in [proto for (proto, _) in proposals]

    @pytest.mark.timeout(60)
    def test_wait_for_vote(self, sandbox_multibranch):
        client = sandbox_multibranch.client(0)
        while client.get_current_period_kind() != 'testing_vote':
            time.sleep(2)

    def test_vote(self, sandbox_multibranch):
        client = sandbox_multibranch.client(0)
        for i in range(1, 5):
            client.submit_ballot(f'bootstrap{i}', NEW_PROTO, 'yay')

    @pytest.mark.timeout(60)
    def test_wait_for_testing(self, sandbox_multibranch):
        for client in sandbox_multibranch.all_clients():
            while client.get_current_period_kind() != 'testing':
                client.get_level()
                time.sleep(2)

    def test_all_testing(self, sandbox_multibranch):
        for client in sandbox_multibranch.all_clients():
            assert client.get_current_period_kind() == 'testing'

    def test_start_beta_baker_test(self, sandbox_multibranch):
        sandbox_multibranch.add_baker(0, 'bootstrap4', proto=NEW_PROTO_BAKER,
                                      params=['--chain', 'test'])

    def test_add_new_node(self, sandbox_multibranch):
        sandbox_multibranch.add_node(4, params=NODE_PARAMS)
        client = sandbox_multibranch.client(4)
        while client.get_current_period_kind() != 'testing':
            client.get_level()
            time.sleep(2)

    def test_rpc_in_test_period(self, sandbox_multibranch):
        for client in sandbox_multibranch.all_clients():
            test_id = client.rpc('get', 'chains/test/chain_id')
            main_id = client.rpc('get', 'chains/main/chain_id')
            assert test_id != main_id

    def test_transfer_in_test_period(self, sandbox_multibranch):
        client = sandbox_multibranch.client(0)
        cmd = '--chain test transfer 10 from bootstrap1 to bootstrap2'.split()
        client.run(cmd)

    def test_testchain_increases(self, sandbox_multibranch):
        client = sandbox_multibranch.client(0)
        res = client.rpc('get', '/chains/test/blocks/head/header/shell')
        level_before = res['level']
        assert utils.check_level_greater_than(client, level_before + 1)

    @pytest.mark.timeout(60)
    def test_wait_for_promotion_vote(self, sandbox_multibranch):
        client = sandbox_multibranch.client(0)
        while client.get_current_period_kind() != 'promotion_vote':
            client.rpc('get', '/chains/main/blocks/head/header/shell')
            time.sleep(2)

    def test_vote_again(self, sandbox_multibranch):
        client = sandbox_multibranch.client(0)
        for i in range(1, 5):
            client.submit_ballot(f'bootstrap{i}', NEW_PROTO, 'yay')

    @pytest.mark.timeout(60)
    def test_wait_for_proposal(self, sandbox_multibranch):
        client = sandbox_multibranch.client(1)
        while client.get_level() < 100:
            client.rpc('get', '/chains/main/blocks/head/header/shell')
            time.sleep(2)

    def test_start_beta_baker(self, sandbox_multibranch):
        sandbox_multibranch.add_baker(1, 'bootstrap3', proto=NEW_PROTO_BAKER)

    @pytest.mark.timeout(40)
    def test_new_protocol(self, sandbox_multibranch):
        all_have_proto = False
        while not all_have_proto:
            clients = sandbox_multibranch.all_clients()
            all_have_proto = all(client.get_protocol() == NEW_PROTO
                                 for client in clients)
            time.sleep(2)

    def test_stop_old_bakers(self, sandbox_multibranch):
        sandbox_multibranch.rm_baker(0, ALPHA_DEAMON)
        sandbox_multibranch.rm_baker(0, NEW_PROTO_BAKER)
        time.sleep(1)

    def test_level_increases(self, sandbox_multibranch):
        client = sandbox_multibranch.client(0)
        res = client.rpc('get', '/chains/main/blocks/head/header/shell')
        level_before = res['level']
        assert utils.check_level_greater_than(client, level_before + 1)

    def test_all_proposal(self, sandbox_multibranch):
        for client in sandbox_multibranch.all_clients():
            assert client.get_current_period_kind() == 'proposal'

    def test_chain_id_new_proto(self, sandbox_multibranch):
        for client in sandbox_multibranch.all_clients():
            client.rpc('get', 'chains/main/chain_id')
            client.rpc('get', 'chains/test/chain_id')
            client.get_level()

    def test_transfer_new_proto(self, sandbox_multibranch, session):
        client = sandbox_multibranch.client(0)
        transfer = client.transfer(10, 'bootstrap1', 'bootstrap2')
        session["transfer_hash"] = transfer.operation_hash

    def test_still_level_increases(self, sandbox_multibranch):
        client = sandbox_multibranch.client(0)
        res = client.rpc('get', '/chains/main/blocks/head/header/shell')
        level_before = res['level']
        assert utils.check_level_greater_than(client, level_before + 1)

    def test_contains_transfer(self, sandbox_multibranch, session):
        transfer_hash = session["transfer_hash"]
        client = sandbox_multibranch.client(2)
        assert utils.check_operation_in_receipt(client, transfer_hash)

    def test_balance(self, sandbox_multibranch):
        client = sandbox_multibranch.client(2)
        balance = client.get_balance('bootstrap2')
        assert balance == 4000010

    def test_check_logs(self, sandbox_multibranch):
        assert utils.check_logs(sandbox_multibranch.logs, ERROR_PATTERN)
