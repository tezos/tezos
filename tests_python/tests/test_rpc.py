import time
import pytest
from tools import utils

BAKE_ARGS = ['--max-priority', '512', '--minimal-timestamp']
CHAIN_ID = "main"
BLOCK_ID = "head"
PKH = "edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav"
PROTOCOL_HASH = "ProtoALphaALphaALphaALphaALphaALphaALphaALphaDdp3zK"
BLOCK_LEVEL = "3"
LIST_OFFSET = "0"
OPERATION_OFFSET = "0"
CONTRACT_ID = ""  # TODO


@pytest.mark.mempool
@pytest.mark.multinode
@pytest.mark.slow
class TestRPCs:
    " Tests RPCs"

    block_hash = ""

    def test_init(self, sandbox):
        sandbox.add_node(1)
        sandbox.add_node(2)
        utils.activate_alpha(sandbox.client(1))
        time.sleep(2)

    def test_bake_for(self, sandbox):
        sandbox.client(1).bake('bootstrap1', BAKE_ARGS)

    def test_network_self(self, sandbox):
        sandbox.client(1).rpc('get', f'/network/self')

    def test_constants(self, sandbox):
        sandbox.client(2).rpc('get', '/network/self')
        sandbox.client(1).bake('bootstrap1', BAKE_ARGS)
        time.sleep(3)

    def test_chain_blocks(self, sandbox):
        sandbox.client(1).rpc('get', f'/chains/{CHAIN_ID}/blocks')

    def test_chain_chain_id(self, sandbox):
        sandbox.client(1).rpc('get', f'/chains/{CHAIN_ID}/chain_id')

    @pytest.mark.skip
    def test_chain_invalid_blocks(self, sandbox):
        sandbox.client(1).rpc('get', f'/chains/{CHAIN_ID}/invalid_blocks')

    @pytest.mark.skip
    def test_chain_invalid_blocks_block_hash(self, sandbox):
        res = sandbox.client(1).bake('bootstrap1', BAKE_ARGS)
        sandbox.client(1).rpc('get',
                              f'/chains/{CHAIN_ID}/invalid_blocks/'
                              f'{res.block_hash}')

    @pytest.mark.skip
    def test_describe(self, sandbox):
        sandbox.client(1).rpc('get', f'/describe')

    def test_errors(self, sandbox):
        sandbox.client(1).rpc('get', '/errors')

    def test_fetch_protocol_protocol_hash(self, sandbox):
        sandbox.client(1).rpc('get', f'/fetch_protocol/{PROTOCOL_HASH}')

    def test_network_connections(self, sandbox):
        sandbox.client(1).rpc('get', f'/network/connections')

    def test_network_connections_peer_id(self, sandbox):
        peer_id = sandbox.client(2).rpc('get', '/network/self')
        sandbox.client(1).rpc('get', f'/network/connections/{peer_id}')

    def test_network_greylist_clear(self, sandbox):
        sandbox.client(1).rpc('get', f'/network/greylist/clear')

    def test_network_peers(self, sandbox):
        sandbox.client(1).rpc('get', f'/network/peers')

    def test_network_peers_peer_id(self, sandbox):
        peer_id = sandbox.client(2).rpc('get', '/network/self')
        sandbox.client(1).rpc('get', f'/network/peers/{peer_id}')

    def test_network_peers_peer_id_ban(self, sandbox):
        peer_id = sandbox.client(2).rpc('get', '/network/self')
        sandbox.client(1).rpc('get', f'/network/peers/{peer_id}/ban')

    def test_network_peers_peer_id_banned(self, sandbox):
        peer_id = sandbox.client(2).rpc('get', '/network/self')
        sandbox.client(1).rpc('get', f'/network/peers/{peer_id}/banned')

    def test_network_peers_peer_id_unban(self, sandbox):
        peer_id = sandbox.client(2).rpc('get', '/network/self')
        sandbox.client(1).rpc('get', f'/network/peers/{peer_id}/unban')

    def test_network_peers_peer_id_untrust(self, sandbox):
        peer_id = sandbox.client(2).rpc('get', '/network/self')
        sandbox.client(1).rpc('get', f'/network/peers/{peer_id}/untrust')

    def test_network_peers_peer_id_trust(self, sandbox):
        peer_id = sandbox.client(2).rpc('get', '/network/self')
        sandbox.client(1).rpc('get', f'/network/peers/{peer_id}/trust')

    def test_network_points(self, sandbox):
        sandbox.client(1).rpc('get', f'/network/points')

    def test_network_points_point(self, sandbox):
        points = sandbox.client(1).rpc('get', f'/network/points')
        point = points[-1][0]
        sandbox.client(1).rpc('get', f'/network/points/{point}')

    def test_network_points_point_ban(self, sandbox):
        points = sandbox.client(1).rpc('get', f'/network/points')
        point = points[-1][0]
        sandbox.client(1).rpc('get', f'/network/points/{point}/ban')

    def test_network_points_point_banned(self, sandbox):
        points = sandbox.client(1).rpc('get', f'/network/points')
        point = points[-1][0]
        sandbox.client(1).rpc('get', f'/network/points/{point}/banned')

    def test_network_points_point_trust(self, sandbox):
        points = sandbox.client(1).rpc('get', f'/network/points')
        point = points[-1][0]
        sandbox.client(1).rpc('get', f'/network/points/{point}/trust')

    def test_network_points_point_unban(self, sandbox):
        points = sandbox.client(1).rpc('get', f'/network/points')
        point = points[-1][0]
        sandbox.client(1).rpc('get', f'/network/points/{point}/unban')

    def test_network_points_point_untrust(self, sandbox):
        points = sandbox.client(1).rpc('get', f'/network/points')
        point = points[-1][0]
        sandbox.client(1).rpc('get', f'/network/points/{point}/untrust')

    def test_network_stat(self, sandbox):
        sandbox.client(1).rpc('get', f'/network/stat')

    def test_network_versions(self, sandbox):
        sandbox.client(1).rpc('get', f'/network/versions')

    def test_protocols(self, sandbox):
        sandbox.client(1).rpc('get', f'/protocols')

    def test_protocols_protocol_hash(self, sandbox):
        sandbox.client(1).rpc('get', f'/protocols/{PROTOCOL_HASH}')

    def test_workers_block_validator(self, sandbox):
        sandbox.client(1).rpc('get', f'/workers/block_validator')

    def test_workers_chain_validators(self, sandbox):
        sandbox.client(1).rpc('get', f'/workers/chain_validators')

    def test_workers_chain_validator(self, sandbox):
        sandbox.client(1).rpc('get',
                              f'/workers/chain_validators/{CHAIN_ID}')

    def test_workers_chain_validator_peers_validators(self,
                                                      sandbox):
        sandbox.client(1).rpc('get',
                              f'/workers/chain_validators/{CHAIN_ID}/'
                              f'peers_validators')

    @pytest.mark.skip
    def test_workers_chain_validator_peer_validator(self, sandbox):
        peer_id = sandbox.client(2).rpc('get', '/network/self')
        sandbox.clientœ(1).rpc('get',
                               f'/workers/chain_validators/{CHAIN_ID}'
                               f'/peers_validators/{peer_id}')

    def test_workers_prevalidators(self, sandbox):
        sandbox.client(1).rpc('get', f'/workers/prevalidators')

    def test_workers_prevalidators_chain_id(self, sandbox):
        sandbox.client(1).rpc('get', f'/workers/prevalidators/{CHAIN_ID}')

    def test_chain_block(self, sandbox):
        sandbox.client(1).rpc('get',
                              f'/chains/{CHAIN_ID}/blocks/{BLOCK_ID}')

    def test_chain_block_context_constants(self, sandbox):
        sandbox.client(1).rpc('get',
                              f'/chains/{CHAIN_ID}/blocks/{BLOCK_ID}/'
                              f'context/constants')

    def test_chain_block_context_constants_errors(self, sandbox):
        sandbox.client(1).rpc('get',
                              f'/chains/{CHAIN_ID}/blocks/{BLOCK_ID}/'
                              f'context/constants/errors')

    @pytest.mark.skip
    # TODO
    def test_chain_block_context_contracts(self, sandbox):
        sandbox.client(1).rpc('get',
                              f'/chains/{CHAIN_ID}/blocks/{BLOCK_ID}/'
                              f'context/contracts')

    @pytest.mark.skip
    # TODO
    def test_chain_block_context_contract(self, sandbox):
        sandbox.client(1).rpc('get',
                              f'/chains/{CHAIN_ID}/blocks/{BLOCK_ID}/'
                              'context/contracts/{CONTRACT_ID}')

    @pytest.mark.skip
    # TODO
    def test_chain_block_context_contract_balance(self, sandbox):
        sandbox.client(1).rpc('get',
                              f'/chains/{CHAIN_ID}/blocks/{BLOCK_ID}/'
                              'context/contracts/{CONTRACT_ID}/balance')

    @pytest.mark.skip
    # TODO
    def test_chain_block_context_contract_counter(self, sandbox):
        sandbox.client(1).rpc('get',
                              f'/chains/{CHAIN_ID}/blocks/{BLOCK_ID}/'
                              'context/contracts/{CONTRACT_ID}/counter')

    @pytest.mark.skip
    # TODO
    def test_chain_block_context_contract_delegatable(self,
                                                      sandbox):
        sandbox.client(1).rpc('get',
                              f'/chains/{CHAIN_ID}/blocks/{BLOCK_ID}/'
                              'context/contracts/{CONTRACT_ID}/delegatable')

    @pytest.mark.skip
    # TODO
    def test_chain_block_context_contract_delegate(self, sandbox):
        sandbox.client(1).rpc('get',
                              f'/chains/{CHAIN_ID}/blocks/{BLOCK_ID}/'
                              'context/contracts/{CONTRACT_ID}/delegate')

    @pytest.mark.skip
    # TODO
    def test_chain_block_context_contract_manager(self, sandbox):
        sandbox.client(1).rpc('get',
                              f'/chains/{CHAIN_ID}/blocks/{BLOCK_ID}/'
                              'context/contracts/{CONTRACT_ID}/manager')

    @pytest.mark.skip
    # TODO
    def test_chain_block_context_contract_manager_key(self,
                                                      sandbox):
        sandbox.client(1).rpc('get',
                              f'/chains/{CHAIN_ID}/blocks/{BLOCK_ID}/'
                              'context/contracts/{CONTRACT_ID}/manager_key')

    @pytest.mark.skip
    # TODO
    def test_chain_block_context_contract_script(self, sandbox):
        sandbox.client(1).rpc('get',
                              f'/chains/{CHAIN_ID}/blocks/{BLOCK_ID}/'
                              'context/contracts/{CONTRACT_ID}/script')

    @pytest.mark.skip
    # TODO
    def test_chain_block_context_contract_spendable(self, sandbox):
        sandbox.client(1).rpc('get',
                              f'/chains/{CHAIN_ID}/blocks/{BLOCK_ID}/'
                              'context/contracts/{CONTRACT_ID}/spendable')

    @pytest.mark.skip
    # TODO
    def test_chain_block_context_contract_storage(self, sandbox):
        sandbox.client(1).rpc('get',
                              f'/chains/{CHAIN_ID}/blocks/{BLOCK_ID}/'
                              'context/contracts/{CONTRACT_ID}/storage')

    def test_chain_block_context_delegates(self, sandbox):
        sandbox.client(1).rpc('get',
                              f'/chains/{CHAIN_ID}/blocks/{BLOCK_ID}/'
                              f'context/delegates')

    @pytest.mark.skip
    # TODO
    def test_chain_block_context_delegate(self, sandbox):
        sandbox.client(1).rpc('get',
                              f'/chains/{CHAIN_ID}/blocks/{BLOCK_ID}/'
                              'context/delegates/{PKH}')

    @pytest.mark.skip
    # TODO
    def test_chain_block_context_delegate_balance(self, sandbox):
        sandbox.client(1).rpc('get',
                              f'/chains/{CHAIN_ID}/blocks/{BLOCK_ID}/'
                              'context/delegates/{PKH}/balance')

    @pytest.mark.skip
    # TODO
    def test_chain_block_context_delegate_deactivated(self,
                                                      sandbox):
        sandbox.client(1).rpc('get',
                              f'/chains/{CHAIN_ID}/blocks/{BLOCK_ID}/'
                              'context/delegates/{PKH}/deactivated')

    @pytest.mark.skip
    # TODO
    def test_chain_block_context_delegate_delegated_balance(self,
                                                            sandbox):
        sandbox.client(1).rpc('get',
                              f'/chains/{CHAIN_ID}/blocks/{BLOCK_ID}/'
                              'context/delegates/{PKH}/delegated_balance')

    @pytest.mark.skip
    # TODO
    def test_chain_block_context_delegate_delegated_contracts(self,
                                                              sandbox):
        sandbox.client(1).rpc('get',
                              f'/chains/{CHAIN_ID}/blocks/{BLOCK_ID}/'
                              'context/delegates/{PKH}/delegated_contracts')

    @pytest.mark.skip
    # TODO
    def test_chain_block_context_delegate_frozen_balance(self, sandbox):
        sandbox.client(1).rpc('get',
                              f'/chains/{CHAIN_ID}/blocks/{BLOCK_ID}/'
                              'context/delegates/{PKH}/frozen_balance')

    @pytest.mark.skip
    # TODO
    def test_chain_block_context_delegate_frozen_balance_by_cycle(self,
                                                                  sandbox):
        sandbox.client(1).rpc('get',
                              f'/chains/{CHAIN_ID}/blocks/{BLOCK_ID}/'
                              f'context/delegates/{PKH}/'
                              f'frozen_balance_by_cycle')

    @pytest.mark.skip
    # TODO
    def test_chain_block_context_delegate_grace_period(self, sandbox):
        sandbox.client(1).rpc('get',
                              f'/chains/{CHAIN_ID}/blocks/{BLOCK_ID}/'
                              'context/delegates/{PKH}/grace_period')

    @pytest.mark.skip
    # TODO
    def test_chain_block_context_delegate_staking_balance(self, sandbox):
        sandbox.client(1).rpc('get',
                              f'/chains/{CHAIN_ID}/blocks/{BLOCK_ID}/'
                              'context/delegates/{PKH}/staking_balance')

    def test_chain_block_context_nonces_block_level(self, sandbox):
        sandbox.client(1).rpc('get',
                              f'/chains/{CHAIN_ID}/blocks/{BLOCK_ID}/'
                              f'context/nonces/{BLOCK_LEVEL}')

    def test_chain_block_context_raw_bytes(self, sandbox):
        sandbox.client(1).rpc('get',
                              f'/chains/{CHAIN_ID}/blocks/{BLOCK_ID}/'
                              f'context/raw/bytes')

    def test_chain_block_hash(self, sandbox):
        sandbox.client(1).rpc('get',
                              f'/chains/{CHAIN_ID}/blocks/{BLOCK_ID}/hash')

    def test_chain_block_header(self, sandbox):
        sandbox.client(1).rpc('get',
                              f'/chains/{CHAIN_ID}/blocks/{BLOCK_ID}/'
                              f'header')

    def test_chain_block_header_protocol_data(self, sandbox):
        sandbox.client(1).rpc('get',
                              f'/chains/{CHAIN_ID}/blocks/{BLOCK_ID}/'
                              f'header/protocol_data')

    def test_chain_block_header_protocol_data_raw(self, sandbox):
        sandbox.client(1).rpc('get',
                              f'/chains/{CHAIN_ID}/blocks/{BLOCK_ID}/'
                              f'header/protocol_data/raw')

    def test_chain_block_header_raw(self, sandbox):
        sandbox.client(1).rpc('get',
                              f'/chains/{CHAIN_ID}/blocks/{BLOCK_ID}/'
                              f'header/raw')

    def test_chain_block_header_shell(self, sandbox):
        sandbox.client(1).rpc('get',
                              f'/chains/{CHAIN_ID}/blocks/{BLOCK_ID}/'
                              f'header/shell')

    def test_chain_block_helpers_baking_rights(self, sandbox):
        sandbox.client(1).rpc('get',
                              f'/chains/{CHAIN_ID}/blocks/{BLOCK_ID}/'
                              f'helpers/baking_rights')

    def test_chain_block_helpers_complete_prefix1(self, sandbox):
        prefix = PKH[:10]
        sandbox.client(1).rpc('get',
                              f'/chains/{CHAIN_ID}/blocks/{BLOCK_ID}/'
                              f'helpers/complete/{prefix}')

    def test_chain_block_helpers_complete_prefix2(self, sandbox):
        res = sandbox.client(1).bake('bootstrap1', BAKE_ARGS)
        prefix = res.block_hash[:5]
        sandbox.client(1).rpc('get',
                              f'/chains/{CHAIN_ID}/blocks/{BLOCK_ID}/'
                              f'helpers/complete/{prefix}')

    def test_chain_block_helpers_current_level(self, sandbox):
        sandbox.client(1).rpc('get',
                              f'/chains/{CHAIN_ID}/blocks/{BLOCK_ID}/'
                              f'helpers/current_level')

    def test_chain_block_helpers_endorsing_rights(self, sandbox):
        sandbox.client(1).rpc('get',
                              f'/chains/{CHAIN_ID}/blocks/{BLOCK_ID}/'
                              f'helpers/endorsing_rights')

    def test_chain_block_helpers_levels_in_current_cycle(self, sandbox):
        sandbox.client(1).rpc('get',
                              f'/chains/{CHAIN_ID}/blocks/{BLOCK_ID}/'
                              f'helpers/levels_in_current_cycle')

    def test_chain_block_live_blocks(self, sandbox):
        sandbox.client(1).rpc('get',
                              f'/chains/{CHAIN_ID}/blocks/{BLOCK_ID}/'
                              f'live_blocks')

    def test_chain_block_metadata(self, sandbox):
        sandbox.client(1).rpc('get',
                              f'/chains/{CHAIN_ID}/blocks/{BLOCK_ID}/'
                              f'metadata')

    def test_chain_block_operation_hashes(self, sandbox):
        sandbox.client(1).rpc('get',
                              f'/chains/{CHAIN_ID}/blocks/{BLOCK_ID}/'
                              f'operation_hashes')

    def test_add_transactions(self, sandbox):
        sandbox.client(1).transfer(1.000, 'bootstrap1', 'bootstrap2')
        sandbox.client(2).transfer(1.000, 'bootstrap3', 'bootstrap4')
        sandbox.client(1).endorse('bootstrap1')
        sandbox.client(1).bake('bootstrap1', BAKE_ARGS)
        time.sleep(3)

    def test_chain_block_operation_hashes_list_offset(self, sandbox):
        sandbox.client(1).rpc('get',
                              f'/chains/{CHAIN_ID}/blocks/{BLOCK_ID}/'
                              f'operation_hashes/{LIST_OFFSET}')

    def test_chain_block_operation_hashes_list_operation(self, sandbox):
        sandbox.client(1).rpc('get',
                              f'/chains/{CHAIN_ID}/blocks/{BLOCK_ID}/'
                              f'operation_hashes/{LIST_OFFSET}/'
                              f'{OPERATION_OFFSET}')

    def test_chain_block_operations(self, sandbox):
        sandbox.client(1).rpc('get',
                              f'/chains/{CHAIN_ID}/blocks/{BLOCK_ID}/'
                              f'operations')

    def test_chain_block_operations_list(self, sandbox):
        sandbox.client(1).rpc('get',
                              f'/chains/{CHAIN_ID}/blocks/{BLOCK_ID}/'
                              f'operations/{LIST_OFFSET}')

    def test_chain_block_operations_list_operation(self, sandbox):
        sandbox.client(1).rpc('get',
                              f'/chains/{CHAIN_ID}/blocks/{BLOCK_ID}/'
                              f'operations/{LIST_OFFSET}/'
                              f'{OPERATION_OFFSET}')

    def test_chain_block_votes_ballot_list(self, sandbox):
        sandbox.client(1).rpc('get',
                              f'/chains/{CHAIN_ID}/blocks/{BLOCK_ID}/'
                              f'votes/ballot_list')

    def test_chain_block_votes_ballots(self, sandbox):
        sandbox.client(1).rpc('get',
                              f'/chains/{CHAIN_ID}/blocks/{BLOCK_ID}/'
                              f'votes/ballots')

    def test_chain_block_votes_current_period_kind(self, sandbox):
        sandbox.client(1).rpc('get',
                              f'/chains/{CHAIN_ID}/blocks/{BLOCK_ID}/'
                              'votes/current_period_kind')

    def test_chain_block_votes_current_proposal(self, sandbox):
        sandbox.client(1).rpc('get',
                              f'/chains/{CHAIN_ID}/blocks/{BLOCK_ID}/'
                              'votes/current_proposal')

    def test_chain_block_votes_current_quorum(self, sandbox):
        sandbox.client(1).rpc('get',
                              f'/chains/{CHAIN_ID}/blocks/{BLOCK_ID}/'
                              'votes/current_quorum')

    def test_chain_block_votes_listings(self, sandbox):
        sandbox.client(1).rpc('get',
                              f'/chains/{CHAIN_ID}/blocks/{BLOCK_ID}/'
                              f'votes/listings')

    def test_chain_block_votes_proposals(self, sandbox):
        sandbox.client(1).rpc('get',
                              f'/chains/{CHAIN_ID}/blocks/{BLOCK_ID}/'
                              f'votes/proposals')

    def test_stat_gc(self, sandbox):
        assert sandbox.client(1).rpc('get', "/stats/gc")

    def test_stat_memory(self, sandbox):
        assert sandbox.client(1).rpc('get', "/stats/memory")
