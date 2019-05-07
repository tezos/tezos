import pytest
from client import client_output

BAKE_ARGS = ['--max-priority', '512', '--minimal-timestamp']


# TODO finish porting test_basic.sh
@pytest.mark.incremental
class TestRawContext:

    def test_delegates(self, client):
        path = '/chains/main/blocks/head/context/raw/bytes/delegates/?depth=3'
        res = client.rpc('get', path)
        expected = {
            "ed25519": {
                "02": {"29": None},
                "a9": {"ce": None},
                "c5": {"5c": None},
                "da": {"c9": None},
                "e7": {"67": None}
            }
        }
        assert res == expected

    def test_no_service_1(self, client):
        path = '/chains/main/blocks/head/context/raw/bytes/non-existent'
        with pytest.raises(client_output.InvalidClientOutput) as exc:
            client.rpc('get', path)
        assert exc.value.client_output == 'No service found at this URL\n\n'

    def test_no_service_2(self, client):
        path = ('/chains/main/blocks/head/context/raw/bytes/'
                'non-existent?depth=-1')
        with pytest.raises(client_output.InvalidClientOutput) as exc:
            client.rpc('get', path)
        expected = 'Command failed : Extraction depth -1 is invalid\n\n'
        assert exc.value.client_output == expected

    def test_no_service_3(self, client):
        path = ('/chains/main/blocks/head/context/raw/bytes/'
                'non-existent?depth=0')
        with pytest.raises(client_output.InvalidClientOutput) as exc:
            client.rpc('get', path)
        assert exc.value.client_output == 'No service found at this URL\n\n'

    def test_bake(self, client):
        client.bake('bootstrap4', BAKE_ARGS)

    def test_gen_keys(self, client):
        keys = {'foo': None,
                'bar': 'secp256k1',
                'boo': 'ed25519',
                'king': None,
                'queen': None,
                'p256': 'p256'}
        for key, sig in keys.items():
            args = [] if sig is None else ['--sig', sig]
            client.gen_key(key, args)
