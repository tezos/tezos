""" Utility functions to check time-dependent assertions in the tests.

Assertions are retried to avoid using arbitrary time constants in test.
"""
from typing import List  # pylint: disable=unused-import
import time
import re
from client.client import Client
from . import constants


def retry(timeout: float, attempts: float):  # pylint: disable=unused-argument
    """Retries execution of a decorated function until it returns True.

    Args:
        attempts (int): max number of attempts.
        timeout (float): time to wait between attempts.

    Returns:
        True iff an attempt was successful.
    """
    def decorator_retry(func):
        def wrapper(*args, **kwargs):
            nonlocal timeout, attempts
            while not func(*args, **kwargs):
                if attempts == 0:
                    print("*** Failed after too many retries")
                    return False
                print(f'*** Will retry after {timeout} seconds...')
                time.sleep(timeout)
                attempts -= 1
            return True
        return wrapper
    return decorator_retry


@retry(timeout=1., attempts=10)
def check_contains_operations(client: Client,
                              operation_hashes: List[str]) -> bool:
    res = client.rpc('get', '/chains/main/blocks/head/operation_hashes')
    flatten = (res[0] + res[1] + res[2] + res[3] if res is not None and
               len(res) == 4 else [])
    return all(oh in flatten for oh in operation_hashes)


@retry(timeout=1., attempts=20)
def check_protocol(client: Client, proto: str,
                   params: List[str] = None) -> bool:
    res = client.rpc('get', '/chains/main/blocks/head/metadata', params=params)
    return res['next_protocol'] == proto


@retry(timeout=1., attempts=10)
def check_level(client: Client, level) -> bool:
    return client.get_level() == level


@retry(timeout=1., attempts=10)
def check_level_greater_than(client: Client, level) -> bool:
    return client.get_level() >= level


@retry(timeout=2., attempts=20)
def check_operation_in_receipt(client: Client,
                               operation_hash: str,
                               check_previous=None) -> bool:
    extra_param = (['--check-previous', str(check_previous)] if
                   check_previous else [])
    receipt = client.get_receipt(operation_hash, extra_param)
    # TODO deal with case where operation isn't included yet
    return receipt.block_hash is not None


@retry(timeout=5, attempts=20)
def synchronize(clients: List[Client], max_diff: int = 2) -> bool:
    """Return when nodes head levels are within max_diff units"""
    levels = [client.get_level() for client in clients]
    return max(levels) - min(levels) <= max_diff


def get_block_hash(client: Client, level: int) -> str:
    """Return block hash at given level, level must be less or equal
       than current head."""
    cur = 'head'
    while True:
        block = client.rpc('get', f'/chains/main/blocks/{cur}')
        assert level <= block['header']['level']
        if block['header']['level'] == level:
            block_hash = block['hash']
            assert isinstance(block_hash, str)
            return str(block)
        cur = block['header']['predecessor']


def all_blocks(client: Client) -> List[dict]:
    """Return list of all blocks"""
    cur = 'head'
    blocks = []
    while True:
        block = client.rpc('get', f'/chains/main/blocks/{cur}')
        blocks.append(block)
        cur = block['header']['predecessor']
        if block['header']['level'] == 0:
            break
    return list(reversed(blocks))


def operations_hash_from_block(block):
    # TODO type
    _, _, _, operations = block['operations']
    res = []
    for operation in operations:
        res.append(operation['hash'])
    return res


def check_logs(logs: List[str], pattern: str) -> bool:
    for file in logs:
        with open(file, "r") as stream:
            for line in stream:
                if re.search(pattern, line):
                    print('#', stream.name)
                    print(line)
                    return False
    return True


def check_logs_counts(logs: List[str], pattern: str) -> int:
    count = 0
    for file in logs:
        with open(file, "r") as stream:
            for line in stream:
                if re.search(pattern, line):
                    print('#', stream.name)
                    print(line)
                    count += 1
    return count


def activate_alpha(client, parameters=None):
    if parameters is None:
        parameters = constants.PARAMETERS
    proto = constants.ALPHA
    client.activate_protocol_json(proto, parameters)
