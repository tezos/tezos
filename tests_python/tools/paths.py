import os


def tezos_home():
    # we rely on a fixed directory structure of the tezos repo
    # TEZOS_HOME/tests_python/tools/paths.py
    cur_path = os.path.dirname(os.path.realpath(__file__))
    tezos_home = os.path.dirname(os.path.dirname(cur_path)) + '/'
    assert os.path.isfile(f'{tezos_home}/tests_python/tools/paths.py')
    return tezos_home


# Use environment variable if tests_python are put outside the tezos
# TEZOS_HOME = os.environ.get('TEZOS_HOME')
TEZOS_HOME = tezos_home()
TEZOS_BINARIES = os.environ.get('TEZOS_BINARIES')
