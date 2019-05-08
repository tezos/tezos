from typing import List
import shutil
import datetime
import os
import subprocess
import tempfile
import json
from . import client_output


def format_command(cmd: List[str]) -> str:
    # TODO the displayed command may not be 'shell' ready, for instance
    # Michelson string parameters may requires additional quotes
    color_code = '\033[34m'
    endc = '\033[0m'
    cmd_str = " ".join(cmd)
    return f'{color_code}# {cmd_str}{endc}'


class Client:
    """Client to a Tezos node.

    Manage the persistent client state and provides methods to call
    tezos-client/tezos-admin-client commands, and return structured
    representation of the client output.

    The most generic method to call the client is `run`. It calls the client
    with an arbitrary sequence of parameters and returns the stdout of the
    command. `CalledProcessError` is raised if command fails.

    Other commands such as `run_script`, `transfer`... are wrapper over `run`,
    they set up the commands parameters and return a structured representation
    of the client output.

    TODO: - the set of methods isn't complete. To be added when needed... some
            methods return client stdout instead of structured representation
          - deal correctly with additional parameters in command wrappers
          - this works for the current tests but should be more generic
    """

    def __init__(self,
                 client_path: str,
                 admin_client_path: str,
                 host: str = '127.0.0.1',
                 base_dir: str = None,
                 rpc_port: int = 8732,
                 use_tls: int = False,
                 disable_disclaimer: bool = True):
        """
        Args:
            client (str): path to the client executable file
            admin_client (str): path to the admin-client executable file
            host (str): IP of the host
            base_dir (str): path to the client dir. If None, a temp file is
                            created.
            rpc_port (int): port of the server
            use_tls (bool): use TLS
            disable_disclaimer (bool): disable disclaimer
        Returns:
            A Client instance.
        """
        assert os.path.isfile(client_path), f"{client_path} is not a file"
        assert os.path.isfile(admin_client_path), (f"{admin_client_path} is "
                                                   f"not a file")
        assert base_dir is None or os.path.isdir(base_dir), (f'{base_dir} not '
                                                             f'a dir')

        self.host = host
        self._disable_disclaimer = disable_disclaimer
        self._is_tmp_dir = base_dir is None

        if base_dir is None:
            base_dir = tempfile.mkdtemp(prefix='tezos-client.')
            assert base_dir
        self.base_dir = base_dir

        client = [client_path] + ['-base-dir', base_dir, '-addr', host,
                                  '-port', str(rpc_port)]
        admin_client = [admin_client_path, '-base-dir', base_dir, '-addr',
                        host, '-port', str(rpc_port)]

        if use_tls:
            client.append('-S')
            admin_client.append('-S')

        self._client = client
        self._admin_client = admin_client
        self.rpc_port = rpc_port

    def run(self,
            params: List[str],
            admin: bool = False,
            check: bool = True,
            trace: bool = False) -> str:
        """Run an arbitrary command

        Args:
            params (list): list of parameters given to the tezos-client,
            admin (bool): False to call tezos-client, True to call
                          tezos-admin-client
            check (bool): raises an exception if client call fails
            trace (bool): use '-l' option to trace RPCs
        Returns:
            stdout of client command.

        The actual command will be displayed according to 'format_command'.
        Client output (stdout, stderr) will be displayed unprocessed.
        Fails with `CalledProcessError` if command fails
        """
        client = self._admin_client if admin else self._client
        trace_opt = ['-l'] if trace else []
        cmd = client + trace_opt + params

        print(format_command(cmd))

        stdout = ""
        new_env = os.environ.copy()
        if self._disable_disclaimer:
            new_env["TEZOS_CLIENT_UNSAFE_DISABLE_DISCLAIMER"] = "Y"
        # in python3.7, cleaner to use capture_output=true, text=True
        with subprocess.Popen(cmd,
                              stdout=subprocess.PIPE,
                              bufsize=1,
                              universal_newlines=True,
                              env=new_env) as process:
            for line in process.stdout:
                print(line, end='')
                stdout += line

        if check and process.returncode:
            raise subprocess.CalledProcessError(process.returncode,
                                                process.args)

        return stdout

    def rpc(self,
            verb: str,
            path: str,
            data: dict = None,
            params: List[str] = None) -> dict:
        """Run an arbitrary RPC command

        Args:
            verb (str): either `get`, `post` or `put`
            path (str): rpc path
            data (dict): json data for post
            params (list): any additional parameters to pass to the client
        Returns:
            dict representing the json output, raise exception
            if output isn't json.

        See `run` for more details.
        """
        assert verb in {'put', 'get', 'post'}
        params = [] if params is None else params
        params = params + ['rpc', verb, path]
        if data is not None:
            params = params + ['with', json.dumps(data)]
        compl_pr = self.run(params)
        return client_output.extract_rpc_answer(compl_pr)

    def typecheck(self, contract: str) -> str:
        assert os.path.isfile(contract), f'{contract} is not a file'
        return self.run(['typecheck', 'script', contract])

    def run_script(self,
                   contract: str,
                   storage: str,
                   inp: str,
                   amount: float = None) -> client_output.RunScriptResult:
        assert os.path.isfile(contract), f'{contract} is not a file'
        cmd = ['run', 'script', contract, 'on', 'storage', storage, 'and',
               'input', inp]
        if amount is not None:
            cmd += ['-z', str(amount)]
        return client_output.RunScriptResult(self.run(cmd))

    def gen_key(self, alias: str, args: List[str] = None) -> str:
        cmd = ['gen', 'keys', alias]
        if args is None:
            args = []
        cmd += args
        return self.run(cmd)

    def import_secret_key(self, name: str, secret: str) -> str:
        return self.run(['import', 'secret', 'key', name, secret])

    def activate_protocol(self,
                          protocol: str,
                          parameter_file: str,
                          fitness: str = '1',
                          key: str = 'activator',
                          timestamp: str = None
                          ) -> client_output.ActivationResult:
        assert os.path.isfile(parameter_file), f'{parameter_file} not a file'
        if timestamp is None:
            utc_now = datetime.datetime.utcnow()
            timestamp = utc_now.strftime("%Y-%m-%dT%H:%M:%SZ")
        cmd = ['-block', 'genesis', 'activate', 'protocol', protocol, 'with',
               'fitness', str(fitness), 'and', 'key', key, 'and', 'parameters',
               parameter_file, '--timestamp', timestamp]
        return client_output.ActivationResult(self.run(cmd))

    def activate_protocol_json(self,
                               protocol: str,
                               parameters: dict,
                               fitness: str = '1',
                               key: str = 'activator',
                               timestamp: str = None
                               ) -> client_output.ActivationResult:
        with tempfile.NamedTemporaryFile(mode='w+', delete=False) as params:
            param_json = json.dumps(parameters)
            params.write(param_json)
            params.close()
            return self.activate_protocol(protocol, params.name, fitness,
                                          key, timestamp)

    def show_voting_period(self) -> str:
        return self.run(['show', 'voting', 'period'])

    def ban_peer(self, port: int) -> dict:
        return self.rpc('get', f'/network/points/127.0.0.1:{port}/ban')

    def unban_peer(self, port: int) -> dict:
        return self.rpc('get', f'/network/points/127.0.0.1:{port}/unban')

    def trust_peer(self, port: int) -> dict:
        return self.rpc('get', f'/network/points/127.0.0.1:{port}/trust')

    def untrust_peer(self, port: int) -> dict:
        return self.rpc('get', f'/network/points/127.0.0.1:{port}/untrust')

    def endorse(self, account: str) -> client_output.EndorseResult:
        res = self.run(['endorse', 'for', account])
        return client_output.EndorseResult(res)

    def bake(self,
             account: str,
             args: List[str] = None) -> client_output.BakeForResult:
        cmd = ['bake', 'for', account]
        if args is None:
            args = []
        cmd += args
        return client_output.BakeForResult(self.run(cmd))

    def originate(self,
                  contract_name: str,
                  manager: str,
                  amount: float,
                  sender: str,
                  contract: str,
                  args: List[str] = None) -> client_output.OriginationResult:
        cmd = ['originate', 'contract', contract_name, 'for', manager,
               'transferring', str(amount), 'from', sender, 'running',
               contract]
        if args is None:
            args = []
        cmd += args
        return client_output.OriginationResult(self.run(cmd))

    def transfer(self,
                 amount: float,
                 account1: str,
                 account2: str,
                 args: List[str] = None) -> client_output.TransferResult:
        cmd = ['transfer', str(amount), 'from', account1, 'to', account2]
        if args is None:
            args = []
        cmd += args
        res = self.run(cmd)
        return client_output.TransferResult(res)

    def p2p_stat(self) -> str:
        return self.run(['p2p', 'stat'], admin=True)

    def get_balance(self, account) -> float:
        res = self.run(['get', 'balance', 'for', account])
        return client_output.extract_balance(res)

    def get_receipt(self,
                    operation: str,
                    args: List[str] = None) -> client_output.GetReceiptResult:
        cmd = ['get', 'receipt', 'for', operation]
        if args is None:
            args = []
        cmd += args
        return client_output.GetReceiptResult(self.run(cmd))

    def get_prevalidator(self) -> dict:
        return self.rpc('get', '/workers/prevalidators')

    def get_mempool(self) -> dict:
        return self.rpc('get', '/chains/main/mempool/pending_operations')

    def mempool_is_empty(self) -> bool:
        rpc_res = self.rpc('get', '/chains/main/mempool/pending_operations')
        return rpc_res['applied'] == [] and \
            rpc_res['refused'] == [] and \
            rpc_res['branch_refused'] == [] and \
            rpc_res['branch_delayed'] == [] and \
            rpc_res['unprocessed'] == []

    def get_head(self) -> dict:
        return self.rpc('get', '/chains/main/blocks/head')

    def get_block(self, block_hash) -> dict:
        return self.rpc('get', f'/chains/main/blocks/{block_hash}')

    def get_ballot_list(self) -> dict:
        return self.rpc('get', '/chains/main/blocks/head/votes/ballot_list')

    def get_ballots(self) -> dict:
        return self.rpc('get', '/chains/main/blocks/head/votes/ballots')

    def get_current_period_kind(self) -> dict:
        return self.rpc('get',
                        'chains/main/blocks/head/votes/current_period_kind')

    def get_current_proposal(self) -> dict:
        return self.rpc('get',
                        '/chains/main/blocks/head/votes/current_proposal')

    def get_current_quorum(self) -> dict:
        return self.rpc('get', '/chains/main/blocks/head/votes/current_quorum')

    def get_listings(self) -> dict:
        return self.rpc('get', '/chains/main/blocks/head/votes/listings')

    def get_proposals(self) -> dict:
        return self.rpc('get', '/chains/main/blocks/head/votes/proposals')

    def get_protocol(self) -> str:
        rpc_res = self.rpc('get', '/chains/main/blocks/head/metadata')
        return rpc_res['protocol']

    def get_period_position(self) -> str:
        rpc_res = self.rpc(
            'get', '/chains/main/blocks/head/helpers/current_level?offset=1')
        return rpc_res['voting_period_position']

    def get_level(self) -> int:
        rpc_res = self.rpc('get', '/chains/main/blocks/head/header/shell')
        return int(rpc_res['level'])

    def wait_for_inclusion(self,
                           operation_hash: str,
                           branch: str = None,
                           args=None) -> client_output.WaitForResult:
        cmd = ['wait', 'for', operation_hash, 'to', 'be', 'included']
        if branch is not None:
            cmd += ['--branch', branch]
        if args is None:
            args = []
        cmd += args
        return client_output.WaitForResult(self.run(cmd))

    def inject_protocol(self, proto) -> str:
        return self.run(['inject', 'protocol', proto], admin=True)

    def list_protocols(self) -> List[str]:
        cmd = ['list', 'protocols']
        return client_output.extract_protocols(self.run(cmd, admin=True))

    def submit_proposals(self,
                         account: str,
                         protos: List[str]
                         ) -> client_output.SubmitProposalsResult:
        cmd = ['submit', 'proposals', 'for', account] + protos
        return client_output.SubmitProposalsResult(self.run(cmd))

    def submit_ballot(self,
                      account: str,
                      proto: str,
                      vote: str) -> str:
        return self.run(['submit', 'ballot', 'for', account, proto, vote])

    def bootstrapped(self) -> str:
        return self.run(['bootstrapped'])

    def cleanup(self) -> None:
        """Remove base dir, only if not provided by user."""
        if self._is_tmp_dir:
            shutil.rmtree(self.base_dir)
