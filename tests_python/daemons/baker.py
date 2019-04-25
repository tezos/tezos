from typing import List
import os
import subprocess
from . import utils


class Baker(subprocess.Popen):
    """Fork a baker process linked to a node and a client"""

    def __init__(self,
                 baker: str,
                 rpc_port: int,
                 base_dir: str,
                 node_dir: str,
                 account: str,
                 params: List[str] = None,
                 log_file: str = None):
        """Create a new Popen instance for the baker process.

        Args:
            baker (str): path to the baker executable file
            rpc_port (int): rpc port of the node
            base_dir (str): client directory
            node_dir (str): node directory
            account (str): account of the delegate
            params (list): additional parameters to be added to the command
            log_file (str): log file name (optional)
        Returns:
            A Popen instance
        """
        assert os.path.isfile(baker), f'{baker} not a file'
        assert os.path.isdir(node_dir), f'{node_dir} not a dir'
        assert os.path.isdir(base_dir), f'{base_dir} not a dir'
        if params is None:
            params = []
        cmd = [baker, '-base-dir', base_dir, '-addr', '127.0.0.1', '-port',
               str(rpc_port)]
        cmd.extend(params)
        cmd.extend(['run', 'with', 'local', 'node', node_dir, account])
        cmd_string = utils.format_command(cmd)
        print(cmd_string)
        stdout, stderr = utils.prepare_log(cmd, log_file)
        subprocess.Popen.__init__(self, cmd, stdout=stdout, stderr=stderr)
