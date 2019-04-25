from typing import List
import os
import subprocess
from . import utils


class Endorser(subprocess.Popen):
    """Fork an endorser linked to a client"""

    def __init__(self,
                 endorser: str,
                 rpc_port: int,
                 base_dir: str,
                 params: List[str] = None,
                 log_file: str = None):
        """Create a new Popen instance for the endorser process.

        Args:
            endorser (str): path to the endorser executable file
            rpc_port (int): rpc port of the node
            base_dir (str): client directory
            params (list): additional parameters to be added to the command
            log_file (str): log file name (optional)

        Returns:
            A Popen instance
        """
        assert os.path.isfile(endorser), f'{endorser} not a file'
        assert os.path.isdir(base_dir), f'{base_dir} not a dir'

        if params is None:
            params = []
        cmd = [endorser, '-base-dir', base_dir, '-addr', '127.0.0.1', '-port',
               str(rpc_port)]
        cmd.extend(params)
        cmd_string = utils.format_command(cmd)
        print(cmd_string)
        stdout, stderr = utils.prepare_log(cmd, log_file)
        subprocess.Popen.__init__(self, cmd, stdout=stdout, stderr=stderr)
