from typing import (Optional, Union, TextIO,  # pylint: disable=unused-import
                    List)
import subprocess


# TODO factorize with client.format_command
# TODO format on 80 chars, starting on newline
def format_command(cmd: List[str], color=True) -> str:
    color_code = '\033[34m'
    endc = '\033[0m'
    cmd_str = " ".join(cmd)
    return f'{color_code}# {cmd_str}{endc}' if color else f'# {cmd_str}'


def prepare_log(cmd: List[str], log_file: Optional[str]):
    stdout = 0  # type: Union[int, TextIO]
    cmd_str = format_command(cmd, color=False)
    if log_file:
        stdout = open(log_file, 'w')
        stdout.write(cmd_str + '\n')
        stderr = subprocess.STDOUT
    else:
        stdout = subprocess.DEVNULL
        stderr = subprocess.DEVNULL
    return stdout, stderr
