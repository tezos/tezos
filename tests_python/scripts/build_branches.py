#!/usr/bin/env python3
import subprocess
import shutil
import glob
import pathlib
import os
import re
import argparse


def print_log(log, color=True):
    color_code = '\033[34m'
    endc = '\033[0m'
    print(f'{color_code}# {log}{endc}' if color else f'# {log}')


def print_command(cmd, color=True):
    color_code = '\033[34m'
    endc = '\033[0m'
    cmd_str = " ".join(cmd)
    print(f'{color_code}# {cmd_str}{endc}' if color else f'# {cmd_str}')


# simple parser for `opam env`
TERM_REGEX = r'''(?mx)
    \s*(?:
        (?P<brackl>\()|
        (?P<brackr>\))|
        (?P<num>\-?\d+\.\d+|\-?\d+)|
        (?P<sq>"[^"]*")|
        (?P<s>[^(^)\s]+)
       )'''


def parse_sexp(sexp):
    stack = []
    out = []
    for termtypes in re.finditer(TERM_REGEX, sexp):
        term, value = [(t, v)
                       for t, v in termtypes.groupdict().items() if v][0]
        if term == 'brackl':
            stack.append(out)
            out = []
        elif term == 'brackr':
            assert stack, "Trouble with nesting of brackets"
            tmpout, out = out, stack.pop(-1)
            out.append(tmpout)
        elif term == 'num':
            val = float(value)
            if val.is_integer():
                val = int(val)
            out.append(val)
        elif term == 'sq':
            out.append(value[1:-1])
        elif term == 's':
            out.append(value)
        else:
            raise NotImplementedError(f'Error: {term, value}')
    assert not stack, "Trouble with nesting of brackets"
    return out[0]


def opam_env(tezos_build):
    process = subprocess.Popen(['opam', 'env', '--sexp', '--set-switch'],
                               stdout=subprocess.PIPE,
                               cwd=tezos_build)
    out, _err = process.communicate()
    out = out.decode('utf-8')
    env = {x[0]: x[1] for x in parse_sexp(out)}
    return env


def run(cmd, cwd, env=None):
    print_command(cmd)
    if env is None:
        subprocess.run(cmd, check=True, cwd=cwd)
    else:
        subprocess.run(cmd, check=True, cwd=cwd, env=env)


def build(branch, tezos_home, tezos_build, tezos_binaries):

    if os.listdir(tezos_build):
        error_msg = f'{tezos_build} is not empty. Should be a git directory'
        assert os.path.isdir(f"{tezos_build}/.git"), error_msg
    else:
        print_log(f'{tezos_build} is empty. Cloning {tezos_home}')
        run(['git', 'clone', tezos_home], tezos_build)

    run(['git', 'clean', '-f'], tezos_build)
    run(['git', 'reset', '--hard'], tezos_build)
    run(['git', 'checkout', branch], tezos_build)
    run(['make', 'build-deps'], tezos_build)

    new_env = opam_env(tezos_build)
    print_log(f'Extending current env with opam env: {new_env}')
    env = {**os.environ, **new_env}

    run(['make'], tezos_build, env=env)

    branch_dir = os.path.join(tezos_binaries, branch)
    print_log(f'Copying binaries to {branch_dir}')
    pathlib.Path(branch_dir).mkdir(parents=True, exist_ok=True)

    for filename in glob.glob(f'{tezos_build}/tezos-*'):
        dest = os.path.join(branch_dir, os.path.basename(filename))
        if os.path.exists(dest):
            print_log(f"{dest} already exists, don't copy")
        print_log(f'copy {filename} to {branch_dir}')
        shutil.copy(filename, branch_dir)


def prepare_binaries(
        tezos_home,
        tezos_build,
        tezos_binaries,
        branch_list):
    assert branch_list, "branch list is empty"
    assert os.path.isdir(tezos_binaries), f"{tezos_binaries} doesn't exist"
    assert os.path.isdir(tezos_build), f"{tezos_build} doesn't exist"
    assert os.path.isdir(tezos_home), f"{tezos_home} doesn't exist"
    for branch in branch_list:
        branch_dir = os.path.join(tezos_binaries, branch)
        if os.path.isdir(branch_dir) and os.listdir(branch_dir):
            print_log(f"Binaries for branch {branch} found. Skip.")
        else:
            print_log(f"Binaries for branch {branch} not found. Build.")
            build(branch, tezos_home, tezos_build, tezos_binaries)


def main():
    parser = argparse.ArgumentParser(description='build_branch.py')

    parser.add_argument('--clone', dest='clone_dir', metavar='DIR',
                        help='repository to be cloned', required=True)
    parser.add_argument('--build-dir', dest='build_dir', metavar='DIR',
                        help='repository where executables will be built',
                        required=True
                        )
    parser.add_argument('--bin-dir', dest='bin_dir', metavar='DIR',
                        help='repository where executables will be copied',
                        required=True)
    parser.add_argument('branches', metavar='BRANCH', type=str, nargs='*',
                        help='list of brnaches')
    args = parser.parse_args()
    prepare_binaries(args.clone_dir, args.build_dir, args.bin_dir,
                     args.branches)


if __name__ == "__main__":
    main()
