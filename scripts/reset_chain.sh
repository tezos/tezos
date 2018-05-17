#! /bin/sh

script_dir="$(cd "$(dirname "$0")" && echo "$(pwd -P)/")"
cd "$script_dir"/..

branch=$1
has_git() {
    which git && [ -d .git ]
}

if has_git && ! [ -z "$(git status -s)" ] ; then
    echo "This script cannot be applied within a dirty git directory,"
    echo "you need 'stash' or 'commit' your changes before."
    exit 1
fi

set -e

cd scripts

ocaml gen_genesis.ml

git commit -a -m "Reset the chain"
