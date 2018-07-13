#! /bin/sh

set -e

script_dir="$(cd "$(dirname "$0")" && echo "$(pwd -P)/")"
src_dir="$(dirname "$script_dir")"

. "$script_dir"/opam-remove.sh

echo
echo "## Unpinning tezos packages..."

opam pin remove $packages > /dev/null 2>&1
