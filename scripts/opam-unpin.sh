#! /bin/sh

set -e

script_dir="$(cd "$(dirname "$0")" && echo "$(pwd -P)/")"
src_dir="$(dirname "$script_dir")"

. "$script_dir"/opam-remove.sh

opam pin remove $packages

### Temporary HACK

## Should be in sync with `opam-pin.sh`
opam pin remove --no-action leveldb
opam pin remove --no-action ocplib-json-typed

### End of temporary HACK
