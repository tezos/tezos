#! /bin/sh

set -e

script_dir="$(cd "$(dirname "$0")" && echo "$(pwd -P)/")"
src_dir="$(dirname "$script_dir")"

. "$script_dir"/opam-remove.sh

opam pin remove $packages

### Temporary HACK

## Should be in sync with `install-build-deps.sh` and `opam-pin.sh`
opam pin remove --no-action sodium
opam pin remove --no-action ocp-ocamlres
opam pin remove --no-action ocplib-json-typed

## Unpin package we used to pin...
opam pin remove --no-action ocplib-resto

### End of temporary HACK
