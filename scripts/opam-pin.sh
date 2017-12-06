#! /bin/sh

set -e

script_dir="$(cd "$(dirname "$0")" && echo "$(pwd -P)/")"
src_dir="$(dirname "$script_dir")"

opams=$(find "$src_dir" -name tezos-deps.opam -prune -or -name \*.opam -print)

export OPAMYES=yes

packages=
for opam in $opams; do
    dir=$(dirname $opam)
    file=$(basename $opam)
    package=${file%.opam}
    packages="$packages $package"
    opam pin add --no-action $package $dir
done

packages=$(opam list --short --all --sort $packages)

### Temporary HACK

## Should be in sync with `install-build-deps.sh` and `opam-unpin.sh`
opam pin add --no-action --dev-repo sodium
opam pin add --no-action --dev-repo ocp-ocamlres
opam pin add --no-action --dev-repo ocplib-json-typed

opam remove tezos-deps || true

### End of temporary HACK
