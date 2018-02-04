#! /bin/sh

set -e

script_dir="$(cd "$(dirname "$0")" && echo "$(pwd -P)/")"
src_dir="$(dirname "$script_dir")"

export OPAMYES=yes

### Temporary HACK

## Should be in sync with `opam-unpin.sh`
opam pin add --no-action --dev-repo ocplib-json-typed

## Unpin package we used to pin...
opam pin remove --no-action ocp-ocamlres
opam pin remove --no-action ocplib-resto
opam pin remove --no-action sodium

### End of temporary HACK

opams=$(find "$src_dir" -name \*.opam -print)

packages=
for opam in $opams; do
    dir=$(dirname $opam)
    file=$(basename $opam)
    package=${file%.opam}
    packages="$packages $package"
    opam pin add --no-action $package $dir
done

packages=$(opam list --short --all --sort $packages)

echo
echo "Pinned packages:"
echo "$packages" | sed 's/^/ /'
echo
