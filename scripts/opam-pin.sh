#! /bin/sh

set -e

script_dir="$(cd "$(dirname "$0")" && echo "$(pwd -P)/")"
src_dir="$(dirname "$script_dir")"

export OPAMYES=yes

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
