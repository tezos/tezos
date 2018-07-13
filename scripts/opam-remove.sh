#! /bin/sh

set -e

script_dir="$(cd "$(dirname "$0")" && echo "$(pwd -P)/")"
src_dir="$(dirname "$script_dir")"

opams=$(find "$src_dir" -name \*.opam)
packages=

for opam in $opams; do
    dir=$(dirname $opam)
    file=$(basename $opam)
    package=${file%.opam}
    packages="$packages $package"
done

installed=$(opam list --short --installed --pinned $packages)

opam remove $installed
