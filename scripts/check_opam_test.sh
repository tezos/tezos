#! /bin/sh

set -e

script_dir="$(cd "$(dirname "$0")" && echo "$(pwd -P)/")"
src_dir="$(dirname "$script_dir")"

opams=$(find "$src_dir/vendors" "$src_dir/src" -name \*.opam -print)

yml="${1:-$src_dir/.gitlab-ci.yml}"

missing=
for opam in $opams; do
    file=$(basename $opam)
    package=${file%.opam}
    if ! grep -qe "opam:..:$package:\$" "$yml"; then
        missing=yes
        echo "Missing test for package '$package'."
    fi
done

tested=$(grep -e '^opam:..:tezos-.*:$' "$yml" | cut -d: -f3)
for package in $tested; do
    found=$(find "$src_dir/src" "$src_dir/vendors" -name $package.opam | wc -l 2>&1)
    if [ $found != 1 ] ; then
        missing=yes
        echo "Test for unknown package '$package'."
    fi
done

if ! [ -z "$missing" ]; then
    echo
    echo "You should update .gitlab-ci.yml by running: ./scripts/update_opam_test.sh"
    echo
    exit 1
fi
