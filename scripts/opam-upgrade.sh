#! /bin/sh

set -e

script_dir="$(cd "$(dirname "$0")" && echo "$(pwd -P)/")"
src_dir="$(dirname "$script_dir")"
cd "$src_dir"

cleanup () {
    set +e
    if [ -f LOG ]; then
        echo "Failure"
        echo
        cat LOG
        echo
        exit 1
    fi
}
trap cleanup EXIT INT

silent () {
    "$@" > LOG 2>&1
    rm LOG
}

echo "Updating package description..."
silent . ./scripts/opam-pin.sh

upgradables=$(opam list --short --installed --pinned $packages)

if [ -z "$upgradables" ]; then
    echo "No previously installed package. Nothing to do."
    exit 1
fi
opam upgrade $upgradables
