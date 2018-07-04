#! /bin/sh

script_dir="$(cd "$(dirname "$0")" && echo "$(pwd -P)/")"
src_dir="$(dirname "$script_dir")"

. "$script_dir"/version.sh

. "$script_dir"/opam-pin.sh

echo "## Checking installed dependencies..."
echo

if ! opam install $packages --deps-only --with-test --show-actions | grep "Nothing to do." > /dev/null 2>&1 ; then
    echo "Failure!"
    opam install $packages --deps-only --with-test --show-actions
    exit 1
fi

echo "Ok."
