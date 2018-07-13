#! /bin/sh

script_dir="$(cd "$(dirname "$0")" && echo "$(pwd -P)/")"
src_dir="$(dirname "$script_dir")"

. "$script_dir"/version.sh

. "$script_dir"/opam-pin.sh

echo "## Checking installed dependencies..."
echo

if ! opam install $packages --deps-only --with-test --show-actions | grep "Nothing to do." > /dev/null 2>&1 ; then
    echo
    echo 'Failure! Missing actions:'
    echo
    opam install $packages --deps-only --with-test --show-actions
    echo
    echo 'Running `./scripts/update_opam_repo.sh`'
    echo
    ./scripts/update_opam_repo.sh
    echo
    echo 'Result:'
    echo
    cat opam_repo.patch
    echo
    echo 'Failed! Please run: `./scripts/update_opam_repo.sh` and follow the instructions.'
    echo
    exit 1
fi

echo "Ok."
