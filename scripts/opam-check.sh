#! /bin/sh

script_dir="$(cd "$(dirname "$0")" && echo "$(pwd -P)/")"
src_dir="$(dirname "$script_dir")"

. "$script_dir"/version.sh

opams=$(find "$src_dir/vendors" "$src_dir/src" -name \*.opam -print)

echo "## Checking installed dependencies..."
echo

if ! opam install $opams --deps-only --with-test --show-actions | grep "Nothing to do." > /dev/null 2>&1 ; then
    echo
    echo 'Failure! Missing actions:'
    echo
    opam install $opams --deps-only --with-test --show-actions
    echo
    echo 'Failed! Please read the doc in `./scripts/update_opam_repo.sh` and act accordingly.'
    echo
    exit 1
fi

echo '## Running `./scripts/update_opam_repo.sh`'
echo
./scripts/update_opam_repo.sh

if [ -n "$(cat opam_repo.patch)" ] ; then

    echo "##################################################"
    cat opam_repo.patch
    echo "##################################################"

    echo 'Failed! The variables `opam_repository_tag` and `full_opam_repository_tag` are not synchronized. Please read the doc in `./scripts/update_opam_repo.sh` and act accordingly.'
    echo
    exit 1
fi

echo "Ok."
