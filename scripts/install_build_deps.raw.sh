#! /bin/sh

script_dir="$(cd "$(dirname "$0")" && echo "$(pwd -P)/")"
src_dir="$(dirname "$script_dir")"

. "$script_dir"/version.sh

opams=$(find "$src_dir/vendors" "$src_dir/src" -name \*.opam -print)

opam install $opams --deps-only --with-test -y --criteria="-notuptodate,-changed,-removed"
