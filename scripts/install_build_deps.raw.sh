#! /bin/sh

script_dir="$(cd "$(dirname "$0")" && echo "$(pwd -P)/")"
src_dir="$(dirname "$script_dir")"

. "$script_dir"/version.sh

opams=$(find "$src_dir/vendors" "$src_dir/src" -name \*.opam -print)

export OPAMYES=${OPAMYES:=true}

## In an ideal world, `--with-test` should be present only when using
## `--dev`. But this would probably break the CI, so we postponed this
## change until someone have some spare time. (@pirbo, @hnrgrgr)

opam install $opams --deps-only --with-test --criteria="-notuptodate,-changed,-removed"
