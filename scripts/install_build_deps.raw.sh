#! /bin/sh

script_dir="$(cd "$(dirname "$0")" && echo "$(pwd -P)/")"
src_dir="$(dirname "$script_dir")"

. "$script_dir"/version.sh

. "$script_dir"/opam-pin.sh

opam depext $packages

opam install $packages --deps-only --with-test
