#! /bin/sh

script_dir="$(cd "$(dirname "$0")" && echo "$(pwd -P)/")"
src_dir="$(dirname "$script_dir")"

. "$script_dir"/version.sh

if [ "$(opam --version)" != "$opam_version" ] ; then
    echo "Unexpected opam version (found: $(opam -version), expected: $opam_version)"
    exit 1
fi

opam repository set-url tezos --dont-select $opam_repository || \
    opam repository add tezos --dont-select $opam_repository

if [ ! -d "$src_dir/_opam" ] ; then
    opam switch create "$src_dir" --repositories=tezos ocaml-base-compiler.$ocaml_version
fi

if [ ! -d "$src_dir/_opam" ] ; then
    echo "Failed to create the opam switch"
    exit 1
fi

eval $(opam env)

if [ "$(ocaml -vnum)" != "$ocaml_version" ]; then
    opam install --unlock-base ocaml-base-compiler.$ocaml_version
fi

"$script_dir"/opam-unpin.sh
. "$script_dir"/opam-pin.sh

opam list --installed depext || opam install depext
opam depext $packages

opam install $packages --deps-only --with-test
