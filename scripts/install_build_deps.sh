#! /bin/sh

script_dir="$(cd "$(dirname "$0")" && echo "$(pwd -P)/")"
src_dir="$(dirname "$script_dir")"

. "$script_dir"/version.sh

if [ "$(ocaml -vnum)" != "$ocaml_version" ]; then
  echo ;
  echo "   Unexpected compiler version ($(ocaml -vnum))";
  echo "   You should use ocaml-$ocaml_version.";
  echo ;
  exit 1;
fi

"$script_dir"/opam-unpin.sh
. "$script_dir"/opam-pin.sh

opam list --installed depext || opam install depext
opam depext $packages

opam install $packages --deps-only
