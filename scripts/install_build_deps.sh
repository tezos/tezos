#! /bin/sh

script_dir="$(cd "$(dirname "$0")" && echo "$(pwd -P)/")"
src_dir="$(dirname "$script_dir")"

. "$script_dir/version.sh"

if [ "$(ocaml -vnum)" != "$ocaml_version" ]; then
  echo ;
  echo "   Unexpected compiler version ($(ocaml -vnum))";
  echo "   You should use ocaml-$ocaml_version.";
  echo ;
  exit 1;
fi

set -e
set -x

opam pin --yes add --no-action --dev-repo sodium
opam pin --yes add --no-action --dev-repo ocp-ocamlres
opam pin --yes add --no-action --dev-repo ocplib-json-typed
opam pin --yes add --no-action --dev-repo ocplib-resto
opam pin --yes add --no-action --dev-repo jbuilder
## Force opam to take account of the new `tezos-deps.opam`
opam pin --yes remove tezos
opam pin --yes add --no-action tezos $src_dir

opam list --installed depext || opam install depext
opam depext tezos

opam install tezos --deps-only
