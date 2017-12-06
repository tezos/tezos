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

### Temporary HACK

## Should be in sync with `opam-pin.sh` and `opam-unpin.sh`
opam pin --yes add --no-action --dev-repo sodium
opam pin --yes add --no-action --dev-repo ocp-ocamlres
opam pin --yes add --no-action --dev-repo ocplib-json-typed

### End of temporary HACK

## Force opam to take account of the new `tezos-deps.opam`
opam pin --yes remove tezos-deps
opam pin --yes add --no-action tezos-deps $src_dir

opam list --installed depext || opam install depext
opam depext tezos-deps

opam install tezos-deps --deps-only
