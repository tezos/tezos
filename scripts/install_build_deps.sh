#!/bin/sh

if ! [ -f 'src/tezos-deps.opam' ]; then
    echo
    echo "   Please run from the project's root directory. Aborting."
    echo
    exit 1
fi

ocaml_version=4.03.0
if [ "$(ocaml -vnum)" != "$ocaml_version" ]; then
    echo
    echo "   Unexpected compiler version ($(ocaml -vnum))"
    echo "   You should use ocaml-$ocaml_version."
    echo
    exit 1
fi

cmd="$1"
if [ -z "$cmd" ]; then cmd=all; fi

case "$cmd" in
    pin)
        pin=yes
        ;;
    depext)
        depext=yes
        ;;
    install)
        install=yes
        ;;
    all)
        pin=yes
        depext=yes
        install=yes
        ;;
    *)
        echo "Unknown command '$cmd'."
        echo "Usage: $0 [pin|depext|install|all|]"
        exit 1
esac

set -e
set -x

if ! [ -z "$pin" ]; then
    opam pin --yes remove --no-action --dev-repo ocplib-resto || true
    opam pin --yes add --no-action --dev-repo sodium
    opam pin --yes add --no-action --dev-repo ocp-ocamlres
    opam pin --yes add --no-action --dev-repo ocplib-json-typed
    opam pin --yes add --no-action --dev-repo ocplib-resto
    ## Force opam to take account of the new `tezos-deps.opam`
    opam pin --yes remove tezos-deps
    opam pin --yes add --no-action tezos-deps src
fi

if ! [ -z "$depext" ]; then
    ## In our CI, this rule is executed as user 'root'
    ## The other rules are executed as user 'opam'.
    opam list --installed depext || opam install depext
    opam depext tezos-deps
fi

if ! [ -z "$install" ]; then
    opam install --build-test tezos-deps
fi
