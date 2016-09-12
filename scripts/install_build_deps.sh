#! /bin/sh

OCAML_VERSION=4.03.0
if [ "$(ocaml -vnum)" != "${OCAML_VERSION}" ]; then
  echo ;
  echo "   Unexpected compiler version ($(ocaml -vnum))";
  echo "   You should use ocaml-${OCAML_VERSION}.";
  echo ;
  exit 1;
fi

cmd=$1
if [ -z "$cmd" ] ; then cmd=all ; fi

case $cmd in
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
        echo "Usage: $0 [pin|depext|install|all]"
	exit 1
esac

set -e
set -x

if [ ! -z "$pin" ] ; then
    opam pin --yes remove --no-action --dev-repo ocplib-json-typed || true
    opam pin --yes remove --no-action --dev-repo ocplib-resto || true
    opam pin --yes add --no-action --dev-repo sodium
    opam pin --yes add --no-action --dev-repo ocp-ocamlres
    opam pin --yes add --no-action --dev-repo ocplib-resto
    opam pin --yes add --no-action tezos-deps src
fi

if [ ! -z "$depext" ] ; then
    opam list --installed depext || opam install depext
    opam depext ${DEPEXTOPT} tezos-deps
fi

if [ ! -z "$install" ] ; then
    if opam list --installed tezos-deps ; then
	opam upgrade $(opam list -s --required-by tezos-deps | grep -ve '^ocaml *$')
    else
	opam install tezos-deps
    fi
fi
