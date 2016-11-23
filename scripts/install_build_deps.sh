#! /bin/sh

OCAML_VERSION=4.03.0
if [ "$(ocaml -vnum)" != "$OCAML_VERSION" ]; then
  echo ;
  echo "   Unexpected compiler version ($(ocaml -vnum))";
  echo "   You should use ocaml-$OCAML_VERSION.";
  echo ;
  exit 1;
fi

cmd="$1"
if [ -z "$cmd" ]; then cmd=all; fi

pin=false
depext=false
install=false

case $cmd in
    pin)
	pin=true
	;;
    depext)
	depext=true
	;;
    install)
	install=true
	;;
    all)
	pin=true
	depext=true
	install=true
	;;
    *)
        echo "Unknown command '$cmd'."
        echo "Usage: $0 [pin|depext|install|all|]"
        exit 1
esac

set -e
set -x

if "$pin"; then
    opam pin --yes remove --no-action --dev-repo ocplib-resto || true
    opam pin --yes add --no-action --dev-repo sodium
    opam pin --yes add --no-action --dev-repo ocp-ocamlres
    opam pin --yes add --no-action --dev-repo ocplib-json-typed
    opam pin --yes add --no-action --dev-repo ocplib-resto
    ## Ouch, that's an awfull (temporary) hack...
    EDITOR='sed -i "s|\"ocamlfind\"|\"ocamlfind\"\ndepopts: \"camlp4\"|"' opam pin add typerex-build 1.99.17-beta --edit --no-action
    ## Force opam to take account of the new `tezos-deps.opam`
    opam pin --yes remove tezos-deps
    opam pin --yes add --no-action tezos-deps src
fi

if "$depext"; then
    opam list --installed depext || opam install depext
    opam depext $DEPEXTOPT tezos-deps
fi

if "$install"; then
    if opam list --installed tezos-deps ; then
	opam upgrade $(opam list -s --required-by tezos-deps | grep -ve '^ocaml *$')
    else
	opam install tezos-deps
    fi
fi
