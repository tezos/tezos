#! /bin/sh

## `ocaml-version` should be in sync with `README.rst` and
## `lib.protocol-compiler/tezos-protocol-compiler.opam`

ocaml_version=4.06.1
opam_version=2.0.0~rc3

## Please update `.gitlab-ci.yml` accordingly
opam_repository_tag=63f06ab145e22aef4cc55b5b9557756d4c868843
opam_repository_url=https://gitlab.com/tezos/opam-repository.git
opam_repository=$opam_repository_url\#$opam_repository_tag
