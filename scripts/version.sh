#! /bin/sh

## `ocaml-version` should be in sync with `README.rst` and
## `lib.protocol-compiler/tezos-protocol-compiler.opam`

ocaml_version=4.07.1
opam_version=2.0

## Please update `.gitlab-ci.yml` accordingly
opam_repository_tag=99ad673bd9f07329bde9674ab93eda437a6a96af
full_opam_repository_tag=59bbbe39c3d62f55e1368be9c9190b3ca4f18f86
opam_repository_url=https://gitlab.com/tezos/opam-repository.git
opam_repository=$opam_repository_url\#$opam_repository_tag
