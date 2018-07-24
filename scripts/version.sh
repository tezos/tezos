#! /bin/sh

## `ocaml-version` should be in sync with `README.rst` and
## `lib.protocol-compiler/tezos-protocol-compiler.opam`

ocaml_version=4.06.1
opam_version=2.0.0

## Please update `.gitlab-ci.yml` accordingly
opam_repository_tag=4200cbebcae75562d79898a8e11e9f4dc237cc08
full_opam_repository_tag=2274027270cc192b88ecd7b9d4035dba66b2cffd
opam_repository_url=https://gitlab.com/tezos/opam-repository.git
opam_repository=$opam_repository_url\#$opam_repository_tag
