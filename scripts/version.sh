#! /bin/sh

## `ocaml-version` should be in sync with `README.rst` and
## `lib.protocol-compiler/tezos-protocol-compiler.opam`

ocaml_version=4.06.1
opam_version=2.0

## Please update `.gitlab-ci.yml` accordingly
opam_repository_tag=bb57cca4069add53f85f26794971581be8e77e3d
full_opam_repository_tag=a1ce1244a86abf897cacd4692a3ae8c369a18938
opam_repository_url=https://gitlab.com/tezos/opam-repository.git
opam_repository=$opam_repository_url\#$opam_repository_tag
