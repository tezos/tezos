#! /bin/sh

## `ocaml-version` should be in sync with `README.rst` and
## `lib.protocol-compiler/tezos-protocol-compiler.opam`

ocaml_version=4.06.1
opam_version=2.0

## Please update `.gitlab-ci.yml` accordingly
opam_repository_tag=9f0956e21f4dcd2803d83072903872eba196bef8
full_opam_repository_tag=3ed20d6cfd8a35fd8b459bec3a30e149b6dc03d4
opam_repository_url=https://gitlab.com/tezos/opam-repository.git
opam_repository=$opam_repository_url\#$opam_repository_tag
